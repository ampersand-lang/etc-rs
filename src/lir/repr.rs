use std::convert::TryInto;
use std::mem;
use std::str;

use crate::types::TypeInfo;
use crate::utils::IntPtr;

pub trait Repr: 'static {
    fn type_info(&self) -> TypeInfo;
    fn write_bytes(&self, out: &mut [u8]);
    fn copy_from_bytes(&mut self, bytes: &[u8]);

    fn to_bytes(&self) -> Vec<u8> {
        let t = self.type_info();
        let mut bytes = vec![0; t.size];
        self.write_bytes(&mut bytes);
        bytes
    }
}

pub trait ReprExt: Repr {
    fn from_bytes(butes: &[u8]) -> Self;
    fn static_type_info() -> TypeInfo;
}

impl<A: Repr> Repr for (A,) {
    fn type_info(&self) -> TypeInfo {
        self.0.type_info()
    }

    fn write_bytes(&self, out: &mut [u8]) {
        self.0.write_bytes(out);
    }

    fn copy_from_bytes(&mut self, bytes: &[u8]) {
        self.0.copy_from_bytes(bytes);
    }
}

impl<A: ReprExt> ReprExt for (A,) {
    fn static_type_info() -> TypeInfo {
        A::static_type_info()
    }

    fn from_bytes(bytes: &[u8]) -> Self {
        (A::from_bytes(bytes),)
    }
}

impl<A: Repr, B: Repr> Repr for (A, B) {
    fn type_info(&self) -> TypeInfo {
        let mut t0 = self.0.type_info();
        let t1 = self.1.type_info();
        t0.size = t0.size.align_up(t1.align);
        t0.size += t1.size;
        t0.align = t0.align.max(t1.align);
        t0
    }

    fn write_bytes(&self, out: &mut [u8]) {
        let mut t0 = self.0.type_info();
        let t1 = self.1.type_info();
        self.0.write_bytes(&mut out[..t0.size]);
        t0.size = t0.size.align_up(t1.align);
        self.1.write_bytes(&mut out[t0.size..]);
    }

    fn copy_from_bytes(&mut self, bytes: &[u8]) {
        let mut t0 = self.0.type_info();
        let t1 = self.1.type_info();
        self.0.copy_from_bytes(&bytes[..t0.size]);
        t0.size = t0.size.align_up(t1.align);
        self.1.copy_from_bytes(&bytes[t0.size..]);
    }
}

impl<A: ReprExt, B: ReprExt> ReprExt for (A, B) {
    fn static_type_info() -> TypeInfo {
        let mut t = A::static_type_info();
        t.size += B::static_type_info().size;
        t
    }

    fn from_bytes(bytes: &[u8]) -> Self {
        let mut t0 = A::static_type_info();
        let t1 = B::static_type_info();
        let a = &bytes[..t0.size];
        t0.size = t0.size.align_up(t1.align);
        let b = &bytes[t0.size..];
        (A::from_bytes(a), B::from_bytes(b))
    }
}

macro_rules! impl_repr_num {
    ( $t:ty ) => {
        impl Repr for $t {
            fn type_info(&self) -> TypeInfo {
                Self::static_type_info()
            }

            fn write_bytes(&self, out: &mut [u8]) {
                out.copy_from_slice(&self.to_le_bytes());
            }

            fn copy_from_bytes(&mut self, bytes: &[u8]) {
                if bytes.len() != mem::size_of::<$t>() {
                    panic!("attempt to copy from slice of invalid length");
                }
                *self = <$t>::from_le_bytes(bytes.try_into().unwrap());
            }
        }

        impl ReprExt for $t {
            fn static_type_info() -> TypeInfo {
                TypeInfo::new(mem::size_of::<$t>(), mem::align_of::<$t>())
            }

            fn from_bytes(bytes: &[u8]) -> Self {
                if bytes.len() != mem::size_of::<$t>() {
                    panic!("attempt to copy from slice of invalid length");
                }
                <$t>::from_le_bytes(bytes.try_into().unwrap())
            }
        }
    };
}

impl_repr_num!(u8);
impl_repr_num!(u16);
impl_repr_num!(u32);
impl_repr_num!(u64);
impl_repr_num!(u128);
impl_repr_num!(i8);
impl_repr_num!(i16);
impl_repr_num!(i32);
impl_repr_num!(i64);
impl_repr_num!(i128);
impl_repr_num!(f32);
impl_repr_num!(f64);

impl Repr for str {
    fn type_info(&self) -> TypeInfo {
        TypeInfo::new(self.len(), 1)
    }

    fn write_bytes(&self, out: &mut [u8]) {
        out.copy_from_slice(&self.as_bytes())
    }

    fn copy_from_bytes(&mut self, bytes: &[u8]) {
        if bytes.len() != self.len() {
            panic!("attempt to copy from slice of invalid length");
        }
        unsafe {
            self.as_bytes_mut()
                .copy_from_slice(str::from_utf8(bytes).expect("invalid utf-8").as_bytes())
        }
    }
}
