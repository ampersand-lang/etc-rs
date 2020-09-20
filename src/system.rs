//! Contains trait definitions and implementations for running code in pipelines.
use std::any::type_name;
use std::borrow::Cow;

use failure::Fallible;
use uuid::Uuid;

use crate::assets::{Asset, AssetBundle, LazyUpdate, Resources, Static, World};

/// A unique handle to a system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SystemId(Uuid);

impl SystemId {
    /// Creates a new random handle.
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

/// A runnable piece of code.
pub trait System: Send + Sync + 'static {
    /// Run the system.
    fn run(&mut self, world: &World) -> Fallible<Option<&'static str>>;
    /// Get the identifier of this system.
    fn id(&self) -> SystemId;

    /// Get a human-readable name of this system.
    ///
    /// Defaults to the typename of `Self`
    fn name(&self) -> Cow<'static, str> {
        Cow::Borrowed(type_name::<Self>())
    }
}

/// Turns an owned value into `ForAllSystem`.
pub trait IntoForAllSystem<Statics, Args> {
    /// Do the conversion.
    fn system(self) -> Box<dyn System>;
}

/// A system that will run with access to the storage types defined in `assets`.
pub struct ForAllSystem<F>
where
    F: FnMut(&World) -> Fallible<Option<&'static str>> + Send + Sync + 'static,
{
    f: F,
    id: SystemId,
}

impl<F> System for ForAllSystem<F>
where
    F: FnMut(&World) -> Fallible<Option<&'static str>> + Send + Sync + 'static,
{
    fn run(&mut self, world: &World) -> Fallible<Option<&'static str>> {
        (&mut self.f)(world)
    }

    fn id(&self) -> SystemId {
        self.id
    }
}

macro_rules! impl_into_for_all_static {
    ( $( $s:ident ),* ; $( $t:ident ),+ ) => {
        impl<$( $s : Asset ),* , $( $t : AssetBundle ),*, F> IntoForAllSystem<( $( $s, )* ) , ( $( $t, )* )> for F
        where
            F: FnMut(&mut LazyUpdate, $( &Static<$s> ),* , $( Resources<$t> ),*) -> Fallible<Option<&'static str>> + Send + Sync + 'static,
        {
            fn system(mut self) -> Box<dyn System> {
                Box::new(ForAllSystem {
                    f: move |world| {
                        let mut lazy = LazyUpdate::new();
                        $(
                            world.borrow::<Static<$s>>();
                        )*
                        let result = (self)(
                            &mut lazy,
                            $(
                                unsafe {
                                    world.static_global::<$s>()
                                }
                            ),* ,
                            $(
                                world.resources::<$t>()
                            ),*
                        );
                        $(
                            unsafe {
                                world.release::<Static<$s>>();
                            }
                        )*
                        lazy.commit(&world);
                        result
                    },
                    id: SystemId::new(),
                })
            }
        }
    }
}

macro_rules! impl_into_for_all {
    ( $( $t:ident ),+ ) => {
        impl<$( $t : AssetBundle ),*, F> IntoForAllSystem<(), ( $( $t, )* )> for F
        where
            F: FnMut(&mut LazyUpdate, $( Resources<$t> ),*) -> Fallible<Option<&'static str>> + Send + Sync + 'static,
        {
            fn system(mut self) -> Box<dyn System> {
                Box::new(ForAllSystem {
                    f: move |world| {
                        let mut lazy = LazyUpdate::new();
                        let result = (self)(
                            &mut lazy,
                            $(
                                world.resources::<$t>()
                            ),*
                        );
                        lazy.commit(&world);
                        result
                    },
                    id: SystemId::new(),
                })
            }
        }

        impl_into_for_all_static!(Sa ; $( $t ),*);
        impl_into_for_all_static!(Sa, Sb ; $( $t ),*);
        impl_into_for_all_static!(Sa, Sb, Sc ; $( $t ),*);
        impl_into_for_all_static!(Sa, Sb, Sc, Sd ; $( $t ),*);
    }
}

impl_into_for_all!(Ra);
impl_into_for_all!(Ra, Rb);
impl_into_for_all!(Ra, Rb, Rc);
impl_into_for_all!(Ra, Rb, Rc, Rd);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr, Rs);
impl_into_for_all!(Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr, Rs, Rt);
impl_into_for_all!(
    Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr, Rs, Rt, Ru
);
impl_into_for_all!(
    Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr, Rs, Rt, Ru, Rv
);
impl_into_for_all!(
    Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr, Rs, Rt, Ru, Rv, Rw
);
impl_into_for_all!(
    Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr, Rs, Rt, Ru, Rv, Rw, Rx
);
impl_into_for_all!(
    Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr, Rs, Rt, Ru, Rv, Rw, Rx,
    Ry
);
impl_into_for_all!(
    Ra, Rb, Rc, Rd, Re, Rf, Rg, Rh, Ri, Rj, Rk, Rl, Rm, Rn, Ro, Rp, Rq, Rr, Rs, Rt, Ru, Rv, Rw, Rx,
    Ry, Rz
);
