use std::fmt::{self, Debug};
use std::marker::PhantomData;
use std::any::{TypeId, Any};
use std::ptr::NonNull;
use std::convert::{AsRef, AsMut};
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use std::hash::{Hash, Hasher};
use std::cell::UnsafeCell;

use hashbrown::{HashMap, HashSet};
use parking_lot::RawRwLock;
use parking_lot::lock_api::RawRwLock as _;
use uuid::Uuid;

pub type SyncLock = Arc<RawRwLock>;

pub struct Mut;
pub struct Unmut;

pub trait Unref {
    type Type: 'static;
    type Kind: 'static;
}

pub unsafe trait UnrefMut: Unref {
}

impl<'a, T: 'static> Unref for &'a T {
    type Type = T;
    type Kind = Unmut;
}

impl<'a, T: 'static> Unref for &'a mut T {
    type Type = T;
    type Kind = Mut;
}

unsafe impl<'a, T: 'static> UnrefMut for &'a mut T {
}

pub trait Storage: Any + Send {
    fn extend(&mut self, other: Box<dyn Storage>);
}

impl dyn Storage {
    pub fn is<T: Storage>(&self) -> bool {
        self.type_id() == TypeId::of::<T>()
    }
    
    pub fn downcast_ref<T: Storage>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe {
                Some(&*(self as *const _ as *const T))
            }
        } else {
            None
        }
    }
    
    pub fn downcast_mut<T: Storage>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe {
                Some(&mut *(self as *mut _ as *mut T))
            }
        } else {
            None
        }
    }
}

pub trait Asset: Send + 'static {}

impl<T: Send + 'static> Asset for T {}

#[derive(Default, Debug)]
pub struct Assets<T: Asset> {
    assets: HashMap<Handle<T>, T>,
    changed: HashMap<Handle<T>, bool>,
    added: HashSet<Handle<T>>,
}

impl<T: Asset> Assets<T> {
    pub fn new() -> Self {
        Self {
            assets: HashMap::new(),
            changed: HashMap::new(),
            added: HashSet::new(),
        }
    }

    pub fn insert(&mut self, handle: Handle<T>, t: T) {
        self.added.insert(handle);
        self.changed.insert(handle, false);
        self.assets.insert(handle, t);
    }
    
    pub fn get(&self, lock: SyncLock, handle: Handle<T>) -> Option<Ref<'_, T>> {
        let ptr = self.assets.get(&handle).map(|t| unsafe {
            NonNull::new_unchecked(t as *const _ as *mut _)
        });
        ptr.map(|ptr| { 
            Ref {
                ptr,
                lock,
                _phantom: PhantomData,
            }
        })
    }
    
    pub fn get_mut(&mut self, lock: SyncLock, handle: Handle<T>) -> Option<RefMut<'_, T>> {
        let ptr = self.assets.get_mut(&handle).map(|t| unsafe {
            NonNull::new_unchecked(t as *const _ as *mut _)
        });
        let changed = self.changed.get_mut(&handle).map(|t| unsafe {
            NonNull::new_unchecked(t as *const _ as *mut _)
        });
        ptr.map(|ptr| { 
            RefMut {
                ptr,
                changed: changed.unwrap(),
                lock,
                _phantom: PhantomData,
            }
        })
    }
    
    pub fn static_get(&self, handle: Handle<T>) -> Option<StaticRef<'_, T>> {
        let ptr = self.assets.get(&handle).map(|t| unsafe {
            NonNull::new_unchecked(t as *const _ as *mut _)
        });
        ptr.map(|ptr| { 
            StaticRef {
                ptr,
                _phantom: PhantomData,
            }
        })
    }
    
    pub fn static_get_mut(&mut self, handle: Handle<T>) -> Option<StaticRefMut<'_, T>> {
        let ptr = self.assets.get_mut(&handle).map(|t| unsafe {
            NonNull::new_unchecked(t as *const _ as *mut _)
        });
        let changed = self.changed.get_mut(&handle).map(|t| unsafe {
            NonNull::new_unchecked(t as *const _ as *mut _)
        });
        ptr.map(|ptr| { 
            StaticRefMut {
                ptr,
                changed: changed.unwrap(),
                _phantom: PhantomData,
            }
        })
    }

    pub fn iter(&self) -> AssetsIter<'_, T> {
        AssetsIter {
            assets: self.assets.iter(),
        }
    }

    pub fn iter_mut(&mut self) -> AssetsIterMut<'_, T> {
        AssetsIterMut {
            assets: self.assets.iter_mut(),
            changed: &mut self.changed,
        }
    }
}

impl<T: Asset> Storage for Assets<T> {
    fn extend(&mut self, other: Box<dyn Storage>) {
        if other.is::<Assets<T>>() {
            let ptr = Box::into_raw(other) as *mut Assets<T>;
            let other = unsafe { Box::from_raw(ptr) };
            for (handle, t) in other.assets {
                self.insert(handle, t);
            }
        } else {
            panic!("attempt to extend assets with a storage of a different type");
        }
    }
}

pub struct AssetsIter<'a, T: Asset> {
    assets: hashbrown::hash_map::Iter<'a, Handle<T>, T>,
}

impl<'a, T: Asset> Iterator for AssetsIter<'a, T> {
    type Item = (Handle<T>, StaticRef<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        self.assets.next().map(|(key, value)| unsafe {
            let ptr = NonNull::new_unchecked(value as *const _ as *mut _);
            let static_ref = StaticRef {
                ptr,
                _phantom: PhantomData,
            };
            (*key, static_ref)
        })
    }
}

pub struct AssetsIterMut<'a, T: Asset> {
    assets: hashbrown::hash_map::IterMut<'a, Handle<T>, T>,
    changed: &'a mut HashMap<Handle<T>, bool>,
}

impl<'a, T: Asset> Iterator for AssetsIterMut<'a, T> {
    type Item = (Handle<T>, StaticRefMut<'a, T>);

    fn next(&mut self) -> Option<Self::Item> {
        self.assets.next().map(|(key, value)| unsafe {
            let changed = self.changed.get_mut(key).unwrap();
            let changed = NonNull::new_unchecked(changed as *const _ as *mut _);
            let ptr = NonNull::new_unchecked(value as *const _ as *mut _);
            let static_ref = StaticRefMut {
                ptr,
                changed,
                _phantom: PhantomData,
            };
            (*key, static_ref)
        })
    }
}

#[derive(Clone)]
pub struct Ref<'a, T: Asset> {
    ptr: NonNull<T>,
    lock: SyncLock,
    _phantom: PhantomData<&'a T>,
}

impl<'a, T: Asset> Drop for Ref<'a, T> {
    fn drop(&mut self) {
        unsafe {
            self.lock.unlock_shared();
        }
    }
}

impl<'a, T: Asset> Deref for Ref<'a, T> {
    type Target = T;
    
    fn deref(&self) -> &T {
        unsafe {
            &*self.ptr.as_ref()
        }
    }
}

impl<'a, T: Asset> AsRef<T> for Ref<'a, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

pub struct RefMut<'a, T: Asset> {
    ptr: NonNull<T>,
    changed: NonNull<bool>,
    lock: SyncLock,
    _phantom: PhantomData<&'a mut T>,
}

impl<'a, T: Asset> Drop for RefMut<'a, T> {
    fn drop(&mut self) {
        unsafe {
            self.lock.unlock_exclusive();
        }
    }
}

impl<'a, T: Asset> Deref for RefMut<'a, T> {
    type Target = T;
    
    fn deref(&self) -> &T {
        unsafe {
            self.ptr.as_ref()
        }
    }
}

impl<'a, T: Asset> DerefMut for RefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe {
            *self.changed.as_mut() = true;
            self.ptr.as_mut()
        }
    }
}

impl<'a, T: Asset> AsRef<T> for RefMut<'a, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<'a, T: Asset> AsMut<T> for RefMut<'a, T> {
    fn as_mut(&mut self) -> &mut T {
        self.deref_mut()
    }
}

#[derive(Clone, Copy)]
pub struct StaticRef<'a, T: Asset> {
    ptr: NonNull<T>,
    _phantom: PhantomData<&'a T>,
}

impl<'a, T: Asset> Deref for StaticRef<'a, T> {
    type Target = T;
    
    fn deref(&self) -> &T {
        unsafe {
            &*self.ptr.as_ref()
        }
    }
}

impl<'a, T: Asset> AsRef<T> for StaticRef<'a, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

pub struct StaticRefMut<'a, T: Asset> {
    ptr: NonNull<T>,
    changed: NonNull<bool>,
    _phantom: PhantomData<&'a mut T>,
}

impl<'a, T: Asset> Deref for StaticRefMut<'a, T> {
    type Target = T;
    
    fn deref(&self) -> &T {
        unsafe {
            self.ptr.as_ref()
        }
    }
}

impl<'a, T: Asset> DerefMut for StaticRefMut<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe {
            *self.changed.as_mut() = true;
            self.ptr.as_mut()
        }
    }
}

impl<'a, T: Asset> AsRef<T> for StaticRefMut<'a, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<'a, T: Asset> AsMut<T> for StaticRefMut<'a, T> {
    fn as_mut(&mut self) -> &mut T {
        self.deref_mut()
    }
}

#[derive(Default)]
struct WorldInner {
    resources: HashMap<TypeId, Box<dyn Storage>>,
    locks: HashMap<TypeId, SyncLock>,
}

#[derive(Clone)]
pub struct World {
    world: Arc<UnsafeCell<WorldInner>>,
    lock: SyncLock,
}

impl World {
    pub fn new() -> Self {
        Self {
            world: Default::default(),
            lock: Arc::new(RawRwLock::INIT),
        }
    }

    pub fn init_asset<T: Asset>(&self) {
        self.lock.lock_exclusive();
        
        let world = unsafe { &mut *self.world.get() };
        let id = TypeId::of::<T>();
        world.locks.insert(id, Arc::new(RawRwLock::INIT));
        world.resources.insert(id, Box::new(Assets::<T>::new()));
        
        unsafe {
            self.lock.unlock_exclusive();
        }
    }

    pub fn insert<T: Asset>(&mut self, handle: Handle<T>, t: T) {
        self.lock.lock_shared();
        
        let world = unsafe { &mut *self.world.get() };
        world.resources.get_mut(&TypeId::of::<T>()).unwrap().downcast_mut::<Assets<T>>().unwrap().insert(handle, t);
        
        unsafe {
            self.lock.unlock_shared();
        }
    }

    pub fn get<T: Asset>(&self, handle: Handle<T>) -> Option<Ref<'_, T>> {
        self.lock.lock_shared();
        
        let world = unsafe { &*self.world.get() };
        let lock = world.locks[&TypeId::of::<T>()].clone();
        lock.lock_shared();
        let result = world.resources[&TypeId::of::<T>()].downcast_ref::<Assets<T>>().unwrap().get(lock, handle);
        
        unsafe {
            self.lock.unlock_shared();
        }
        
        result
    }

    pub fn get_mut<T: Asset>(&self, handle: Handle<T>) -> Option<RefMut<'_, T>> {
        self.lock.lock_shared();
        
        let world = unsafe { &mut *self.world.get() };
        let lock = world.locks[&TypeId::of::<T>()].clone();
        lock.lock_exclusive();
        let result = world.resources.get_mut(&TypeId::of::<T>()).unwrap().downcast_mut::<Assets<T>>().unwrap().get_mut(lock, handle);
        
        unsafe {
            self.lock.unlock_shared();
        }

        result
    }

    pub unsafe fn iter<T: Asset>(&self) -> AssetsIter<'_, T> {
        self.lock.lock_shared();
        
        let world = &*self.world.get();
        let result = world.resources[&TypeId::of::<T>()].downcast_ref::<Assets<T>>().unwrap().iter();
        
        self.lock.unlock_shared();

        result
    }

    pub unsafe fn iter_mut<T: Asset>(&self) -> AssetsIterMut<'_, T> {
        self.lock.lock_shared();
        
        let world = &mut *self.world.get();
        let result = world.resources.get_mut(&TypeId::of::<T>()).unwrap().downcast_mut::<Assets<T>>().unwrap().iter_mut();
        
        self.lock.unlock_shared();

        result
    }

    pub unsafe fn static_get<T: Asset>(&self, handle: Handle<T>) -> Option<StaticRef<'_, T>> {
        self.lock.lock_shared();
        
        let world = &*self.world.get();
        let result = world.resources[&TypeId::of::<T>()].downcast_ref::<Assets<T>>().unwrap().static_get(handle);
        
        self.lock.unlock_shared();

        result
    }

    pub unsafe fn static_get_mut<T: Asset>(&self, handle: Handle<T>) -> Option<StaticRefMut<'_, T>> {
        self.lock.lock_shared();
        
        let world = &mut *self.world.get();
        let result = world.resources.get_mut(&TypeId::of::<T>()).unwrap().downcast_mut::<Assets<T>>().unwrap().static_get_mut(handle);
        
        self.lock.unlock_shared();

        result
    }

    pub fn borrow<T: Asset>(&self) { 
        self.lock.lock_shared();
        
        let world = unsafe { &*self.world.get() };
        world.locks[&TypeId::of::<T>()].lock_shared();
        
        unsafe {
            self.lock.unlock_shared();
        }

    }

    pub fn borrow_mut<T: Asset>(&self) {
        self.lock.lock_shared();
        
        let world = unsafe { &*self.world.get() };
        world.locks[&TypeId::of::<T>()].lock_exclusive();
        
        unsafe {
            self.lock.unlock_shared();
        }

    }

    pub unsafe fn release<T: Asset>(&self) {
        self.lock.lock_shared();
        
        let world = &*self.world.get();
        world.locks[&TypeId::of::<T>()].unlock_shared();
        
        self.lock.unlock_shared();
    }

    pub unsafe fn release_mut<T: Asset>(&self) {
        self.lock.lock_shared();
        
        let world = &*self.world.get();
        world.locks[&TypeId::of::<T>()].unlock_exclusive();
        
        self.lock.unlock_shared();
    }

    pub fn resources<A: AssetBundle>(&self) -> Resources<A> {
        self.lock.lock_shared();
        
        A::borrow(self);
        let res = Resources {
            world: self.clone(),
            _phantom: PhantomData,
        };
        
        unsafe {
            self.lock.unlock_shared();
        }

        res
    }
}

pub trait Res {
    fn borrow(world: &World);
    unsafe fn release(world: &World);
    unsafe fn get<T: Asset>(world: &World, handle: Handle<T>) -> Option<StaticRef<'_, T>>;
    unsafe fn get_mut<T: Asset>(world: &World, handle: Handle<T>) -> Option<StaticRefMut<'_, T>>;
}

impl<'a, T: Asset> Res for &'a T {
    fn borrow(world: &World) {
        world.borrow::<T>()
    }
    
    unsafe fn release(world: &World) {
        world.release::<T>()
    }
    
    unsafe fn get<U: Asset>(world: &World, handle: Handle<U>) -> Option<StaticRef<'_, U>> {
        if TypeId::of::<U>() == TypeId::of::<T>() {
            world.static_get(handle)
        } else {
            None
        }
    }
    
    unsafe fn get_mut<U: Asset>(_world: &World, _handle: Handle<U>) -> Option<StaticRefMut<'_, U>> {
        None
    }
}

impl<'a, T: Asset> Res for &'a mut T {
    fn borrow(world: &World) {
        world.borrow_mut::<T>()
    }
    
    unsafe fn release(world: &World) {
        world.release_mut::<T>()
    }
    
    unsafe fn get<U: Asset>(world: &World, handle: Handle<U>) -> Option<StaticRef<'_, U>> {
        if TypeId::of::<U>() == TypeId::of::<T>() {
            world.static_get(handle)
        } else {
            None
        }
    }
    
    unsafe fn get_mut<U: Asset>(world: &World, handle: Handle<U>) -> Option<StaticRefMut<'_, U>> {
        if TypeId::of::<U>() == TypeId::of::<T>() {
            world.static_get_mut(handle)
        } else {
            None
        }
    }
}

pub trait AsIter {
    unsafe fn as_iter<T: Asset>(world: &World) -> AssetsIter<'_, T>;
}

pub trait AsIterMut {
    unsafe fn as_iter_mut<T: Asset>(world: &World) -> AssetsIterMut<'_, T>;
}

pub trait AssetBundle {
    fn borrow(world: &World);
    unsafe fn release(world: &World);
    unsafe fn get<T: Asset>(world: &World, handle: Handle<T>) -> Option<StaticRef<'_, T>>;
    unsafe fn get_mut<T: Asset>(world: &World, handle: Handle<T>) -> Option<StaticRefMut<'_, T>>;
}

impl AssetBundle for () {
    fn borrow(_world: &World) {}
    unsafe fn release(_world: &World) {}
    
    unsafe fn get<T: Asset>(_world: &World, _handle: Handle<T>) -> Option<StaticRef<'_, T>> {
        None
    }
    
    unsafe fn get_mut<T: Asset>(_lock: &World, _handle: Handle<T>) -> Option<StaticRefMut<'_, T>> {
        None
    }
}

impl<A: Res> AssetBundle for A {
    fn borrow(world: &World) {
        A::borrow(world);
    }

    unsafe fn release(world: &World) {
        A::release(world);
    }
    
    unsafe fn get<T: Asset>(world: &World, handle: Handle<T>) -> Option<StaticRef<'_, T>> {
        A::get(world, handle)
    }
    
    unsafe fn get_mut<T: Asset>(world: &World, handle: Handle<T>) -> Option<StaticRefMut<'_, T>> {
        A::get_mut(world, handle)
    }
}

impl<A: Res + Unref> AsIter for A {
    unsafe fn as_iter<T: Asset>(world: &World) -> AssetsIter<'_, T> {
        if TypeId::of::<<A as Unref>::Type>() == TypeId::of::<T>() {
            world.iter::<T>()
        } else {
            panic!("attempt to iterate on a AssetBundle of a different type");
        }
    }
}

impl<A: Res + UnrefMut> AsIterMut for A {
    unsafe fn as_iter_mut<T: Asset>(world: &World) -> AssetsIterMut<'_, T> {
        if TypeId::of::<<A as Unref>::Type>() == TypeId::of::<T>() {
            world.iter_mut::<T>()
        } else {
            panic!("attempt to iterate on a AssetBundle of a different type");
        }
    }
}

macro_rules! impl_asset_bundle {
    ( $( $t:ident ),+ ) => {
        impl<$( $t : Res ),*> AssetBundle for ($( $t , )*) {
            fn borrow(world: &World) {
                $(
                    <$t as Res>::borrow(world);
                )*
            }

            unsafe fn release(world: &World) {
                $(
                    <$t as Res>::release(world);
                )*
            }
    
            unsafe fn get<T1: Asset>(world: &World, handle: Handle<T1>) -> Option<StaticRef<'_, T1>> {
                $(
                    if let Some(result) = <$t as Res>::get::<T1>(world, handle) {
                        return Some(result);
                    }
                )*
                None
            }
            
            unsafe fn get_mut<T1: Asset>(world: &World, handle: Handle<T1>) -> Option<StaticRefMut<'_, T1>> {
                $(
                    if let Some(result) = <$t as Res>::get_mut::<T1>(world, handle) {
                        return Some(result);
                    }
                )*
                None
            }
        }

        impl<$( $t: Res + Unref ),*> AsIter for ($( $t , )*) {
            unsafe fn as_iter<T1: Asset>(world: &World) -> AssetsIter<'_, T1> {
                $(
                    if TypeId::of::<<$t as Unref>::Type>() == TypeId::of::<T1>() {
                        return world.iter::<T1>()
                    }
                )*
                panic!("attempt to iterate on a AssetBundle of a different type");
            }
        }

        impl<$( $t: Res + Unref ),*> AsIterMut for ($( $t , )*) {
            unsafe fn as_iter_mut<T1: Asset>(world: &World) -> AssetsIterMut<'_, T1> {
                $(
                    if TypeId::of::<<$t as Unref>::Type>() == TypeId::of::<T1>() {
                        if TypeId::of::<<$t as Unref>::Kind>() == TypeId::of::<Mut>() {
                            return world.iter_mut::<T1>()
                        } else {
                            panic!("attempt to iterate on a AssetBundle of a different mutability");
                        }
                    }
                )*
                panic!("attempt to iterate on a AssetBundle of a different type");
            }
        }
    }
}

impl_asset_bundle!(A);
impl_asset_bundle!(A, B);
impl_asset_bundle!(A, B, C);
impl_asset_bundle!(A, B, C, D);
impl_asset_bundle!(A, B, C, D, E);
impl_asset_bundle!(A, B, C, D, E, F);
impl_asset_bundle!(A, B, C, D, E, F, G);
impl_asset_bundle!(A, B, C, D, E, F, G, H);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y);
impl_asset_bundle!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z);

pub struct Resources<A: AssetBundle> {
    world: World,
    _phantom: PhantomData<A>,
}

impl<A: AssetBundle> Drop for Resources<A> {
    fn drop(&mut self) {
        unsafe {
            A::release(&self.world);
        }
    }
}

impl<A: AssetBundle> Resources<A> {
    pub fn get<T: Asset>(&self, handle: Handle<T>) -> Option<StaticRef<'_, T>> {
        unsafe {
            A::get::<T>(&self.world, handle)
        }
    }

    pub fn get_mut<T: Asset>(&mut self, handle: Handle<T>) -> Option<StaticRefMut<'_, T>> {
        unsafe {
            A::get_mut::<T>(&mut self.world, handle)
        }
    }
}
    
impl<A: AssetBundle + AsIter> Resources<A> {
    pub fn iter<T: Asset>(&self) -> AssetsIter<'_, T> {
        unsafe {
            A::as_iter::<T>(&self.world)
        }
    }
}

impl<A: AssetBundle + AsIterMut> Resources<A> {    
    pub fn iter_mut<T: Asset>(&self) -> AssetsIterMut<'_, T> {
        unsafe {
            A::as_iter_mut::<T>(&self.world)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedHandle(Uuid);

pub struct Handle<T>(Uuid, PhantomData<T>);

impl<T> Handle<T> {
    pub fn new() -> Self {
        Self(Uuid::new_v4(), PhantomData)
    }

    pub fn into_untyped(self) -> UntypedHandle {
        UntypedHandle(self.0)
    }

    pub unsafe fn from_untyped(untyped: UntypedHandle) -> Self {
        Self(untyped.0, PhantomData)
    }
}

impl<T> Debug for Handle<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Handle")
            .field(&self.0)
            .finish()
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<T> Copy for Handle<T> { }

impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Handle<T> { }

impl<T> Hash for Handle<T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.0.hash(state);
    }
}

#[derive(Default)]
pub struct LazyUpdate {
    resources: HashMap<TypeId, Box<dyn Storage>>,
}

impl LazyUpdate {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert<T: Asset>(&mut self, handle: Handle<T>, t: T) {
        let id = TypeId::of::<T>();
        self.resources.entry(id).or_insert_with(|| {
            let mut assets = Assets::new();
            assets.insert(handle, t);
            Box::new(assets)
        });
    }

    pub fn commit(self, w: &World) {
        w.lock.lock_exclusive();
        
        let world = unsafe { &mut *w.world.get() };
        
        for (id, res) in self.resources {
            if let Some(storage) = world.resources.get_mut(&id) {
                storage.extend(res);
            } else {
                world.resources.insert(id, res);
                world.locks.insert(id, Arc::new(RawRwLock::INIT));
            }
        }
        
        unsafe {
            w.lock.unlock_exclusive();
        }
    }
}
