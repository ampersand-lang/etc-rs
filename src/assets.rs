//! Storage types for runtime-generic mapped and static values.
use std::any::{Any, TypeId};
use std::cell::UnsafeCell;
use std::convert::{AsMut, AsRef, TryInto};
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::sync::Arc;

use hashbrown::{HashMap, HashSet};
use parking_lot::lock_api::RawRwLock as _;
use parking_lot::RawRwLock;
use uuid::Uuid;

use crate::lir::repr::{Repr, ReprExt};
use crate::types::TypeInfo;

/// The default namespace for SHA-1 hashed `Handle`s (Uuids)
const NAMESPACE_ETC: Uuid = Uuid::from_bytes([
    0x6b, 0xa7, 0xb8, 0x1f, 0x9d, 0xad, 0x11, 0xd1, 0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
]);

/// A `Sync`hronized version of `parking_lot::RawRwLock`.
pub type SyncLock = Arc<RawRwLock>;

/// A marker struct for type-level recognition of reference mutability.
///
/// Mutable version.
pub struct Mut;

/// A marker struct for type-level recognition of reference mutability.
///
/// Immutable version.
pub struct Unmut;

/// A trait for type-level derefences.
pub trait Unref {
    type Type: 'static;
    type Kind: 'static;
}

/// A trait for type-level mutable dereferences.
pub unsafe trait UnrefMut: Unref {}

impl<'a, T: 'static> Unref for &'a T {
    type Type = T;
    type Kind = Unmut;
}

impl<'a, T: 'static> Unref for &'a mut T {
    type Type = T;
    type Kind = Mut;
}

unsafe impl<'a, T: 'static> UnrefMut for &'a mut T {}

/// A specialized `Any`-like trait with a `Send` requirement.
///
/// Introduces `Extend::extend`-like functionality to arbitrary storages.
pub trait Storage: Any + Send {
    /// Extend this storage with an owned trait object of the same type.
    fn extend(&mut self, other: Box<dyn Storage>);
}

impl dyn Storage {
    /// Dynamically checks if a `Storage` is of type `T`.
    pub fn is<T: Storage>(&self) -> bool {
        self.type_id() == TypeId::of::<T>()
    }

    /// Dynamically casts a `Storage` reference to any reference `&T` if and only if the underlying `Self == T`.
    pub fn downcast_ref<T: Storage>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe { Some(&*(self as *const _ as *const T)) }
        } else {
            None
        }
    }

    /// Dynamically casts a mutable `Storage` reference to any mutable reference `T` if and only if the underlying `Self == T`.
    pub fn downcast_mut<T: Storage>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe { Some(&mut *(self as *mut _ as *mut T)) }
        } else {
            None
        }
    }
}

/// A helper trait for things which can be stored in `Assets`.
pub trait Asset: Send + 'static {}

impl<T: Send + 'static> Asset for T {}

/// A map from a `Handle<T>` to `T` with flags for notifying on change or insertion.
#[derive(Default, Debug)]
pub struct Assets<T: Asset> {
    /// The main storage
    assets: HashMap<Handle<T>, T>,
    /// Flags notifying on every mutable dereference of an asset obtained from this storage unit
    changed: HashMap<Handle<T>, bool>,
    /// Flags notifying on every insertion of an asset into this storage unit
    added: HashSet<Handle<T>>,
}

impl<T: Asset> Assets<T> {
    /// Creates a new, empty storage unit.
    pub fn new() -> Self {
        Self {
            assets: HashMap::new(),
            changed: HashMap::new(),
            added: HashSet::new(),
        }
    }

    /// Inserts an existing value with a pre-set key into this storage unit.
    ///
    /// # Notes
    /// - Sets the `added` flag to true.
    pub fn insert(&mut self, handle: Handle<T>, t: T) {
        self.added.insert(handle);
        self.changed.insert(handle, false);
        self.assets.insert(handle, t);
    }

    /// Obtains an immutable reference to an asset with the key `handle`.
    ///
    /// # Notes
    /// - Assumes ownership over an already locked `SyncLock`.
    pub fn get(&self, lock: SyncLock, handle: Handle<T>) -> Option<Ref<'_, T>> {
        let ptr = self
            .assets
            .get(&handle)
            .map(|t| unsafe { NonNull::new_unchecked(t as *const _ as *mut _) });
        ptr.map(|ptr| Ref {
            ptr,
            lock,
            _phantom: PhantomData,
        })
    }

    /// Obtains a mutable reference to an asset with the key `handle`.
    ///
    /// # Notes
    /// -Assumes ownership over an already locked `SyncLock`.
    pub fn get_mut(&mut self, lock: SyncLock, handle: Handle<T>) -> Option<RefMut<'_, T>> {
        let ptr = self
            .assets
            .get_mut(&handle)
            .map(|t| unsafe { NonNull::new_unchecked(t as *const _ as *mut _) });
        let changed = self
            .changed
            .get_mut(&handle)
            .map(|t| unsafe { NonNull::new_unchecked(t as *const _ as *mut _) });
        ptr.map(|ptr| RefMut {
            ptr,
            changed: changed.unwrap(),
            lock,
            _phantom: PhantomData,
        })
    }

    /// Removes a value with the key `handle` from collection.
    pub fn remove(&mut self, handle: Handle<T>) -> Option<T> {
        self.changed.remove(&handle);
        self.assets.remove(&handle)
    }

    /// Obtains a mutable reference to an asset with the key `handle`.
    ///
    /// # Notes
    /// - Assumes that the resource is already locked.
    /// - Assumes the lock will be released manually.
    pub fn static_get(&self, handle: Handle<T>) -> Option<StaticRef<'_, T>> {
        let ptr = self
            .assets
            .get(&handle)
            .map(|t| unsafe { NonNull::new_unchecked(t as *const _ as *mut _) });
        ptr.map(|ptr| StaticRef {
            ptr,
            _phantom: PhantomData,
        })
    }

    /// Obtains a mutable reference to an asset with the key `handle`.
    ///
    /// # Notes
    /// - Assumes that the resource is already uniquely locked.
    /// - Assumes the lock will be released manually.
    pub fn static_get_mut(&mut self, handle: Handle<T>) -> Option<StaticRefMut<'_, T>> {
        let ptr = self
            .assets
            .get_mut(&handle)
            .map(|t| unsafe { NonNull::new_unchecked(t as *const _ as *mut _) });
        let changed = self
            .changed
            .get_mut(&handle)
            .map(|t| unsafe { NonNull::new_unchecked(t as *const _ as *mut _) });
        ptr.map(|ptr| StaticRefMut {
            ptr,
            changed: changed.unwrap(),
            _phantom: PhantomData,
        })
    }

    /// Returns an immutable iterator over elements.
    pub fn iter(&self) -> AssetsIter<'_, T> {
        AssetsIter {
            assets: self.assets.iter(),
        }
    }

    /// Returns a mutable iterator over elements.
    pub fn iter_mut(&mut self) -> AssetsIterMut<'_, T> {
        AssetsIterMut {
            assets: self.assets.iter_mut(),
            changed: &mut self.changed,
        }
    }
}

impl<T: Asset> Storage for Assets<T> {
    fn extend(&mut self, mut other: Box<dyn Storage>) {
        if other.is::<Assets<T>>() {
            let other = other.downcast_mut::<Assets<T>>().unwrap();
            for (handle, t) in other.assets.drain() {
                self.insert(handle, t);
            }
        } else {
            panic!("attempt to extend assets with a storage of a different type");
        }
    }
}

/// An immutable iterator over elements of an `Assets`.
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

/// A mutable iterator over elements of an `Assets`.
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

/// An automatically unlocking smart pointer to an `Asset`.
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
        unsafe { &*self.ptr.as_ref() }
    }
}

impl<'a, T: Asset> AsRef<T> for Ref<'a, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

/// An automatically unlocking mutable smart pointer to an `Asset`.
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
        unsafe { self.ptr.as_ref() }
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

/// A reference to an `Asset`.
#[derive(Clone, Copy)]
pub struct StaticRef<'a, T: Asset> {
    ptr: NonNull<T>,
    _phantom: PhantomData<&'a T>,
}

impl<'a, T: Asset> Deref for StaticRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.ptr.as_ref() }
    }
}

impl<'a, T: Asset> AsRef<T> for StaticRef<'a, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

/// A mutable reference to an `Asset`.
pub struct StaticRefMut<'a, T: Asset> {
    ptr: NonNull<T>,
    changed: NonNull<bool>,
    _phantom: PhantomData<&'a mut T>,
}

impl<'a, T: Asset> Deref for StaticRefMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
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

/// A collection of multiple storages, mapped with their `TypeId`s, `Static` resources and `RwLock`s to storages and static resources.
#[derive(Default)]
struct WorldInner {
    resources: HashMap<TypeId, Box<dyn Storage>>,
    globals: HashMap<TypeId, Box<dyn Any + Send>>,
    changed: HashMap<TypeId, bool>,
    locks: HashMap<TypeId, SyncLock>,
}

/// A thread-safe, cloneable collection of multiple storages, mapped with their `TypeId`s, `Static` resources and `RwLock`s to storages and static resources.
#[derive(Clone)]
pub struct World {
    /// A thread-safe, cloneable smart pointer to the world.
    world: Arc<UnsafeCell<WorldInner>>,
    /// The lock for reading from or writing to the world.
    lock: SyncLock,
}

impl World {
    /// Creates a new empty world.
    pub fn new() -> Self {
        Self {
            world: Default::default(),
            lock: Arc::new(RawRwLock::INIT),
        }
    }

    /// Initializes an empty storage.
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

    /// Initializes a static value with its default value.
    pub fn init_static<T: Asset + Default>(&self) {
        self.lock.lock_exclusive();

        let world = unsafe { &mut *self.world.get() };
        let id = TypeId::of::<Static<T>>();
        world.locks.insert(id, Arc::new(RawRwLock::INIT));
        world.globals.insert(id, Box::new(Static::<T>::default()));
        world.changed.insert(id, false);

        unsafe {
            self.lock.unlock_exclusive();
        }
    }

    /// Adds a pre-existing static value.
    pub fn add_static<T: Asset>(&self, value: T) {
        self.lock.lock_exclusive();

        let world = unsafe { &mut *self.world.get() };
        let id = TypeId::of::<Static<T>>();
        world.locks.insert(id, Arc::new(RawRwLock::INIT));
        world.globals.insert(id, Box::new(Static { asset: value }));
        world.changed.insert(id, false);

        unsafe {
            self.lock.unlock_exclusive();
        }
    }

    /// Inserts a value with a handle.
    pub fn insert<T: Asset>(&self, handle: Handle<T>, t: T) {
        self.lock.lock_shared();

        let world = unsafe { &mut *self.world.get() };
        let lock = world.locks[&TypeId::of::<T>()].clone();
        lock.lock_exclusive();
        world
            .resources
            .get_mut(&TypeId::of::<T>())
            .unwrap()
            .downcast_mut::<Assets<T>>()
            .unwrap()
            .insert(handle, t);

        unsafe {
            lock.unlock_exclusive();
        }

        unsafe {
            self.lock.unlock_shared();
        }
    }

    pub fn insert_unsafe<T: Asset>(&self, handle: Handle<T>, t: T) {
        self.lock.lock_shared();

        let world = unsafe { &mut *self.world.get() };
        world
            .resources
            .get_mut(&TypeId::of::<T>())
            .unwrap()
            .downcast_mut::<Assets<T>>()
            .unwrap()
            .insert(handle, t);

        unsafe {
            self.lock.unlock_shared();
        }
    }

    pub fn global<T: Asset>(&self) -> Ref<'_, Static<T>> {
        self.lock.lock_shared();

        let world = unsafe { &*self.world.get() };
        let lock = world.locks[&TypeId::of::<Static<T>>()].clone();
        lock.lock_shared();
        let result = world.globals[&TypeId::of::<Static<T>>()]
            .downcast_ref::<Static<T>>()
            .unwrap();
        let result = Ref {
            ptr: unsafe { NonNull::new_unchecked(result as *const _ as *mut _) },
            lock,
            _phantom: PhantomData,
        };

        unsafe {
            self.lock.unlock_shared();
        }

        result
    }

    pub fn global_mut<T: Asset>(&self) -> RefMut<'_, Static<T>> {
        self.lock.lock_shared();

        let world = unsafe { &mut *self.world.get() };
        let lock = world.locks[&TypeId::of::<Static<T>>()].clone();
        lock.lock_exclusive();
        let result = world
            .globals
            .get_mut(&TypeId::of::<Static<T>>())
            .unwrap()
            .downcast_mut::<Static<T>>()
            .unwrap();
        let changed = world.changed.get_mut(&TypeId::of::<Static<T>>()).unwrap();
        let result = unsafe {
            RefMut {
                ptr: NonNull::new_unchecked(result as _),
                changed: NonNull::new_unchecked(changed as _),
                lock,
                _phantom: PhantomData,
            }
        };

        unsafe {
            self.lock.unlock_shared();
        }

        result
    }

    pub unsafe fn static_global<T: Asset>(&self) -> &Static<T> {
        self.lock.lock_shared();

        let world = &*self.world.get();
        let result = world.globals[&TypeId::of::<Static<T>>()]
            .downcast_ref::<Static<T>>()
            .unwrap();

        self.lock.unlock_shared();

        result
    }

    pub unsafe fn static_global_mut<T: Asset>(&self) -> &mut Static<T> {
        self.lock.lock_shared();

        let world = &mut *self.world.get();
        let result = world
            .globals
            .get_mut(&TypeId::of::<Static<T>>())
            .unwrap()
            .downcast_mut::<Static<T>>()
            .unwrap();
        *world.changed.get_mut(&TypeId::of::<Static<T>>()).unwrap() = true;

        self.lock.unlock_shared();

        result
    }

    pub fn get<T: Asset>(&self, handle: Handle<T>) -> Option<Ref<'_, T>> {
        self.lock.lock_shared();

        let world = unsafe { &*self.world.get() };
        let lock = world.locks[&TypeId::of::<T>()].clone();
        lock.lock_shared();
        let result = world.resources[&TypeId::of::<T>()]
            .downcast_ref::<Assets<T>>()
            .unwrap()
            .get(lock, handle);

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
        let result = world
            .resources
            .get_mut(&TypeId::of::<T>())
            .unwrap()
            .downcast_mut::<Assets<T>>()
            .unwrap()
            .get_mut(lock, handle);

        unsafe {
            self.lock.unlock_shared();
        }

        result
    }

    pub unsafe fn iter<T: Asset>(&self) -> AssetsIter<'_, T> {
        self.lock.lock_shared();

        let world = &*self.world.get();
        let result = world.resources[&TypeId::of::<T>()]
            .downcast_ref::<Assets<T>>()
            .unwrap()
            .iter();

        self.lock.unlock_shared();

        result
    }

    pub unsafe fn iter_mut<T: Asset>(&self) -> AssetsIterMut<'_, T> {
        self.lock.lock_shared();

        let world = &mut *self.world.get();
        let result = world
            .resources
            .get_mut(&TypeId::of::<T>())
            .unwrap()
            .downcast_mut::<Assets<T>>()
            .unwrap()
            .iter_mut();

        self.lock.unlock_shared();

        result
    }

    pub unsafe fn remove<T: Asset>(&self, handle: Handle<T>) -> Option<T> {
        self.lock.lock_shared();

        let world = &mut *self.world.get();
        let result = world
            .resources
            .get_mut(&TypeId::of::<T>())
            .unwrap()
            .downcast_mut::<Assets<T>>()
            .unwrap()
            .remove(handle);

        self.lock.unlock_shared();

        result
    }

    pub unsafe fn static_get<T: Asset>(&self, handle: Handle<T>) -> Option<StaticRef<'_, T>> {
        self.lock.lock_shared();

        let world = &*self.world.get();
        let result = world.resources[&TypeId::of::<T>()]
            .downcast_ref::<Assets<T>>()
            .unwrap()
            .static_get(handle);

        self.lock.unlock_shared();

        result
    }

    pub unsafe fn static_get_mut<T: Asset>(
        &self,
        handle: Handle<T>,
    ) -> Option<StaticRefMut<'_, T>> {
        self.lock.lock_shared();

        let world = &mut *self.world.get();
        let result = world
            .resources
            .get_mut(&TypeId::of::<T>())
            .unwrap()
            .downcast_mut::<Assets<T>>()
            .unwrap()
            .static_get_mut(handle);

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

unsafe impl Sync for World {}
unsafe impl Send for World {}

pub trait Res {
    fn borrow(world: &World);
    unsafe fn release(world: &World);
    unsafe fn remove<T: Asset>(world: &World, handle: Handle<T>) -> Option<T>;
    unsafe fn insert<T: Asset>(world: &World, handle: Handle<T>, t: T);
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

    unsafe fn remove<U: Asset>(_world: &World, _handle: Handle<U>) -> Option<U> {
        None
    }

    unsafe fn insert<U: Asset>(_world: &World, _handle: Handle<U>, _u: U) {}

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

    unsafe fn remove<U: Asset>(world: &World, handle: Handle<U>) -> Option<U> {
        if TypeId::of::<U>() == TypeId::of::<T>() {
            world.remove(handle)
        } else {
            None
        }
    }

    unsafe fn insert<U: Asset>(world: &World, handle: Handle<U>, u: U) {
        if TypeId::of::<U>() == TypeId::of::<T>() {
            world.insert_unsafe(handle, u);
        }
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
    unsafe fn remove<T: Asset>(world: &World, handle: Handle<T>) -> Option<T>;
    unsafe fn insert<T: Asset>(world: &World, handle: Handle<T>, t: T);
    unsafe fn get<T: Asset>(world: &World, handle: Handle<T>) -> Option<StaticRef<'_, T>>;
    unsafe fn get_mut<T: Asset>(world: &World, handle: Handle<T>) -> Option<StaticRefMut<'_, T>>;
}

impl AssetBundle for () {
    fn borrow(_world: &World) {}
    unsafe fn release(_world: &World) {}

    unsafe fn remove<T: Asset>(_world: &World, _handle: Handle<T>) -> Option<T> {
        None
    }

    unsafe fn insert<T: Asset>(_world: &World, _handle: Handle<T>, _t: T) {}

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

    unsafe fn remove<T: Asset>(world: &World, handle: Handle<T>) -> Option<T> {
        A::remove(world, handle)
    }

    unsafe fn insert<T: Asset>(world: &World, handle: Handle<T>, t: T) {
        A::insert(world, handle, t)
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
        impl<$( $t : Res + Unref ),*> AssetBundle for ($( $t , )*) {
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

            unsafe fn remove<T1: Asset>(world: &World, handle: Handle<T1>) -> Option<T1> {
                $(
                    if let Some(result) = <$t as Res>::remove::<T1>(world, handle) {
                        return Some(result);
                    }
                )*
                None
            }

            unsafe fn insert<T1: Asset>(world: &World, handle: Handle<T1>, t: T1) {
                $(
                    if TypeId::of::<<$t as Unref>::Type>() == TypeId::of::<T1>() {
                        <$t as Res>::insert::<T1>(world, handle, t);
                        return;
                    }
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
    pub fn remove<T: Asset>(&mut self, handle: Handle<T>) -> Option<T> {
        unsafe { A::remove::<T>(&mut self.world, handle) }
    }

    pub fn insert<T: Asset>(&mut self, handle: Handle<T>, t: T) {
        unsafe { A::insert::<T>(&mut self.world, handle, t) }
    }

    pub fn get<T: Asset>(&self, handle: Handle<T>) -> Option<StaticRef<'_, T>> {
        unsafe { A::get::<T>(&self.world, handle) }
    }

    pub fn get_mut<T: Asset>(&mut self, handle: Handle<T>) -> Option<StaticRefMut<'_, T>> {
        unsafe { A::get_mut::<T>(&mut self.world, handle) }
    }
}

impl<A: AssetBundle + AsIter> Resources<A> {
    pub fn iter<T: Asset>(&self) -> AssetsIter<'_, T> {
        unsafe { A::as_iter::<T>(&self.world) }
    }
}

impl<A: AssetBundle + AsIterMut> Resources<A> {
    pub fn iter_mut<T: Asset>(&self) -> AssetsIterMut<'_, T> {
        unsafe { A::as_iter_mut::<T>(&self.world) }
    }
}

#[derive(Default, Debug)]
pub struct Static<T: Asset> {
    asset: T,
}

impl<T: Asset> Deref for Static<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.asset
    }
}

impl<T: Asset> DerefMut for Static<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.asset
    }
}

impl<T: Asset> AsRef<T> for Static<T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<T: Asset> AsMut<T> for Static<T> {
    fn as_mut(&mut self) -> &mut T {
        self.deref_mut()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UntypedHandle(Uuid);

pub struct Handle<T>(Uuid, PhantomData<T>);

impl<T> Handle<T> {
    pub fn nil() -> Self {
        Self(Uuid::nil(), PhantomData)
    }

    pub fn new() -> Self {
        Self(Uuid::new_v4(), PhantomData)
    }

    pub fn from_name<U, V: AsRef<[u8]> + ?Sized>(u: Handle<U>, v: &V) -> Self {
        Self(Uuid::new_v5(&u.0, v.as_ref()), PhantomData)
    }

    pub fn from_hash<U: AsRef<[u8]> + ?Sized>(u: &U) -> Self {
        Self(Uuid::new_v5(&NAMESPACE_ETC, u.as_ref()), PhantomData)
    }

    pub fn from_u128(v: u128) -> Self {
        Self(Uuid::from_u128(v), PhantomData)
    }

    pub fn as_u128(&self) -> u128 {
        self.0.as_u128()
    }

    pub fn into_untyped(self) -> UntypedHandle {
        UntypedHandle(self.0)
    }

    pub unsafe fn from_untyped(untyped: UntypedHandle) -> Self {
        Self(untyped.0, PhantomData)
    }

    pub fn display(&self) -> impl Display + '_ {
        self.0.to_hyphenated_ref()
    }
}

impl<T> Debug for Handle<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Handle").field(&self.0).finish()
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<T> Copy for Handle<T> {}

impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Handle<T> {}

impl<T> Hash for Handle<T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.0.hash(state);
    }
}

impl<T: 'static> Repr for Handle<T> {
    fn type_info(&self) -> TypeInfo {
        Self::static_type_info()
    }

    fn write_bytes(&self, out: &mut [u8]) {
        out.copy_from_slice(&self.0.as_u128().to_le_bytes());
    }

    fn copy_from_bytes(&mut self, bytes: &[u8]) {
        if bytes.len() != mem::size_of::<u128>() {
            panic!("attempt to copy from slice of invalid length");
        }
        self.0 = Uuid::from_u128(u128::from_le_bytes(bytes.try_into().unwrap()));
    }
}

impl<T: 'static> ReprExt for Handle<T> {
    fn static_type_info() -> TypeInfo {
        TypeInfo::new(mem::size_of::<u128>(), mem::align_of::<u128>())
    }

    fn from_bytes(bytes: &[u8]) -> Self {
        if bytes.len() != mem::size_of::<u128>() {
            panic!("attempt to copy from slice of invalid length");
        }
        Handle(
            Uuid::from_u128(u128::from_le_bytes(bytes.try_into().unwrap())),
            PhantomData,
        )
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
        let assets = self.resources.entry(id).or_insert_with(|| {
            let assets = Assets::<T>::new();
            Box::new(assets)
        });
        assets.downcast_mut::<Assets<T>>().unwrap().insert(handle, t);
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
