use std::borrow::Borrow;
use std::iter::FromIterator;
use std::ops::Deref;

use smallvec::SmallVec;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Assoc<T> {
    names: SmallVec<[String; 4]>,
    values: SmallVec<[T; 4]>,
}

impl<T> Assoc<T> {
    pub fn new() -> Self {
        Self {
            names: SmallVec::new(),
            values: SmallVec::new(),
        }
    }

    pub fn with<S: Into<String>>(mut self, name: S, value: T) -> Self {
        self.insert(name, value);
        self
    }

    pub fn insert<S: Into<String>>(&mut self, name: S, value: T) {
        self.names.push(name.into());
        self.values.push(value);
    }

    pub fn get<K: Eq + ?Sized>(&self, key: &K) -> Option<&T>
    where
        String: Borrow<K>,
    {
        for (name, value) in self.iter() {
            if name.borrow() == key {
                return Some(value);
            }
        }
        None
    }

    pub fn get_mut<K: Eq + ?Sized>(&mut self, key: &K) -> Option<&mut T>
    where
        String: Borrow<K>,
    {
        for (name, value) in self.iter_mut() {
            if name.borrow() == key {
                return Some(value);
            }
        }
        None
    }

    pub fn position<K: Eq + ?Sized>(&self, key: &K) -> Option<usize>
    where
        String: Borrow<K>,
    {
        for (i, name) in self.names.iter().enumerate() {
            if name.borrow() == key {
                return Some(i);
            }
        }
        None
    }

    pub fn nth(&self, n: usize) -> Option<&T> {
        self.values.get(n)
    }

    pub fn nth_mut(&mut self, n: usize) -> Option<&mut T> {
        self.values.get_mut(n)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &T)> {
        self.names.iter().zip(self.values.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&String, &mut T)> {
        self.names.iter().zip(self.values.iter_mut())
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.values.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.iter_mut()
    }
}

impl<T> Deref for Assoc<T> {
    type Target = SmallVec<[T; 4]>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl<S: Into<String>, T> FromIterator<(S, T)> for Assoc<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (S, T)>,
    {
        let (names, values): (SmallVec<[S; 4]>, SmallVec<[T; 4]>) = iter.into_iter().unzip();
        let names = names.into_iter().map(Into::into).collect();
        Assoc { names, values }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct AssocOptional<T> {
    names: SmallVec<[Option<String>; 4]>,
    values: SmallVec<[T; 4]>,
}

impl<T> AssocOptional<T> {
    pub fn new() -> Self {
        Self {
            names: SmallVec::new(),
            values: SmallVec::new(),
        }
    }

    pub fn with<S: Into<String>>(mut self, name: Option<S>, value: T) -> Self {
        self.insert(name, value);
        self
    }

    pub fn insert<S: Into<String>>(&mut self, name: Option<S>, value: T) {
        self.names.push(name.map(Into::into));
        self.values.push(value);
    }

    pub fn get<K: Eq + ?Sized>(&self, key: &K) -> Option<&T>
    where
        String: Borrow<K>,
    {
        for (name, value) in self.iter() {
            if name.map(Borrow::borrow) == Some(key) {
                return Some(value);
            }
        }
        None
    }

    pub fn get_mut<K: Eq + ?Sized>(&mut self, key: &K) -> Option<&mut T>
    where
        String: Borrow<K>,
    {
        for (name, value) in self.iter_mut() {
            if name.map(Borrow::borrow) == Some(key) {
                return Some(value);
            }
        }
        None
    }

    pub fn position<K: Eq + ?Sized>(&self, key: &K) -> Option<usize>
    where
        String: Borrow<K>,
    {
        for (i, name) in self.names.iter().enumerate() {
            if name.as_ref().map(Borrow::borrow) == Some(key) {
                return Some(i);
            }
        }
        None
    }

    pub fn nth(&self, n: usize) -> Option<&T> {
        self.values.get(n)
    }

    pub fn nth_mut(&mut self, n: usize) -> Option<&mut T> {
        self.values.get_mut(n)
    }

    pub fn iter(&self) -> impl Iterator<Item = (Option<&String>, &T)> {
        self.names
            .iter()
            .map(Option::as_ref)
            .zip(self.values.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (Option<&String>, &mut T)> {
        self.names
            .iter()
            .map(Option::as_ref)
            .zip(self.values.iter_mut())
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.values.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.iter_mut()
    }
}

impl<T> Deref for AssocOptional<T> {
    type Target = SmallVec<[T; 4]>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl<S: Into<String>, T> FromIterator<(Option<S>, T)> for AssocOptional<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = (Option<S>, T)>,
    {
        let (names, values): (SmallVec<[Option<S>; 4]>, SmallVec<[T; 4]>) =
            iter.into_iter().unzip();
        let names = names.into_iter().map(|opt| opt.map(Into::into)).collect();
        AssocOptional { names, values }
    }
}
