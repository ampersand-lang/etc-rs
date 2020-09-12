use std::any::type_name;
use std::borrow::Cow;

use failure::Fallible;
use uuid::Uuid;

use crate::assets::{AssetBundle, LazyUpdate, Resources, World};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SystemId(Uuid);

impl SystemId {
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

pub trait System: Send + Sync + 'static {
    fn run(&mut self, world: &World) -> Fallible<()>;
    fn id(&self) -> SystemId;

    fn name(&self) -> Cow<'static, str> {
        Cow::Borrowed(type_name::<Self>())
    }
}

pub trait IntoForAllSystem<Args> {
    fn system(self) -> Box<dyn System>;
}

pub struct ForAllSystem<F>
where
    F: FnMut(&World) -> Fallible<()> + Send + Sync + 'static,
{
    f: F,
    id: SystemId,
}

impl<F> System for ForAllSystem<F>
where
    F: FnMut(&World) -> Fallible<()> + Send + Sync + 'static,
{
    fn run(&mut self, world: &World) -> Fallible<()> {
        (&mut self.f)(world)
    }

    fn id(&self) -> SystemId {
        self.id
    }
}

impl<Args: AssetBundle, F> IntoForAllSystem<Args> for F
where
    F: FnMut(&mut LazyUpdate, Resources<Args>) -> Fallible<()> + Send + Sync + 'static,
{
    fn system(mut self) -> Box<dyn System> {
        Box::new(ForAllSystem {
            f: move |world| {
                let mut lazy = LazyUpdate::new();
                let result = (self)(&mut lazy, world.resources::<Args>());
                lazy.commit(&world);
                result
            },
            id: SystemId::new(),
        })
    }
}
