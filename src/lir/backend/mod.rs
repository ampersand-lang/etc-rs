use std::borrow::Cow;

use failure::Fallible;

use crate::assets::World;
use crate::system::{System, SystemId};

pub mod amd64;

#[derive(Debug, Clone)]
pub struct Output(pub Vec<u8>);

pub trait Backend: Send + Sync + 'static {
    fn name(&self) -> &'static str;
    fn build(&mut self) -> Fallible<Vec<u8>>;
    fn run(&mut self, world: &World) -> Fallible<()>;
}

pub struct BackendSystem {
    id: SystemId,
    backend: Box<dyn Backend>,
}

impl System for BackendSystem {
    fn run(&mut self, world: &World) -> Fallible<Option<&'static str>> {
        self.backend.run(world)?;
        let output = self.backend.build()?;
        world.add_static(Output(output));
        Ok(None)
    }

    fn id(&self) -> SystemId {
        self.id
    }

    fn name(&self) -> Cow<'static, str> {
        Cow::from(self.backend.name())
    }
}

pub trait IntoBackendSystem: Sized {
    fn system(self) -> Box<dyn System>;
}

impl<B: Backend + Sized> IntoBackendSystem for B {
    fn system(self) -> Box<dyn System> {
        Box::new(BackendSystem {
            id: SystemId::new(),
            backend: Box::new(self),
        })
    }
}

impl IntoBackendSystem for Box<dyn Backend> {
    fn system(self) -> Box<dyn System> {
        Box::new(BackendSystem {
            id: SystemId::new(),
            backend: self,
        })
    }
}
