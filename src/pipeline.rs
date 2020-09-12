use std::sync::mpsc;

use failure::Fallible;
use rayon::prelude::*;

use crate::error::MultiError;
use crate::assets::World;
use crate::system::System;

pub struct Stage {
    name: &'static str,
    systems: Vec<Box<dyn System>>,
}

pub struct Pipeline {
    stages: Vec<Stage>,
}

impl Pipeline {
    pub fn new() -> Self {
        Self {
            stages: Vec::new(),
        }
    }

    pub fn run(&mut self, world: &World) -> Fallible<()> {
        for stage in &mut self.stages {
            let (tx, rx) = mpsc::channel();
            stage.systems.par_iter_mut()
                .for_each_with(tx, |tx, sys| {
                    let world = world.clone();
                    match sys.run(&world) {
                        Ok(_) => {}
                        Err(err) => tx.send(err).expect("could not send error"),
                    }
                });
            let errors = rx.try_iter().collect::<Vec<_>>();
            if !errors.is_empty() {
                return Err(From::from(MultiError::from(errors)));
            }
        }
        Ok(())
    }

    pub fn add_stage(&mut self, name: &'static str) {
        self.stages.push(Stage { name, systems: Vec::new() });
    }

    pub fn add_system_to_stage(&mut self, name: &'static str, system: Box<dyn System>) {
        for stage in &mut self.stages {
            if stage.name == name {
                stage.systems.push(system);
                return;
            }
        }
        panic!("stage {:?} doesn't exist", name);
    }
}
