//! Contains a runnable, normalized pipeline for the compiler.
use std::sync::mpsc;

use failure::Fallible;
use rayon::prelude::*;

use crate::assets::World;
use crate::error::MultiError;
use crate::system::System;

/// A disjuction of stages that may run at the same time.
pub struct Stage {
    name: &'static str,
    systems: Vec<Box<dyn System>>,
}

/// A conjuction of stages that run in series.
pub struct Pipeline {
    stages: Vec<Stage>,
}

impl Pipeline {
    /// Constructs a new empty pipeline.
    pub fn new() -> Self {
        Self { stages: Vec::new() }
    }

    /// Runs this pipeline in the given world.
    pub fn run(&mut self, world: &World) -> Fallible<()> {
        for stage in &mut self.stages {
            let (tx, rx) = mpsc::channel();
            stage.systems.par_iter_mut().for_each_with(tx, |tx, sys| {
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

    /// Add an empty stage.
    pub fn add_stage(&mut self, name: &'static str) {
        self.stages.push(Stage {
            name,
            systems: Vec::new(),
        });
    }

    /// Add a system to the specified stage.
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
