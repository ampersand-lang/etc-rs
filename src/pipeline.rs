//! Contains a runnable, normalized pipeline for the compiler.

use failure::Fallible;
#[allow(unused_imports)]
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
    repeat: Option<Box<dyn FnMut(&World) -> Option<&'static str> + Send + Sync + 'static>>,
}

impl Pipeline {
    /// Constructs a new empty pipeline.
    pub fn new() -> Self {
        Self {
            stages: Vec::new(),
            repeat: None,
        }
    }

    /// Sets the `repeat` predicate.
    pub fn repeat<F: FnMut(&World) -> Option<&'static str> + Send + Sync + 'static>(
        &mut self,
        f: F,
    ) {
        self.repeat = Some(Box::new(f));
    }

    /// Runs this pipeline in the given world.
    pub fn run(&mut self, world: &World) -> Fallible<()> {
        let mut idx = 0;
        let mut brk = false;
        let mut repeat = self.repeat.as_mut();
        let stages = self
            .stages
            .iter()
            .map(|stage| stage.name.clone())
            .collect::<Vec<_>>();
        while let Some(stage) = self.stages.get_mut(idx) {
            if brk {
                break;
            }
            let mut errors = Vec::new();
            // let (tx, rx) = mpsc::channel();
            // stage.systems.par_iter_mut().for_each_with(tx, |tx, sys| {
            stage.systems.iter_mut().for_each(|sys| {
                let world = world.clone();
                match sys.run(&world) {
                    Ok(Some("repeat")) => {
                        let name = (repeat.as_mut().unwrap())(&world);
                        if let Some(name) = name {
                            let position = stages.iter().position(|stage| *stage == name);
                            idx = position.unwrap();
                        }
                    }
                    Ok(Some("finish")) => {
                        brk = true;
                    }
                    Ok(Some(name)) => {
                        let position = stages.iter().position(|stage| *stage == name);
                        idx = position.unwrap();
                    }
                    Ok(None) => idx += 1,
                    // Err(err) => tx.send(err).expect("could not send error"),
                    Err(err) => errors.push(err),
                }
            });
            // let errors = rx.iter().collect::<Vec<_>>();
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
