use failure::Fallible;

pub mod error;
pub mod assets;
pub mod system;
pub mod pipeline;

use assets::*;
use system::*;
use pipeline::*;

fn tiny_system(lazy: &mut LazyUpdate, res: Resources<(&i32, &String)>) -> Fallible<()> {
    for (_, i) in res.iter::<i32>() {
        println!("{}", &*i);
    }
    for (_, s) in res.iter::<String>() {
        println!("{:?}", &*s);
    }
    lazy.insert(Handle::new(), 42_i32);
    lazy.insert(Handle::new(), "42".to_string());
    Ok(())
}

fn main() {
    let world = World::new();
    world.init_asset::<i32>();
    world.init_asset::<String>();
    let system = tiny_system.system();
    let mut pipeline = Pipeline::new();
    pipeline.add_stage("init");
    pipeline.add_system_to_stage("init", system);
    pipeline.run(&world).unwrap();
    pipeline.run(&world).unwrap();
}
