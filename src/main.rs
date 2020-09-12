use failure::Fallible;

pub mod assets;
pub mod system;

use assets::*;
use system::*;

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
    let mut system = tiny_system.system();
    system.run(&world).unwrap();
    system.run(&world).unwrap();
}
