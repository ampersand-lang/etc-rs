use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::mem;
use std::path::Path;
use std::process;

use etc_rs::assets::*;
use etc_rs::ast::{Node, RootNode};
use etc_rs::dispatch;
use etc_rs::lexer::Lexer;
use etc_rs::lir::{context::ExecutionContext, foreign, ThreadId};
use etc_rs::parser::{grammar, State};
use etc_rs::types::{builtin, primitive};

fn try_main() -> io::Result<()> {
    let directory = env::args().skip(1).next().unwrap_or(".".to_string());
    let directory: &Path = directory.as_ref();
    let filename = directory.join("manifest.amp");
    println!("* etc-rs: reading manifest from: `{}`", filename.display());
    let mut file = File::open(&filename)?;
    let mut src = String::new();
    file.read_to_string(&mut src)?;

    let world = World::new();
    etc_rs::init_assets(&world);

    primitive::init(world.resources());
    builtin::init(world.resources());
    foreign::init(world.resources());
    dispatch::init(world.resources());

    let filename = &filename.to_string_lossy();
    let node = grammar::parse(&mut State::new(
        Lexer::new(filename, &src, world.resources()),
        world.resources(),
    ));

    let node = match node {
        Ok(node) => node,
        Err(err) => {
            eprintln!("errors:");
            eprintln!("{}", err);
            process::exit(1);
        }
    };

    let handle = Handle::new();
    let root = RootNode(node, false);
    world.insert(handle, root);

    let mut pipeline = etc_rs::interpreter_pipeline();

    if let Err(err) = pipeline.run(&world) {
        eprintln!("errors:");
        eprintln!("{}", err);
        process::exit(1);
    }

    let node = world.get(node).unwrap();
    let ctx = world.get(node.thread.unwrap()).unwrap();
    let compile_list = ctx.compile_list().to_vec();
    mem::drop(node);
    mem::drop(ctx);

    let interpreted = compile_list
        .iter()
        .take(compile_list.len().saturating_sub(1));

    let compiled = compile_list.last();

    for filename in interpreted {
        let mut remove = Vec::new();
        let mut roots = world.resources::<&mut RootNode>();
        let nodes = world.resources::<&Node>();
        let mut threads = world.resources::<&mut ExecutionContext>();
        let mut ctx: Option<ExecutionContext> = None;
        for (id, root_node) in roots.iter::<RootNode>() {
            let thread = nodes.get(root_node.0).unwrap().thread.unwrap();
            let thread = threads.remove(thread).unwrap();
            if let Some(ctx) = &mut ctx {
                ctx.extend(thread);
            } else {
                ctx = Some(thread);
            }
            remove.push(id);
        }
        let thread = ctx.map(|ctx| {
            let thread = ThreadId::new();
            threads.insert(thread, ctx);
            thread
        });
        for id in remove {
            roots.remove(id);
        }
        mem::drop(roots);
        mem::drop(nodes);
        mem::drop(threads);

        let filename = directory.join(filename);
        println!("* etc-rs: interpreting: `{}`", filename.display());
        let mut file = File::open(&filename)?;
        let mut src = String::new();
        file.read_to_string(&mut src)?;

        let filename = &filename.to_string_lossy();
        let node = grammar::parse(&mut State::new(
            Lexer::new(filename, &src, world.resources()),
            world.resources(),
        ));

        let node = match node {
            Ok(node) => node,
            Err(err) => {
                eprintln!("errors:");
                eprintln!("{}", err);
                process::exit(1);
            }
        };

        world.get_mut(node).unwrap().thread = thread;

        let handle = Handle::new();
        let root = RootNode(node, false);
        world.insert(handle, root);

        let mut pipeline = etc_rs::interpreter_pipeline();

        if let Err(err) = pipeline.run(&world) {
            eprintln!("errors:");
            eprintln!("{}", err);
            process::exit(1);
        }
    }

    if let Some(filename) = compiled {
        let mut remove = Vec::new();
        let mut roots = world.resources::<&mut RootNode>();
        let nodes = world.resources::<&Node>();
        let mut threads = world.resources::<&mut ExecutionContext>();
        let mut ctx: Option<ExecutionContext> = None;
        for (id, root_node) in roots.iter::<RootNode>() {
            let thread = nodes.get(root_node.0).unwrap().thread.unwrap();
            let thread = threads.remove(thread).unwrap();
            if let Some(ctx) = &mut ctx {
                ctx.extend(thread);
            } else {
                ctx = Some(thread);
            }
            remove.push(id);
        }
        let thread = ctx.map(|ctx| {
            let thread = ThreadId::new();
            threads.insert(thread, ctx);
            thread
        });
        for id in remove {
            roots.remove(id);
        }
        mem::drop(roots);
        mem::drop(nodes);
        mem::drop(threads);

        let filename = directory.join(filename);
        println!("* etc-rs: compiling: `{}`", filename.display());
        let mut file = File::open(&filename)?;
        let mut src = String::new();
        file.read_to_string(&mut src)?;

        let filename = &filename.to_string_lossy();
        let node = grammar::parse(&mut State::new(
            Lexer::new(filename, &src, world.resources()),
            world.resources(),
        ));

        let node = match node {
            Ok(node) => node,
            Err(err) => {
                eprintln!("errors:");
                eprintln!("{}", err);
                process::exit(1);
            }
        };

        world.get_mut(node).unwrap().thread = thread;

        let handle = Handle::new();
        let root = RootNode(node, false);
        world.insert(handle, root);

        let mut pipeline = etc_rs::compiler_pipeline();

        if let Err(err) = pipeline.run(&world) {
            eprintln!("errors:");
            eprintln!("{}", err);
            process::exit(1);
        }
    }

    Ok(())
}

fn main() {
    if let Err(e) = try_main() {
        eprintln!("{}", e);
    }
}
