use failure::Fallible;

use crate::assets::{Asset, Handle, Resources};
use crate::ast::Node;
use crate::types::NamedType;
use crate::lir::context::ExecutionContext;
use crate::lir::builder::*;

pub trait Compile<B>: Asset + Sized {
    type Output;
    
    fn compile(handle: Handle<Self>, res: &Resources<&mut Self>, builder: B) -> Fallible<Self::Output>;
}

impl<'a> Compile<Builder<'a>> for Node {
    type Output = (Resources<&'a NamedType>, ExecutionContext);
    
    fn compile(handle: Handle<Self>, res: &Resources<&mut Self>, builder: Builder<'a>) -> Fallible<Self::Output> {
        todo!()
    }
}

impl<'a> Compile<FunctionBuilder<'a>> for Node {
    type Output = Builder<'a>;
    
    fn compile(handle: Handle<Self>, res: &Resources<&mut Self>, builder: FunctionBuilder<'a>) -> Fallible<Self::Output> {
        todo!()
    }
}
