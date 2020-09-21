use crate::lir::{BasicBlock, BasicBlockExt, BasicBlockPrototype, BindingPrototype};

use super::*;

#[derive(Debug, Clone, Copy)]
pub struct Lifetime {
    pub(crate) block: BasicBlock,
    pub(crate) position: u32,
}

impl Lifetime {
    pub fn new(block: BasicBlock, position: u32) -> Self {
        Self { block, position }
    }

    pub fn empty(block: BasicBlock) -> Self {
        Self { block, position: 0 }
    }

    pub fn to_cmp(&self) -> (BasicBlock, u32) {
        (self.block, self.position)
    }

    pub fn ge(&self, other: &Self, prot: &[BasicBlockPrototype]) -> bool {
        if self.block == other.block {
            self.position >= other.position
        } else if prot.is_child(self.block, other.block) {
            false
        } else if prot.is_child(other.block, self.block) {
            true
        } else {
            false
        }
    }

    pub fn gt(&self, other: &Self, prot: &[BasicBlockPrototype]) -> bool {
        if self.block == other.block {
            self.position > other.position
        } else if prot.is_child(self.block, other.block) {
            false
        } else if prot.is_child(other.block, self.block) {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterPurpose {
    Allocated(Register),
    Spilled(u64),
}

impl RegisterPurpose {
    pub fn is_spilled(&self) -> bool {
        match self {
            Self::Allocated(_) => false,
            Self::Spilled(_) => true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Interval {
    pub stack: bool,
    pub name: BindingPrototype,
    pub ti: TypeInfo,
    pub reg: RegisterPurpose,
    pub start: Lifetime,
    pub end: Lifetime,
}

impl Interval {
    pub fn new(name: BindingPrototype, ti: TypeInfo, start: Lifetime, end: Lifetime) -> Self {
        Self {
            stack: false,
            name,
            ti,
            reg: RegisterPurpose::Spilled(0),
            start,
            end,
        }
    }

    pub fn on_stack(name: BindingPrototype, ti: TypeInfo, start: Lifetime, end: Lifetime) -> Self {
        Self {
            stack: true,
            name,
            ti,
            reg: RegisterPurpose::Spilled(0),
            start,
            end,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Allocator {
    pool: Vec<Register>,
    live: Vec<Interval>,
    stack: Vec<Interval>,
    active: Vec<(usize, Register)>,
    offset: u64,
}

impl Allocator {
    pub fn new<I: IntoIterator<Item = Register>>(pool: I) -> Self {
        Self {
            pool: pool.into_iter().collect(),
            live: Vec::new(),
            stack: Vec::new(),
            active: Vec::new(),
            offset: 0,
        }
    }

    pub fn add(&mut self, mut i: Interval) -> &mut Self {
        if i.ti.size > 8 || i.stack {
            self.offset = self.offset.align_up(i.ti.align as u64);
            self.offset += i.ti.size as u64;
            i.reg = RegisterPurpose::Spilled(self.offset);
            self.stack.push(i);
        } else {
            self.live.push(i);
        }
        self
    }

    pub fn size(&self) -> u64 {
        self.offset
    }

    pub fn iter(&self) -> impl Iterator<Item = &Interval> {
        self.live.iter().chain(self.stack.iter())
    }

    pub fn allocate(&mut self, prot: &[BasicBlockPrototype]) {
        self.live.sort_unstable_by_key(|i| i.start.to_cmp());
        let mut expire = Vec::new();
        let mut delta = Vec::new();
        for idx in 0..self.live.len() {
            let i = &self.live[idx];

            for (j, (jdx, _)) in self.active.iter().enumerate() {
                if self.live[*jdx].end.ge(&i.start, prot) {
                    break;
                }
                expire.push(j);
            }

            for j in expire.drain(..) {
                self.pool.push(self.active.remove(j).1);
            }

            if self.pool.is_empty() {
                if let Some((jdx, _reg)) = self.active.last() {
                    let j = self.active.len() - 1;
                    let spill = &self.live[*jdx];
                    if spill.end.gt(&i.end, prot) {
                        delta.push((idx, spill.reg));
                        self.offset += 8;
                        delta.push((*jdx, RegisterPurpose::Spilled(self.offset)));
                        let (_, reg) = self.active.remove(j);
                        self.active.push((idx, reg));
                    } else {
                        self.offset += 8;
                        delta.push((idx, RegisterPurpose::Spilled(self.offset)));
                    }
                }
            } else {
                let reg = self.pool.pop().unwrap();
                delta.push((idx, RegisterPurpose::Allocated(reg)));
                self.active.push((idx, reg));
                // PERF: sorting in every repetition of this loop *might* be inoptimal
                let live = &self.live;
                self.active
                    .sort_unstable_by_key(|(idx, _)| live[*idx].end.to_cmp());
            }

            for (idx, reg) in delta.drain(..) {
                self.live[idx].reg = reg;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn alloc() {
        let pool = vec![Register::rax(), Register::rdx(), Register::rcx()];
        let mut alloc = Allocator::new(pool);
        alloc.add(Interval::new(
            BindingPrototype::new(0, 1),
            TypeInfo::new(8, 8),
            Lifetime::new(BasicBlock::new(0), 1),
            Lifetime::new(BasicBlock::new(0), 10),
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 2),
            TypeInfo::new(8, 8),
            Lifetime::new(BasicBlock::new(0), 1),
            Lifetime::new(BasicBlock::new(0), 7),
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 3),
            TypeInfo::new(8, 8),
            Lifetime::new(BasicBlock::new(0), 3),
            Lifetime::new(BasicBlock::new(0), 6),
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 4),
            TypeInfo::new(8, 8),
            Lifetime::new(BasicBlock::new(0), 7),
            Lifetime::new(BasicBlock::new(0), 10),
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 5),
            TypeInfo::new(8, 8),
            Lifetime::new(BasicBlock::new(0), 6),
            Lifetime::new(BasicBlock::new(0), 8),
        ));
        alloc.allocate(&[BasicBlockPrototype::with_start(0, 0)]);
        for i in alloc.iter() {
            println!("{:?}", i);
        }
    }

    #[test]
    fn basic_blocks() {
        let pool = vec![Register::rax()];
        let mut alloc = Allocator::new(pool);
        alloc.add(Interval::new(
            BindingPrototype::new(0, 1),
            TypeInfo::new(8, 8),
            Lifetime::new(BasicBlock::new(0), 1),
            Lifetime::new(BasicBlock::new(3), 10),
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 2),
            TypeInfo::new(8, 8),
            Lifetime::new(BasicBlock::new(1), 4),
            Lifetime::new(BasicBlock::new(1), 5),
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 3),
            TypeInfo::new(8, 8),
            Lifetime::new(BasicBlock::new(2), 6),
            Lifetime::new(BasicBlock::new(2), 7),
        ));
        let mut root = BasicBlockPrototype::with_start(0, 0);
        let mut a = BasicBlockPrototype::with_start(1, 4);
        let mut b = BasicBlockPrototype::with_start(2, 6);
        let end = BasicBlockPrototype::with_start(3, 8);
        root.add(1);
        root.add(2);
        a.add(3);
        b.add(3);
        alloc.allocate(&[root, a, b, end]);
        for i in alloc.iter() {
            println!("{:?}", i);
        }
    }
}
