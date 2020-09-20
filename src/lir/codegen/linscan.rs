use crate::lir::BindingPrototype;

use super::*;

#[derive(Debug, Clone, Copy)]
pub enum RegisterPurpose {
    Allocated(Register),
    Spilled(u64),
}

#[derive(Debug, Clone, Copy)]
pub struct Interval {
    pub stack: bool,
    pub name: BindingPrototype,
    pub ti: TypeInfo,
    pub reg: RegisterPurpose,
    pub start: i64,
    pub end: i64,
}

impl Interval {
    pub fn new(name: BindingPrototype, ti: TypeInfo, start: i64, end: i64) -> Self {
        Self {
            stack: false,
            name,
            ti,
            reg: RegisterPurpose::Spilled(0),
            start,
            end,
        }
    }

    pub fn on_stack(name: BindingPrototype, ti: TypeInfo, start: i64, end: i64) -> Self {
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

    pub fn allocate(&mut self) {
        self.live.sort_unstable_by_key(|i| i.start);
        let mut expire = Vec::new();
        let mut delta = Vec::new();
        for idx in 0..self.live.len() {
            let i = &self.live[idx];

            for (j, (jdx, _)) in self.active.iter().enumerate() {
                if self.live[*jdx].end >= i.start {
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
                    if spill.end > i.end {
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
                self.active.sort_unstable_by_key(|(idx, _)| live[*idx].end);
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
            1,
            10,
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 2),
            TypeInfo::new(8, 8),
            1,
            7,
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 3),
            TypeInfo::new(8, 8),
            3,
            6,
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 4),
            TypeInfo::new(8, 8),
            7,
            10,
        ));
        alloc.add(Interval::new(
            BindingPrototype::new(0, 5),
            TypeInfo::new(8, 8),
            6,
            8,
        ));
        alloc.allocate();
        for i in alloc.iter() {
            println!("{:?}", i);
        }
    }
}
