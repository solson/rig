use str_arena::StrArena;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Debug, Formatter};
use std::marker::PhantomData;
use std::mem;

/// An interned string, represented as an index into a thread-local string interner.
#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub struct Symbol {
    index: u32,

    // Disable Send and Sync using a marker type.
    _marker: PhantomData<*const ()>,
}

impl Symbol {
    fn new(index: u32) -> Symbol {
        Symbol { index: index, _marker: PhantomData }
    }

    /// The main entry point to the interner.
    pub fn intern(string: &str) -> Symbol {
        INTERNER.with(|interner| {
            interner.borrow_mut().intern(string)
        })
    }

    pub fn with<F, R>(self, f: F) -> R where F: FnOnce(&str) -> R {
        INTERNER.with(|interner| {
            let string = interner.borrow().sym_to_str[self.index as usize];
            f(string)
        })
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.with(|name| write!(f, "{:?}:{}", name, self.index))
    }
}

thread_local!(static INTERNER: RefCell<Interner> = RefCell::new(Interner::new()));

#[derive(Debug)]
struct Interner {
    arena: StrArena,
    str_to_sym: HashMap<&'static str, Symbol>,
    sym_to_str: Vec<&'static str>,

    // Disable Send and Sync using a marker type.
    _marker: PhantomData<*const ()>,
}

impl Interner {
    fn new() -> Self {
        Interner {
            arena: StrArena::new(),
            str_to_sym: HashMap::new(),
            sym_to_str: Vec::new(),
            _marker: PhantomData,
        }
    }

    fn intern(&mut self, string: &str) -> Symbol {
        if let Some(&sym) = self.str_to_sym.get(string) {
            return sym;
        }

        let new_sym = Symbol::new(self.sym_to_str.len() as u32);
        let arena_str = self.arena.allocate(string);

        // Extend the lifetime of the slice pointer into the arena to the static lifetime. This is
        // safe because these static references never escape the interner, and dropping
        // `str_to_sym` and `sym_to_str` doesn't cause any reads of the slices (in case the arena
        // is dropped first).
        let arena_str = unsafe { mem::transmute::<&str, &'static str>(arena_str) };

        self.str_to_sym.insert(arena_str, new_sym);
        self.sym_to_str.push(arena_str);
        new_sym
    }
}
