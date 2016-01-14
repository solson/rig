//! Provides a simple but efficient arena allocator for allocating long-lived `&str` values
//! cheaply.
//!
//! Its safety relies on the fact that it never grows its internal `String` chunks beyond their
//! capacities, so they never reallocate their data buffers. Thus, pointers into these chunks are
//! valid for as long as the arena itself is alive and so `allocate` returns a slice pointer with
//! the lifetime of the arena.

use std::cell::RefCell;
use std::cmp;
use std::mem;

/// This must be at least 1 or else the chunk growing code would multiply 0 by 2 and still have no
/// space.
const MIN_CAPACITY: usize = 1;

/// This is pretty arbitrary and ought to be tuned.
const DEFAULT_CAPACITY: usize = 1024;

/// The factor to multiply the current chunk capacity by to determine the capacity of the next
/// chunk.
const GROWTH_FACTOR: usize = 2;

#[derive(Debug)]
pub struct StrArena {
    chunks: RefCell<ChunkList>,
}

#[derive(Debug)]
struct ChunkList {
    current: String,
    previous: Vec<String>,
}

impl StrArena {
    pub fn new() -> Self {
        StrArena::with_capacity(DEFAULT_CAPACITY)
    }

    pub fn with_capacity(mut cap: usize) -> Self {
        if cap < MIN_CAPACITY {
            cap = MIN_CAPACITY;
        }

        StrArena {
            chunks: RefCell::new(ChunkList {
                current: String::with_capacity(cap),
                previous: Vec::new(),
            }),
        }
    }

    pub fn allocate<'arena>(&'arena self, string: &str) -> &'arena str {
        let mut chunks = self.chunks.borrow_mut();

        let mut old_len = chunks.current.len();
        let free_space = chunks.current.capacity() - old_len;

        if string.len() > free_space {
            chunks.grow(string.len());
            old_len = 0;
        }

        // If no grow happened above, the current chunk already has room for `string`. If a grow
        // happened, the new current chunk has at least `string.len()` capacity.
        chunks.current.push_str(string);

        // Get a pointer to the newly allocated str.
        let new_slice = &chunks.current[old_len..old_len + string.len()];

        // Extend the lifetime of `new_slice` from the lifetime of `chunks` to 'arena. This is safe
        // because the arena ensures the data in the  chunk `new_slice` points into will never be
        // modified or moved.
        unsafe {
            mem::transmute::<&str, &'arena str>(new_slice)
        }
    }
}

impl ChunkList {
    fn grow(&mut self, min_size: usize) {
        let new_cap =
            cmp::max(self.current.capacity(), min_size).checked_mul(GROWTH_FACTOR).unwrap();
        let prev_chunk = mem::replace(&mut self.current, String::with_capacity(new_cap));
        self.previous.push(prev_chunk);
    }
}

#[cfg(test)]
mod test {
    use super::StrArena;

    #[test]
    fn basic() {
        let arena = StrArena::new();
        let a = arena.allocate("this is ");
        let b = arena.allocate("a test");
        assert_eq!(a, "this is ");
        assert_eq!(b, "a test");
        assert_eq!(arena.chunks.borrow().previous, &[] as &[String]);
        assert_eq!(arena.chunks.borrow().current, "this is a test");
    }

    #[test]
    fn small_capacity() {
        let arena = StrArena::with_capacity(5);

        // Fits in the original 5 capacity.
        let a = arena.allocate("123");

        // Arena grows a new chunk of capacity 10.
        let b = arena.allocate("4567");

        // Fits in the chunk of capacity 10.
        let c = arena.allocate("89012");

        // Doesn't fit, and wouldn't fit the next natural capacity of 20. Instead, the arena
        // expands to the size of this string times 2.
        let d = arena.allocate("this is a longer string that will not fit");
        let e = arena.allocate("there is some space left over");

        assert_eq!(a, "123");
        assert_eq!(b, "4567");
        assert_eq!(c, "89012");
        assert_eq!(d, "this is a longer string that will not fit");
        assert_eq!(e, "there is some space left over");

        assert_eq!(arena.chunks.borrow().previous, &["123", "456789012"]);
        assert_eq!(arena.chunks.borrow().current,
                   "this is a longer string that will not fitthere is some space left over");
    }
}
