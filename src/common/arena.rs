//! Taken from the [naga](https://github.com/gfx-rs/naga) project under the MIT license.
//!
//! Copyright (c) 2021 The naga project
//!
//! Permission is hereby granted, free of charge, to any person obtaining a copy
//! of this software and associated documentation files (the "Software"), to
//! deal in the Software without restriction, including without limitation the
//! rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//! sell copies of the Software, and to permit persons to whom the Software is
//! furnished to do so, subject to the following conditions:
//!
//! The above copyright notice and this permission notice shall be included in
//! all copies or substantial portions of the Software.
//!
//! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
//! IN THE SOFTWARE.

use super::src::Span;
use std::{cmp::Ordering, fmt, hash, marker::PhantomData, num::NonZeroU32, ops};

/// An unique index in the arena array that a handle points to.
/// The "non-zero" part ensures that an `Option<Handle<T>>` has
/// the same size and representation as `Handle<T>`.
type Index = NonZeroU32;

/// A strongly typed reference to an arena item.
///
/// A `Handle` value can be used as an index into an [`Arena`].
pub struct Handle<T> {
    index: Index,
    marker: PhantomData<T>,
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Handle {
            index: self.index,
            marker: self.marker,
        }
    }
}
impl<T> Copy for Handle<T> {}
impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool { self.index == other.index }
}
impl<T> Eq for Handle<T> {}
impl<T> PartialOrd for Handle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { self.index.partial_cmp(&other.index) }
}
impl<T> Ord for Handle<T> {
    fn cmp(&self, other: &Self) -> Ordering { self.index.cmp(&other.index) }
}
impl<T> fmt::Debug for Handle<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "[{}]", self.index)
    }
}
impl<T> hash::Hash for Handle<T> {
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) { self.index.hash(hasher) }
}

impl<T> Handle<T> {
    #[cfg(test)]
    pub const DUMMY: Self = Handle {
        index: unsafe { NonZeroU32::new_unchecked(!0) },
        marker: PhantomData,
    };

    pub(crate) fn new(index: Index) -> Self {
        Handle {
            index,
            marker: PhantomData,
        }
    }

    /// Returns the zero-based index of this handle.
    pub fn index(self) -> usize {
        let index = self.index.get() - 1;
        index as usize
    }

    /// Convert a `usize` index into a `Handle<T>`, without range checks.
    pub unsafe fn from_usize_unchecked(index: usize) -> Self {
        Handle::new(Index::new_unchecked((index + 1) as u32))
    }
}

/// An arena holding some kind of component (e.g., type, constant,
/// instruction, etc.) that can be referenced.
///
/// Adding new items to the arena produces a strongly-typed [`Handle`].
/// The arena can be indexed using the given handle to obtain
/// a reference to the stored item.
#[cfg_attr(test, derive(PartialEq))]
pub struct Arena<T> {
    /// Values of this arena.
    data: Vec<T>,
    span_info: Vec<Span>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self { Self::new() }
}
impl<T: fmt::Debug> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T> Arena<T> {
    /// Create a new arena with no initial capacity allocated.
    pub fn new() -> Self {
        Arena {
            data: Vec::new(),
            span_info: Vec::new(),
        }
    }

    pub fn len(&self) -> usize { self.data.len() }

    /// Returns `true` if the arena contains no elements.
    pub fn is_empty(&self) -> bool { self.data.is_empty() }

    /// Returns an iterator over the items stored in this arena, returning both
    /// the item's handle and a reference to it.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Handle<T>, &T)> {
        self.data
            .iter()
            .enumerate()
            .map(|(i, v)| unsafe { (Handle::from_usize_unchecked(i), v) })
    }

    /// Adds a new value to the arena, returning a typed handle.
    pub fn append(&mut self, value: T, span: Span) -> Handle<T> {
        let index = self.data.len();
        self.data.push(value);
        self.span_info.push(span);
        unsafe { Handle::from_usize_unchecked(index) }
    }

    pub fn get_span(&self, handle: Handle<T>) -> Span {
        *self.span_info.get(handle.index()).unwrap_or(&Span::none())
    }
}

impl<T> ops::Index<Handle<T>> for Arena<T> {
    type Output = T;

    fn index(&self, handle: Handle<T>) -> &T { &self.data[handle.index()] }
}

impl<T> ops::IndexMut<Handle<T>> for Arena<T> {
    fn index_mut(&mut self, handle: Handle<T>) -> &mut T { &mut self.data[handle.index()] }
}
