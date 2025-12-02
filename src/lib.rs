#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]

use std::{
    ffi::{c_char, CStr},
    marker::PhantomData,
    ptr::{null, slice_from_raw_parts, NonNull},
};

pub mod kinds {
    pub struct BigDec;
    pub struct BigInt;
    pub struct Bool;
    pub struct Char;
    pub struct Double;
    pub struct Int64;
    pub struct Keyword;
    pub struct List;
    pub struct Map;
    // pub struct Ratio;
    pub struct Set;
    pub struct String;
    pub struct Symbol;
    pub struct Vector;

    pub trait EdnTag {
        fn tag_of() -> edn_type_t;
    }

    use crate::edn_type_t;
    use crate::edn_type_t::*;
    impl EdnTag for String {
        fn tag_of() -> edn_type_t {
            EDN_TYPE_STRING
        }
    }
    impl EdnTag for Set {
        fn tag_of() -> edn_type_t {
            EDN_TYPE_SET
        }
    }
    impl EdnTag for Keyword {
        fn tag_of() -> edn_type_t {
            EDN_TYPE_KEYWORD
        }
    }
    impl EdnTag for Symbol {
        fn tag_of() -> edn_type_t {
            EDN_TYPE_SYMBOL
        }
    }
}

// Include the generated bindings
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub struct Value(NonNull<edn_value_t>);

pub struct Edn<'a, T> {
    inner: NonNull<edn_value_t>,
    _phantom: PhantomData<&'a T>,
}

impl<'a, T> Clone for Edn<'a, T> {
    fn clone(&self) -> Self {
        Edn {
            inner: self.inner.clone(),
            _phantom: PhantomData,
        }
    }
}

pub struct NamespacedStr<'a> {
    namespace: Option<&'a str>,
    name: &'a str,
}

pub struct ReaderRegistry(NonNull<edn_reader_registry_t>);

#[derive(Clone)]
pub enum DefaultReaderMode {
    Passthrough,
    Unwrap,
    Error,
}

pub struct ParseOptions {
    registry: ReaderRegistry,
    eof_value: Value,
    default_reader_mode: DefaultReaderMode,
}

pub struct Arena(NonNull<edn_arena_t>);

impl Value {
    pub fn parse(_input: &str) -> Result<Self, EdnError> {
        todo!()
    }

    pub fn parse_with_options(_input: &str, _options: &ParseOptions) -> Result<Self, EdnError> {
        todo!()
    }

    pub fn into_edn<'a, U>(&'a self) -> Option<Edn<'a, U>>
    where
        U: kinds::EdnTag,
    {
        unsafe {
            if edn_type(self.as_ptr()) == U::tag_of() {
                Some(Edn {
                    inner: self.0,
                    _phantom: PhantomData,
                })
            } else {
                None
            }
        }
    }

    pub fn edn_string<'a>(&'a self) -> Option<Edn<'a, kinds::String>> {
        self.into_edn()
    }

    pub fn edn_keyword<'a>(&'a self) -> Option<Edn<'a, kinds::Symbol>> {
        self.into_edn()
    }

    pub fn edn_set<'a>(&'a self) -> Option<Edn<'a, kinds::Set>> {
        self.into_edn()
    }

    pub fn edn_symbol<'a>(&'a self) -> Option<Edn<'a, kinds::Keyword>> {
        self.into_edn()
    }

    fn as_ptr(&self) -> *const edn_value_t {
        self.0.as_ptr()
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        unsafe { edn_free(self.0.as_ptr()) }
    }
}

impl<'a, T> Edn<'a, T> {
    pub fn cast<U>(&self) -> Option<Edn<'a, U>>
    where
        U: kinds::EdnTag,
    {
        unsafe {
            if self.has_tag::<U>() {
                Some(self.cast_unchecked())
            } else {
                None
            }
        }
    }

    pub unsafe fn cast_unchecked<U>(&self) -> Edn<'a, U> {
        Edn {
            inner: self.inner,
            _phantom: PhantomData,
        }
    }

    pub fn has_tag<U>(&self) -> bool
    where
        U: kinds::EdnTag,
    {
        unsafe { edn_type(self.as_ptr()) == U::tag_of() }
    }

    pub fn is_nil(&self) -> bool {
        unsafe { edn_is_nil(self.as_ptr()) }
    }

    pub fn is_string(&self) -> bool {
        unsafe { edn_is_string(self.as_ptr()) }
    }

    pub fn is_number(&self) -> bool {
        unsafe { edn_is_number(self.as_ptr()) }
    }

    pub fn is_integer(&self) -> bool {
        unsafe { edn_is_integer(self.as_ptr()) }
    }

    pub fn is_collection(&self) -> bool {
        unsafe { edn_is_collection(self.as_ptr()) }
    }

    fn as_ptr(&self) -> *const edn_value_t {
        self.inner.as_ptr()
    }
}

impl<'a> Edn<'a, kinds::String> {
    pub fn as_c_str(&self) -> &'a CStr {
        unsafe {
            let mut len = 0usize;
            let c_ptr = edn_string_get(self.inner.as_ptr(), &raw mut len) as *const u8;
            if c_ptr.is_null() {
                panic!("Invariant violated: NULL string inside an Edn<String>")
            }
            let slice = std::slice::from_raw_parts(c_ptr, len);
            CStr::from_bytes_with_nul_unchecked(slice)
        }
    }

    pub fn as_str(&'a self) -> &'a str {
        self.as_c_str()
            .to_str()
            .expect("Invariant violated: bad UTF-8 in Edn<String>")
    }
}

impl<'a> Into<&'a str> for Edn<'a, kinds::String> {
    fn into(self) -> &'a str {
        unsafe {
            let mut len = 0usize;
            let c_ptr = edn_string_get(self.inner.as_ptr(), &raw mut len) as *const u8;
            if c_ptr.is_null() {
                panic!("Invariant violated: NULL string inside an Edn<String>")
            }
            let slice = std::slice::from_raw_parts(c_ptr, len);
            CStr::from_bytes_with_nul_unchecked(slice)
                .to_str()
                .expect("Invariant violated: bad UTF-8 in Edn<String>")
        }
    }
}

impl<'a> Edn<'a, kinds::Keyword> {
    fn as_namespaced_str(&self) -> NamespacedStr<'a> {
        unsafe {
            let mut namespace_ptr: *const c_char = std::ptr::null();
            let mut namespace_size = 0usize;
            let mut name_ptr: *const c_char = std::ptr::null();
            let mut name_size = 0usize;
            let ok = edn_keyword_get(
                self.as_ptr(),
                &raw mut namespace_ptr,
                &raw mut namespace_size,
                &raw mut name_ptr,
                &raw mut name_size,
            );
            if !ok {
                panic!("Invariant violated: bad keyword in Edn<Keyword>")
            }
            NamespacedStr::from_raw(namespace_ptr, namespace_size, name_ptr, name_size)
        }
    }
}

impl<'a> Edn<'a, kinds::Set> {
    fn len(&self) -> usize {
        unsafe { edn_set_count(self.as_ptr()) }
    }

    fn contains<U>(&self, val: Edn<'a, U>) -> bool {
        unsafe { edn_set_contains(self.as_ptr(), val.as_ptr()) }
    }

    fn iter(&self) -> impl Iterator<Item = Edn<'a, ()>> {
        EdnIterator {
            value: self.clone(),
            index: 0,
            max: self.len(),
        }
    }
}

struct EdnIterator<'a, T> {
    value: Edn<'a, T>,
    index: usize,
    max: usize,
}

impl<'a> Iterator for EdnIterator<'a, kinds::Set> {
    type Item = Edn<'a, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index > self.max {
            None
        } else {
            unsafe {
                let next = edn_set_get(self.value.as_ptr(), self.index);
                self.index += 1;
                Some(Edn {
                    inner: NonNull::new_unchecked(next),
                    _phantom: PhantomData,
                })
            }
        }
    }
}

impl<'a> Edn<'a, kinds::Symbol> {
    fn as_namespaced_str(&self) -> NamespacedStr<'a> {
        unsafe {
            let mut namespace_ptr: *const c_char = std::ptr::null();
            let mut namespace_size = 0usize;
            let mut name_ptr: *const c_char = std::ptr::null();
            let mut name_size = 0usize;
            let ok = edn_symbol_get(
                self.as_ptr(),
                &raw mut namespace_ptr,
                &raw mut namespace_size,
                &raw mut name_ptr,
                &raw mut name_size,
            );
            if !ok {
                panic!("Invariant violated: bad keyword in Edn<Keyword>")
            }
            NamespacedStr::from_raw(namespace_ptr, namespace_size, name_ptr, name_size)
        }
    }
}

impl<'a> NamespacedStr<'a> {
    fn from_raw(
        namespace_ptr: *const c_char,
        namespace_len: usize,
        name_ptr: *const c_char,
        name_len: usize,
    ) -> Self {
        unsafe {
            let mut namespace: Option<&'a str> = None;
            if !namespace_ptr.is_null() {
                let namespace_slice =
                    std::slice::from_raw_parts(namespace_ptr as *const u8, namespace_len);
                namespace = Some(
                    CStr::from_bytes_with_nul_unchecked(namespace_slice)
                        .to_str()
                        .expect("Invariant violated: bad UTF-8"),
                );
            }
            let name_slice = std::slice::from_raw_parts(name_ptr as *const u8, name_len);
            let name = CStr::from_bytes_with_nul_unchecked(name_slice)
                .to_str()
                .expect("Invariant violated: bad UTF-8");
            NamespacedStr { namespace, name }
        }
    }
}

impl Into<edn_default_reader_mode_t> for DefaultReaderMode {
    fn into(self) -> edn_default_reader_mode_t {
        use edn_default_reader_mode_t::*;
        match self {
            DefaultReaderMode::Passthrough => EDN_DEFAULT_READER_PASSTHROUGH,
            DefaultReaderMode::Unwrap => EDN_DEFAULT_READER_UNWRAP,
            DefaultReaderMode::Error => EDN_DEFAULT_READER_ERROR,
        }
    }
}

// TODO: how do we ensure that reader registries live long enough
impl ReaderRegistry {
    pub fn new() -> Self {
        unsafe {
            let registry = edn_reader_registry_create();
            if registry.is_null() {
                panic!("Out of memory in ReaderRegistry::new")
            }
            ReaderRegistry(NonNull::new_unchecked(registry))
        }
    }

    pub fn register<'a, F>(&self, _tag: &str, _reader: F)
    where
        F: Fn(Value, &'a Arena, &'a str) -> (),
    {
        todo!()
    }
}

impl Drop for ReaderRegistry {
    fn drop(&mut self) {
        unsafe { edn_reader_registry_destroy(self.0.as_ptr()) }
    }
}

impl ParseOptions {
    fn as_raw(&self) -> edn_parse_options_t {
        edn_parse_options_t {
            reader_registry: self.registry.0.as_ptr(),
            eof_value: self.eof_value.0.as_ptr(),
            default_reader_mode: self.default_reader_mode.clone().into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EdnErrorKind {
    InvalidSyntax,
    UnexpectedEof,
    InvalidUtf8,
    InvalidNumber,
    InvalidString,
    InvalidEscape,
    UnmatchedDelimiter,
    UnknownTag,
    DuplicateKey,
    DuplicateElement,
    OutOfMemory,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdnError {
    pub kind: EdnErrorKind,
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl From<edn_error_t> for EdnErrorKind {
    fn from(error: edn_error_t) -> Self {
        use edn_error_t::*;
        match error {
            EDN_OK => unreachable!("EDN_OK should not be converted to EdnErrorKind"),
            EDN_ERROR_INVALID_SYNTAX => EdnErrorKind::InvalidSyntax,
            EDN_ERROR_UNEXPECTED_EOF => EdnErrorKind::UnexpectedEof,
            EDN_ERROR_OUT_OF_MEMORY => EdnErrorKind::OutOfMemory,
            EDN_ERROR_INVALID_UTF8 => EdnErrorKind::InvalidUtf8,
            EDN_ERROR_INVALID_NUMBER => EdnErrorKind::InvalidNumber,
            EDN_ERROR_INVALID_STRING => EdnErrorKind::InvalidString,
            EDN_ERROR_INVALID_ESCAPE => EdnErrorKind::InvalidEscape,
            EDN_ERROR_UNMATCHED_DELIMITER => EdnErrorKind::UnmatchedDelimiter,
            EDN_ERROR_UNKNOWN_TAG => EdnErrorKind::UnknownTag,
            EDN_ERROR_DUPLICATE_KEY => EdnErrorKind::DuplicateKey,
            EDN_ERROR_DUPLICATE_ELEMENT => EdnErrorKind::DuplicateElement,
        }
    }
}
