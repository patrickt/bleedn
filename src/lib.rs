#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]

use std::{
    ffi::{c_char, CStr, CString},
    fmt,
    marker::PhantomData,
    ptr::{null, null_mut, NonNull},
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
    pub struct Nil;
    pub struct Ratio;
    pub struct Set;
    pub struct String;
    pub struct Symbol;
    pub struct Tagged;
    pub struct Vector;

    pub trait EdnTag {
        fn tag_of() -> edn_type_t;
    }

    use crate::c::edn_type_t;

    macro_rules! impl_edn_tag {
        ($($kind:ident => $tag:ident),* $(,)?) => {
            $(
                impl EdnTag for $kind {
                    fn tag_of() -> edn_type_t {
                        edn_type_t::$tag
                    }
                }
            )*
        };
    }

    impl_edn_tag! {
        Nil     => EDN_TYPE_NIL,
        Bool    => EDN_TYPE_BOOL,
        Int64   => EDN_TYPE_INT,
        BigInt  => EDN_TYPE_BIGINT,
        Double  => EDN_TYPE_FLOAT,
        BigDec  => EDN_TYPE_BIGDEC,
        Ratio   => EDN_TYPE_RATIO,
        Char    => EDN_TYPE_CHARACTER,
        String  => EDN_TYPE_STRING,
        Symbol  => EDN_TYPE_SYMBOL,
        Keyword => EDN_TYPE_KEYWORD,
        List    => EDN_TYPE_LIST,
        Vector  => EDN_TYPE_VECTOR,
        Map     => EDN_TYPE_MAP,
        Set     => EDN_TYPE_SET,
        Tagged  => EDN_TYPE_TAGGED,
    }
}

// Include the generated bindings
pub mod c {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

pub struct Root(NonNull<c::edn_value_t>);

pub struct Edn<'a, T> {
    inner: NonNull<c::edn_value_t>,
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

pub struct Bignum<'a> {
    value: &'a str,
    negative: bool,
    radix: Option<u8>,
}

pub struct NamespacedStr<'a> {
    namespace: Option<&'a str>,
    name: &'a str,
}

pub struct ReaderRegistry(NonNull<c::edn_reader_registry_t>);

#[derive(Clone)]
pub enum DefaultReaderMode {
    Passthrough,
    Unwrap,
    Error,
}

pub struct ParseOptions {
    registry: ReaderRegistry,
    eof_value: Root,
    default_reader_mode: DefaultReaderMode,
}

pub struct Arena(NonNull<c::edn_arena_t>);

// TODO: backwards iterator?
struct EdnIterator<'a, T> {
    value: Edn<'a, T>,
    index: usize,
    max: usize,
}

impl Root {
    pub fn parse_cstr(input: &CStr) -> Result<Self, EdnError> {
        unsafe {
            let c_result = c::edn_read(input.as_ptr(), input.count_bytes());
            if c_result.error == c::edn_error_t::EDN_OK {
                Ok(Root(NonNull::new_unchecked(c_result.value)))
            } else {
                Err(EdnError {
                    kind: c_result.error.into(),
                    line: c_result.error_line,
                    column: c_result.error_column,
                    message: CStr::from_ptr(c_result.error_message)
                        .to_string_lossy()
                        .into_owned(),
                })
            }
        }
    }

    pub fn parse_cstr_with_options(input: &CStr, options: &ParseOptions) -> Result<Self, EdnError> {
        unsafe {
            let mut c_options = options.as_raw();
            let c_result =
                c::edn_read_with_options(input.as_ptr(), input.count_bytes(), &raw mut c_options);
            if c_result.error == c::edn_error_t::EDN_OK {
                Ok(Root(NonNull::new_unchecked(c_result.value)))
            } else {
                Err(EdnError {
                    kind: c_result.error.into(),
                    line: c_result.error_line,
                    column: c_result.error_column,
                    message: CStr::from_ptr(c_result.error_message)
                        .to_string_lossy()
                        .into_owned(),
                })
            }
        }
    }

    pub fn any<'a>(&'a self) -> Edn<'a, ()> {
        Edn::new(self.0)
    }

    pub fn into_edn<'a, U>(&'a self) -> Option<Edn<'a, U>>
    where
        U: kinds::EdnTag,
    {
        unsafe {
            if c::edn_type(self.as_ptr()) == U::tag_of() {
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

    fn as_ptr(&self) -> *const c::edn_value_t {
        self.0.as_ptr()
    }
}

impl Drop for Root {
    fn drop(&mut self) {
        unsafe { c::edn_free(self.0.as_ptr()) }
    }
}

impl<'a, T> Edn<'a, T> {
    fn new(inner: NonNull<c::edn_value_t>) -> Self {
        Edn {
            inner,
            _phantom: PhantomData,
        }
    }

    fn from_raw(val: *mut c::edn_value_t) -> Option<Edn<'a, T>> {
        NonNull::new(val).map(Self::new)
    }

    unsafe fn from_raw_unchecked(val: *mut c::edn_value_t) -> Self {
        Self::new(NonNull::new_unchecked(val))
    }

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
        unsafe { c::edn_type(self.as_ptr()) == U::tag_of() }
    }

    pub fn is_nil(&self) -> bool {
        unsafe { c::edn_is_nil(self.as_ptr()) }
    }

    pub fn is_string(&self) -> bool {
        unsafe { c::edn_is_string(self.as_ptr()) }
    }

    pub fn is_number(&self) -> bool {
        unsafe { c::edn_is_number(self.as_ptr()) }
    }

    pub fn is_integer(&self) -> bool {
        unsafe { c::edn_is_integer(self.as_ptr()) }
    }

    pub fn is_collection(&self) -> bool {
        unsafe { c::edn_is_collection(self.as_ptr()) }
    }

    pub fn metadata(&self) -> Option<Edn<'a, kinds::Map>> {
        Edn::from_raw(unsafe { c::edn_value_meta(self.as_ptr()) })
    }

    pub fn has_metadata(&self) -> bool {
        unsafe { c::edn_value_has_meta(self.as_ptr()) }
    }

    // TODO: why can't this be a TryInto implementation without conflicting
    // with the Into declaration for Edn<'a, Double>?
    pub fn as_f64(&self) -> Option<f64> {
        unsafe {
            let mut val = 0.0f64;
            if c::edn_number_as_double(self.as_ptr(), &raw mut val) {
                Some(val)
            } else {
                None
            }
        }
    }

    fn as_ptr(&self) -> *const c::edn_value_t {
        self.inner.as_ptr()
    }
}

/// Nil

impl<'a> Edn<'a, kinds::Nil> {}

/// Bool

impl<'a> Into<bool> for Edn<'a, kinds::Bool> {
    fn into(self) -> bool {
        unsafe {
            let mut val = false;
            let ok = c::edn_bool_get(self.as_ptr(), &raw mut val);
            if !ok {
                panic!("Invariant violated: edn_bool_get failed")
            }
            val
        }
    }
}

/// Int64

impl<'a> Into<i64> for Edn<'a, kinds::Int64> {
    fn into(self) -> i64 {
        unsafe {
            let mut val = 0i64;
            let ok = c::edn_int64_get(self.as_ptr(), &raw mut val);
            if !ok {
                panic!("Invariant violated: edn_int64_get failed")
            }
            val
        }
    }
}

/// Bigints

impl<'a> Edn<'a, kinds::BigInt> {
    pub fn to_bignum(&self) -> Bignum<'a> {
        unsafe {
            let mut length = 0usize;
            let mut negative = false;
            let mut radix = 0u8;
            let val_ptr = c::edn_bigint_get(
                self.as_ptr(),
                &raw mut length,
                &raw mut negative,
                &raw mut radix,
            ) as *const u8;
            if val_ptr.is_null() {
                panic!("Invariant violated: edn_bigint_getfailed")
            }
            let slice = std::slice::from_raw_parts(val_ptr, length);
            Bignum {
                value: str::from_utf8_unchecked(slice),
                negative,
                radix: Some(radix),
            }
        }
    }
}

/// Doubles

impl<'a> Into<f64> for Edn<'a, kinds::Double> {
    fn into(self) -> f64 {
        unsafe {
            let mut val = 0f64;
            let ok = c::edn_double_get(self.as_ptr(), &raw mut val);
            if !ok {
                panic!("Invariant violated: edn_double_get failed")
            }
            val
        }
    }
}

/// Ratio

impl<'a> Edn<'a, kinds::Ratio> {
    pub fn get(&self) -> (i64, i64) {
        unsafe {
            let mut numerator = 0i64;
            let mut denominator = 0i64;
            let ok = c::edn_ratio_get(self.as_ptr(), &raw mut numerator, &raw mut denominator);
            if !ok {
                panic!("Invariant violated: edn_ratio_get failed")
            }
            (numerator, denominator)
        }
    }
}

/// Characters
impl<'a> Into<char> for Edn<'a, kinds::Char> {
    fn into(self) -> char {
        unsafe {
            let mut val = 0u32;
            let ok = c::edn_character_get(self.as_ptr(), &raw mut val);
            if !ok {
                panic!("Invariant violated: edn_character_get failed")
            }
            char::from_u32_unchecked(val)
        }
    }
}

/// Strings

impl<'a> Edn<'a, kinds::String> {
    pub fn equals(&self, val: &CStr) -> bool {
        unsafe { c::edn_string_equals(self.as_ptr(), val.as_ptr()) }
    }

    pub fn as_c_str(&self) -> &'a CStr {
        unsafe {
            let mut len = 0usize;
            let c_ptr = c::edn_string_get(self.inner.as_ptr(), &raw mut len) as *const u8;
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
            let c_ptr = c::edn_string_get(self.inner.as_ptr(), &raw mut len) as *const u8;
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

impl fmt::Display for Edn<'_, kinds::String> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Keywords

impl<'a> Edn<'a, kinds::Keyword> {
    fn as_namespaced_str(&self) -> NamespacedStr<'a> {
        unsafe {
            let mut namespace_ptr: *const c_char = std::ptr::null();
            let mut namespace_size = 0usize;
            let mut name_ptr: *const c_char = std::ptr::null();
            let mut name_size = 0usize;
            let ok = c::edn_keyword_get(
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

impl fmt::Display for Edn<'_, kinds::Keyword> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ns = self.as_namespaced_str();
        match ns.namespace {
            None => write!(f, ":{}", ns.name),
            Some(space) => write!(f, ":{}/{}", space, ns.name),
        }
    }
}

/// Sets

impl<'a> Edn<'a, kinds::Set> {
    fn len(&self) -> usize {
        unsafe { c::edn_set_count(self.as_ptr()) }
    }

    fn contains<U>(&self, val: Edn<'a, U>) -> bool {
        unsafe { c::edn_set_contains(self.as_ptr(), val.as_ptr()) }
    }

    fn iter(&self) -> impl Iterator<Item = Edn<'a, ()>> {
        EdnIterator {
            value: self.clone(),
            index: 0,
            max: self.len(),
        }
    }
}

impl<'a> Iterator for EdnIterator<'a, kinds::Set> {
    type Item = Edn<'a, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = Edn::from_raw(unsafe { c::edn_set_get(self.value.as_ptr(), self.index) });
        if val.is_some() {
            self.index += 1;
        }
        val
    }
}

/// Maps

impl<'a> Edn<'a, kinds::Map> {
    pub fn contains<U>(&self, val: Edn<'a, U>) -> bool {
        unsafe { c::edn_map_contains_key(self.as_ptr(), val.as_ptr()) }
    }

    pub fn get<U>(&self, key: Edn<'a, U>) -> Option<Edn<'a, ()>> {
        Edn::from_raw(unsafe { c::edn_map_lookup(self.as_ptr(), key.as_ptr()) })
    }

    pub fn get_by_string(&self, string: &str) -> Option<Edn<'a, ()>> {
        self.get_by_cstr(&CString::new(string).ok()?)
    }

    pub fn get_by_cstr(&self, string: &CStr) -> Option<Edn<'a, ()>> {
        Edn::from_raw(unsafe { c::edn_map_get_string_key(self.as_ptr(), string.as_ptr()) })
    }

    pub fn get_by_keyword(&self, keyword: &str) -> Option<Edn<'a, ()>> {
        self.get_by_keyword_cstr(&CString::new(keyword).ok()?)
    }

    pub fn get_by_keyword_cstr(&self, keyword: &CStr) -> Option<Edn<'a, ()>> {
        Edn::from_raw(unsafe { c::edn_map_get_keyword(self.as_ptr(), keyword.as_ptr()) })
    }

    pub fn get_by_namespaced_keyword(&self, namespace: &str, name: &str) -> Option<Edn<'a, ()>> {
        self.get_by_namespaced_keyword_cstr(
            &CString::new(namespace).ok()?,
            &CString::new(name).ok()?,
        )
    }

    pub fn get_by_namespaced_keyword_cstr(
        &self,
        namespace: &CStr,
        name: &CStr,
    ) -> Option<Edn<'a, ()>> {
        Edn::from_raw(unsafe {
            c::edn_map_get_namespaced_keyword(self.as_ptr(), namespace.as_ptr(), name.as_ptr())
        })
    }
}

/// Tagged literals
impl<'a> Edn<'a, kinds::Tagged> {
    pub fn get(&self) -> (&'a str, Edn<'a, ()>) {
        unsafe {
            let mut tag_ptr: *const c_char = null();
            let mut tag_length = 0usize;
            let mut value_ptr: *mut c::edn_value_t = null_mut();
            let ok = c::edn_tagged_get(
                self.as_ptr(),
                &raw mut tag_ptr,
                &raw mut tag_length,
                &raw mut value_ptr,
            );
            if !ok {
                panic!("Invariant violated: edn_tagged_get failed")
            }
            let tag_slice = std::slice::from_raw_parts(tag_ptr as *const u8, tag_length);
            let tag = str::from_utf8_unchecked(tag_slice);
            let value = Edn::from_raw_unchecked(value_ptr);
            (tag, value)
        }
    }
}

/// Vectors

impl<'a> Edn<'a, kinds::Vector> {
    pub fn len(&self) -> usize {
        unsafe { c::edn_vector_count(self.as_ptr()) }
    }

    pub fn get(&self, idx: usize) -> Option<Edn<'a, ()>> {
        Edn::from_raw(unsafe { c::edn_vector_get(self.as_ptr(), idx) })
    }

    pub fn iter(&self) -> impl Iterator<Item = Edn<'a, ()>> {
        EdnIterator {
            value: self.clone(),
            index: 0,
            max: self.len(),
        }
    }
}

impl<'a> Iterator for EdnIterator<'a, kinds::Vector> {
    type Item = Edn<'a, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = Edn::from_raw(unsafe { c::edn_vector_get(self.value.as_ptr(), self.index) });
        if val.is_some() {
            self.index += 1;
        }
        val
    }
}

/// Lists

impl<'a> Edn<'a, kinds::List> {
    pub fn len(&self) -> usize {
        unsafe { c::edn_list_count(self.as_ptr()) }
    }

    pub fn get(&self, idx: usize) -> Option<Edn<'a, ()>> {
        Edn::from_raw(unsafe { c::edn_list_get(self.as_ptr(), idx) })
    }

    pub fn iter(&self) -> impl Iterator<Item = Edn<'a, ()>> {
        EdnIterator {
            value: self.clone(),
            index: 0,
            max: self.len(),
        }
    }
}

impl<'a> Iterator for EdnIterator<'a, kinds::List> {
    type Item = Edn<'a, ()>;

    fn next(&mut self) -> Option<Self::Item> {
        let val = Edn::from_raw(unsafe { c::edn_list_get(self.value.as_ptr(), self.index) });
        if val.is_some() {
            self.index += 1;
        }
        val
    }
}

/// Symbols

impl<'a> Edn<'a, kinds::Symbol> {
    fn as_namespaced_str(&self) -> NamespacedStr<'a> {
        unsafe {
            let mut namespace_ptr: *const c_char = std::ptr::null();
            let mut namespace_size = 0usize;
            let mut name_ptr: *const c_char = std::ptr::null();
            let mut name_size = 0usize;
            let ok = c::edn_symbol_get(
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

impl Into<c::edn_default_reader_mode_t> for DefaultReaderMode {
    fn into(self) -> c::edn_default_reader_mode_t {
        use c::edn_default_reader_mode_t::*;
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
            let registry = c::edn_reader_registry_create();
            if registry.is_null() {
                panic!("Out of memory in ReaderRegistry::new")
            }
            ReaderRegistry(NonNull::new_unchecked(registry))
        }
    }

    pub fn register<'a, F>(&self, _tag: &str, _reader: F)
    where
        F: Fn(Root, &'a Arena, &'a str) -> (),
    {
        todo!()
    }
}

impl Drop for ReaderRegistry {
    fn drop(&mut self) {
        unsafe { c::edn_reader_registry_destroy(self.0.as_ptr()) }
    }
}

impl ParseOptions {
    fn as_raw(&self) -> c::edn_parse_options_t {
        c::edn_parse_options_t {
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

impl From<c::edn_error_t> for EdnErrorKind {
    fn from(error: c::edn_error_t) -> Self {
        use c::edn_error_t::*;
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
