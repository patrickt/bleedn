#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]

use std::{
    borrow::Borrow,
    ffi::{c_char, CStr, CString},
    fmt,
    marker::PhantomData,
    ops::Deref,
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

#[derive(Debug)]
pub struct Edn<'a, T = ()> {
    inner: NonNull<c::edn_value_t>,
    _phantom: PhantomData<&'a T>,
}

#[repr(transparent)]
#[derive(Debug)]
pub struct Doc(Edn<'static, ()>);

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
    eof_value: Doc,
    default_reader_mode: DefaultReaderMode,
}

pub struct Arena(NonNull<c::edn_arena_t>);

struct EdnIterator<'a, T> {
    value: Edn<'a, T>,
    index: usize,
    max: usize,
}

impl Doc {
    /// Parse EDN from a Rust string slice.
    ///
    /// This is the most convenient parsing method for Rust strings.
    ///
    /// # Example
    /// ```
    /// # use bleedn::Root;
    /// let root = Root::parse("[1 2 3]").unwrap();
    /// ```
    pub fn parse(input: impl AsRef<str>) -> Result<Self, ParseError> {
        let c_string = CString::new(input.as_ref()).map_err(|_| ParseError {
            kind: EdnError::InvalidUtf8,
            line: 0,
            column: 0,
            message: "Input contains null bytes".to_string(),
        })?;
        Self::parse_cstr(&c_string)
    }

    /// Parse EDN from a C string.
    ///
    /// Use this if you already have a `CStr` to avoid an extra allocation.
    pub fn parse_cstr(input: &CStr) -> Result<Self, ParseError> {
        unsafe {
            let c_result = c::edn_read(input.as_ptr(), input.count_bytes());
            if c_result.error == c::edn_error_t::EDN_OK {
                Ok(Doc(Edn {
                    inner: NonNull::new_unchecked(c_result.value),
                    _phantom: PhantomData,
                }))
            } else {
                Err(ParseError {
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

    pub fn parse_cstr_with_options(
        input: &CStr,
        options: &ParseOptions,
    ) -> Result<Self, ParseError> {
        unsafe {
            let mut c_options = options.as_raw();
            let c_result =
                c::edn_read_with_options(input.as_ptr(), input.count_bytes(), &raw mut c_options);
            if c_result.error == c::edn_error_t::EDN_OK {
                Ok(Doc(Edn {
                    inner: NonNull::new_unchecked(c_result.value),
                    _phantom: PhantomData,
                }))
            } else {
                Err(ParseError {
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
}

impl Drop for Doc {
    fn drop(&mut self) {
        unsafe { c::edn_free(self.0.inner.as_ptr()) }
    }
}

impl Deref for Doc {
    type Target = Edn<'static, ()>;

    fn deref(&self) -> &Self::Target {
        // Safety: Root is repr(transparent) over Edn<'static, ()>
        &self.0
    }
}

impl Borrow<Edn<'static, ()>> for Doc {
    fn borrow(&self) -> &Edn<'static, ()> {
        self.deref()
    }
}

impl AsRef<Edn<'static, ()>> for Doc {
    fn as_ref(&self) -> &Edn<'static, ()> {
        self.deref()
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
            // len is the string length WITHOUT null terminator, so we need len+1 for CStr
            let slice = std::slice::from_raw_parts(c_ptr, len + 1);
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
            // len is the string length WITHOUT null terminator, so we need len+1 for CStr
            let slice = std::slice::from_raw_parts(c_ptr, len + 1);
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

    fn iter(&self) -> impl ExactSizeIterator<Item = Edn<'a, ()>> {
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

impl<'a> ExactSizeIterator for EdnIterator<'a, kinds::Set> {
    fn len(&self) -> usize {
        self.max - self.index
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

    pub fn iter(&self) -> impl ExactSizeIterator<Item = Edn<'a, ()>> {
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

impl<'a> ExactSizeIterator for EdnIterator<'a, kinds::Vector> {
    fn len(&self) -> usize {
        self.max - self.index
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

    pub fn iter(&self) -> impl ExactSizeIterator<Item = Edn<'a, ()>> {
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

impl<'a> ExactSizeIterator for EdnIterator<'a, kinds::List> {
    fn len(&self) -> usize {
        self.max - self.index
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
                // namespace_len doesn't include null terminator, so we need namespace_len+1
                let namespace_slice =
                    std::slice::from_raw_parts(namespace_ptr as *const u8, namespace_len + 1);
                namespace = Some(
                    CStr::from_bytes_with_nul_unchecked(namespace_slice)
                        .to_str()
                        .expect("Invariant violated: bad UTF-8"),
                );
            }
            // name_len doesn't include null terminator, so we need name_len+1
            let name_slice = std::slice::from_raw_parts(name_ptr as *const u8, name_len + 1);
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
        F: Fn(Doc, &'a Arena, &'a str) -> (),
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
            eof_value: self.eof_value.0.inner.as_ptr(),
            default_reader_mode: self.default_reader_mode.clone().into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EdnError {
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
pub struct ParseError {
    pub kind: EdnError,
    pub line: usize,
    pub column: usize,
    pub message: String,
}

impl From<c::edn_error_t> for EdnError {
    fn from(error: c::edn_error_t) -> Self {
        use c::edn_error_t::*;
        match error {
            EDN_OK => unreachable!("EDN_OK should not be converted to EdnErrorKind"),
            EDN_ERROR_INVALID_SYNTAX => EdnError::InvalidSyntax,
            EDN_ERROR_UNEXPECTED_EOF => EdnError::UnexpectedEof,
            EDN_ERROR_OUT_OF_MEMORY => EdnError::OutOfMemory,
            EDN_ERROR_INVALID_UTF8 => EdnError::InvalidUtf8,
            EDN_ERROR_INVALID_NUMBER => EdnError::InvalidNumber,
            EDN_ERROR_INVALID_STRING => EdnError::InvalidString,
            EDN_ERROR_INVALID_ESCAPE => EdnError::InvalidEscape,
            EDN_ERROR_UNMATCHED_DELIMITER => EdnError::UnmatchedDelimiter,
            EDN_ERROR_UNKNOWN_TAG => EdnError::UnknownTag,
            EDN_ERROR_DUPLICATE_KEY => EdnError::DuplicateKey,
            EDN_ERROR_DUPLICATE_ELEMENT => EdnError::DuplicateElement,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_parse_nil() {
        let root = Doc::parse("nil").unwrap();
        assert!(root.is_nil());
    }

    #[test]
    fn test_parse_with_null_bytes() {
        let result = Doc::parse("hello\0world");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.kind, EdnError::InvalidUtf8);
    }

    #[test]
    fn test_parse_string() {
        let input = CString::new(r#""hello world""#).unwrap();
        let root = Doc::parse_cstr(&input).unwrap();
        assert!(root.is_string());

        let edn_string = root.cast::<kinds::String>().unwrap();
        assert_eq!(edn_string.as_str(), "hello world");
    }

    #[test]
    fn test_parse_integer() {
        let input = CString::new("42").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();
        assert!(root.is_integer());

        let edn_int = root.cast::<kinds::Int64>().unwrap();
        let val: i64 = edn_int.into();
        assert_eq!(val, 42);
    }

    #[test]
    fn test_parse_float() {
        let input = CString::new("3.14").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();
        assert!(root.is_number());

        assert_eq!(root.as_f64(), Some(3.14));
    }

    #[test]
    fn test_parse_boolean() {
        let input = CString::new("true").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let edn_bool = root.cast::<kinds::Bool>().unwrap();
        let val: bool = edn_bool.into();
        assert_eq!(val, true);
    }

    #[test]
    fn test_parse_vector() {
        let input = CString::new("[1 2 3]").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();
        assert!(root.is_collection());

        let vec = root.cast::<kinds::Vector>().unwrap();
        assert_eq!(vec.len(), 3);

        let first = vec.get(0).unwrap();
        let first_int = first.cast::<kinds::Int64>().unwrap();
        let val: i64 = first_int.into();
        assert_eq!(val, 1);
    }

    #[test]
    fn test_parse_list() {
        let input = CString::new("(1 2 3)").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let list = root.cast::<kinds::List>().unwrap();
        assert_eq!(list.len(), 3);
    }

    #[test]
    fn test_parse_map() {
        let input = CString::new(r#"{:name "Alice" :age 30}"#).unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let map = root.cast::<kinds::Map>().unwrap();

        let name = map.get_by_keyword("name").unwrap();
        let name_str = name.cast::<kinds::String>().unwrap();
        assert_eq!(name_str.as_str(), "Alice");
    }

    #[test]
    fn test_parse_set() {
        let input = CString::new("#{1 2 3}").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let set = root.cast::<kinds::Set>().unwrap();
        assert_eq!(set.len(), 3);
    }

    #[test]
    fn test_vector_iterator() {
        let input = CString::new("[1 2 3 4 5]").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let vec = root.cast::<kinds::Vector>().unwrap();
        let mut sum = 0i64;

        for item in vec.iter() {
            if let Some(int_val) = item.cast::<kinds::Int64>() {
                sum += Into::<i64>::into(int_val);
            }
        }

        assert_eq!(sum, 15);
    }

    #[test]
    fn test_exact_size_iterator() {
        let input = CString::new("[1 2 3]").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let vec = root.cast::<kinds::Vector>().unwrap();
        let iter = vec.iter();

        assert_eq!(iter.len(), 3);
    }

    #[test]
    fn test_keyword() {
        let input = CString::new(":hello").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let keyword = root.cast::<kinds::Keyword>().unwrap();
        assert_eq!(format!("{}", keyword), ":hello");
    }

    #[test]
    fn test_namespaced_keyword() {
        let input = CString::new(":ns/name").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let keyword = root.cast::<kinds::Keyword>().unwrap();
        assert_eq!(format!("{}", keyword), ":ns/name");
    }

    #[test]
    fn test_parse_error() {
        let input = CString::new("[1 2").unwrap();
        let result = Doc::parse_cstr(&input);

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.kind, EdnError::UnexpectedEof);
    }

    #[test]
    fn test_deref_coercion() {
        let input = CString::new(r#""test""#).unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        // Should be able to call Edn methods directly on Root via Deref
        assert!(root.is_string());
        let str_val = root.cast::<kinds::String>().unwrap();
        assert_eq!(str_val.as_str(), "test");
    }

    #[test]
    fn test_borrow_trait() {
        use std::borrow::Borrow;

        let input = CString::new("42").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let borrowed: &Edn<'static, ()> = root.borrow();
        assert!(borrowed.is_integer());
    }

    #[test]
    fn test_as_ref_trait() {
        let input = CString::new("42").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let referenced: &Edn<'static, ()> = root.as_ref();
        assert!(referenced.is_integer());
    }

    #[test]
    fn test_ratio() {
        let input = CString::new("22/7").unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let ratio = root.cast::<kinds::Ratio>().unwrap();
        let (num, denom) = ratio.get();
        assert_eq!(num, 22);
        assert_eq!(denom, 7);
    }

    #[test]
    fn test_character() {
        let input = CString::new(r#"\a"#).unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let ch = root.cast::<kinds::Char>().unwrap();
        let c: char = ch.into();
        assert_eq!(c, 'a');
    }

    #[test]
    fn test_nested_structures() {
        let input = CString::new(r#"[{:name "Alice"} {:name "Bob"}]"#).unwrap();
        let root = Doc::parse_cstr(&input).unwrap();

        let vec = root.cast::<kinds::Vector>().unwrap();
        assert_eq!(vec.len(), 2);

        let first = vec.get(0).unwrap();
        let first_map = first.cast::<kinds::Map>().unwrap();

        let name = first_map.get_by_keyword("name").unwrap();
        let name_str = name.cast::<kinds::String>().unwrap();
        assert_eq!(name_str.as_str(), "Alice");
    }
}
