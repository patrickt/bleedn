use std::{
    borrow::Borrow,
    ffi::{c_char, CStr, CString},
    fmt,
    marker::PhantomData,
    ops::Deref,
    ptr::{null, null_mut, NonNull},
};

/// Rather than providing a discriminated enum of all the cases of an EDN value,
/// this library uses phantom types and safe casts to refine values to more specific
/// types.
pub mod kinds {
    pub trait EdnTag {
        fn tag_of() -> edn_type_t;
    }

    use crate::c::edn_type_t;

    macro_rules! impl_edn_tag {
        ($($kind:ident => $tag:ident),* $(,)?) => {
            $(
                pub struct $kind;
                impl EdnTag for $kind {
                    fn tag_of() -> edn_type_t {
                        edn_type_t::$tag
                    }
                }
            )*
        };
    }

    impl_edn_tag! {
        BigDec  => EDN_TYPE_BIGDEC,
        BigInt  => EDN_TYPE_BIGINT,
        Bool    => EDN_TYPE_BOOL,
        Char    => EDN_TYPE_CHARACTER,
        Float   => EDN_TYPE_FLOAT,
        Int64   => EDN_TYPE_INT,
        Keyword => EDN_TYPE_KEYWORD,
        List    => EDN_TYPE_LIST,
        Map     => EDN_TYPE_MAP,
        Nil     => EDN_TYPE_NIL,
        Ratio   => EDN_TYPE_RATIO,
        Set     => EDN_TYPE_SET,
        String  => EDN_TYPE_STRING,
        Symbol  => EDN_TYPE_SYMBOL,
        Tagged  => EDN_TYPE_TAGGED,
        Vector  => EDN_TYPE_VECTOR,
    }
}

/// Generated C bindings for the edn.c library.
pub mod c {
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

/// A reference to an EDN entity within a document.
/// This is a wrapper around a pointer, so it is cheap to clone.
#[derive(Debug)]
pub struct Edn<'a, T = ()> {
    inner: NonNull<c::edn_value_t>,
    _phantom: PhantomData<&'a T>,
}

/// An EDN document. Use its AsRef implementation to get an `Edn` node.
#[repr(transparent)]
#[derive(Debug)]
pub struct Doc(Edn<'static, ()>);

/// A native representation of EDN bignums. Should be suitable for consumption
/// by whatever arbitrary-precision library you might be using.
pub struct Bignum<'a> {
    pub value: &'a str,
    pub negative: bool,
    pub radix: Option<u8>,
}

/// A string slice with an optional namespace. Used for keywords and symbols.
pub struct NamespacedStr<'a> {
    pub namespace: Option<&'a str>,
    pub name: &'a str,
}

struct EdnIterator<'a, T> {
    value: Edn<'a, T>,
    index: usize,
    max: usize,
}

impl Doc {
    /// Parse EDN from a string slice.
    ///
    /// # Example
    /// ```
    /// let root = bleedn::Doc::parse("[1 2 3]").unwrap();
    /// ```
    pub fn parse(input: impl AsRef<str>) -> Result<Self, ParseError> {
        let input_str = input.as_ref();
        unsafe {
            let c_result = c::edn_read(input_str.as_ptr() as *const c_char, input_str.len());
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

    pub fn cast<U>(&'a self) -> Option<Edn<'a, U>>
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

    /// Is this node of the specified type (as determined with the turbofish operator)?
    pub fn has_tag<U>(&self) -> bool
    where
        U: kinds::EdnTag,
    {
        unsafe { c::edn_type(self.as_ptr()) == U::tag_of() }
    }

    /// Is this node nil?
    pub fn is_nil(&self) -> bool {
        unsafe { c::edn_is_nil(self.as_ptr()) }
    }

    /// Is this node a string?
    pub fn is_string(&self) -> bool {
        unsafe { c::edn_is_string(self.as_ptr()) }
    }

    /// Is this node a number (int64, bignum, bigdec, or ratio)?
    pub fn is_number(&self) -> bool {
        unsafe { c::edn_is_number(self.as_ptr()) }
    }

    /// Is this node an int64?
    pub fn is_integer(&self) -> bool {
        unsafe { c::edn_is_integer(self.as_ptr()) }
    }

    /// Is this node a collection (list, map, vector, or set)?
    pub fn is_collection(&self) -> bool {
        unsafe { c::edn_is_collection(self.as_ptr()) }
    }

    /// Return the metadata associated with this node, or None.
    pub fn metadata(&self) -> Option<Edn<'a, kinds::Map>> {
        Edn::from_raw(unsafe { c::edn_value_meta(self.as_ptr()) })
    }

    /// Returns true if the node has metadata. Equivalent to `self.metadata.is_some()`, but
    /// concievably faster.
    pub fn has_metadata(&self) -> bool {
        unsafe { c::edn_value_has_meta(self.as_ptr()) }
    }

    // TODO: why can't this be a TryInto implementation without conflicting
    // with the Into declaration for Edn<'a, Double>?
    /// Convert a numeric type (int64, bignum, float, or ratio)
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

impl<'a, T> Clone for Edn<'a, T> {
    fn clone(&self) -> Self {
        Edn {
            inner: self.inner.clone(),
            _phantom: PhantomData,
        }
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
            assert!(ok, "Invariant violated: edn_bool_get failed");
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
            assert!(ok, "Invariant violated: edn_int64_get failed");
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
            );
            if val_ptr.is_null() {
                panic!("Invariant violated: edn_bigint_getfailed")
            }
            let slice = std::slice::from_raw_parts(val_ptr as *const u8, length);
            Bignum {
                value: str::from_utf8_unchecked(slice),
                negative,
                radix: Some(radix),
            }
        }
    }
}

/// BigDecs

impl<'a> Edn<'a, kinds::BigDec> {
    pub fn to_bignum(&self) -> Bignum<'a> {
        unsafe {
            let mut length = 0usize;
            let mut negative = false;
            let val_ptr = c::edn_bigdec_get(self.as_ptr(), &raw mut length, &raw mut negative);
            if val_ptr.is_null() {
                panic!("Invariant violated: edn_bigdec_get failed")
            }
            let slice = std::slice::from_raw_parts(val_ptr as *const u8, length);
            Bignum {
                value: str::from_utf8_unchecked(slice),
                negative,
                radix: None,
            }
        }
    }
}

/// Floats

impl<'a> Into<f64> for Edn<'a, kinds::Float> {
    fn into(self) -> f64 {
        unsafe {
            let mut val = 0f64;
            let ok = c::edn_double_get(self.as_ptr(), &raw mut val);
            assert!(ok, "Invariant violated: edn_double_get failed");
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
            assert!(ok, "Invariant violated: edn_ratio_get failed");
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
            assert!(ok, "Invariant violated: edn_character_get failed");
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
            let c_ptr = c::edn_string_get(self.inner.as_ptr(), &raw mut len);
            if c_ptr.is_null() {
                panic!("Invariant violated: NULL string inside an Edn<String>")
            }
            // len is the string length WITHOUT null terminator, so we need len+1 for CStr
            let slice = std::slice::from_raw_parts(c_ptr as *const u8, len + 1);
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
            let c_ptr = c::edn_string_get(self.inner.as_ptr(), &raw mut len);
            if c_ptr.is_null() {
                panic!("Invariant violated: NULL string inside an Edn<String>")
            }
            // len is the string length WITHOUT null terminator, so we need len+1 for CStr
            let slice = std::slice::from_raw_parts(c_ptr as *const u8, len + 1);
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
            assert!(ok, "Invariant violated: bad keyword in Edn<Keyword>");
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
    pub fn len(&self) -> usize {
        unsafe { c::edn_set_count(self.as_ptr()) }
    }

    pub fn contains<U>(&self, val: Edn<'a, U>) -> bool {
        unsafe { c::edn_set_contains(self.as_ptr(), val.as_ptr()) }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = Edn<'a, ()>> {
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
            assert!(ok, "Invariant violated: edn_tagged_get failed");
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
    pub fn as_namespaced_str(&self) -> NamespacedStr<'a> {
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
            assert!(ok, "Invariant violated: bad keyword in Edn<Keyword>");
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

    #[test]
    fn test_parse_nil() {
        let root = Doc::parse("nil").unwrap();
        assert!(root.is_nil());
    }

    #[test]
    fn test_parse_string() {
        let input = r#""hello world""#;
        let root = Doc::parse(input).unwrap();
        assert!(root.is_string());

        let edn_string = root.cast::<kinds::String>().unwrap();
        assert_eq!(edn_string.as_str(), "hello world");
    }

    #[test]
    fn test_parse_integer() {
        let input = "42";
        let root = Doc::parse(input).unwrap();
        assert!(root.is_integer());

        let edn_int = root.cast::<kinds::Int64>().unwrap();
        let val: i64 = edn_int.into();
        assert_eq!(val, 42);
    }

    #[test]
    fn test_parse_float() {
        let input = "3.14";
        let root = Doc::parse(input).unwrap();
        assert!(root.is_number());

        assert_eq!(root.as_f64(), Some(3.14));
    }

    #[test]
    fn test_parse_boolean() {
        let input = "true";
        let root = Doc::parse(input).unwrap();

        let edn_bool = root.cast::<kinds::Bool>().unwrap();
        let val: bool = edn_bool.into();
        assert_eq!(val, true);
    }

    #[test]
    fn test_parse_vector() {
        let input = "[1 2 3]";
        let root = Doc::parse(input).unwrap();
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
        let input = "(1 2 3)";
        let root = Doc::parse(input).unwrap();

        let list = root.cast::<kinds::List>().unwrap();
        assert_eq!(list.len(), 3);
    }

    #[test]
    fn test_parse_map() {
        let input = r#"{:name "Alice" :age 30}"#;
        let root = Doc::parse(input).unwrap();

        let map = root.cast::<kinds::Map>().unwrap();

        let name = map.get_by_keyword("name").unwrap();
        let name_str = name.cast::<kinds::String>().unwrap();
        assert_eq!(name_str.as_str(), "Alice");
    }

    #[test]
    fn test_parse_set() {
        let input = "#{1 2 3}";
        let root = Doc::parse(input).unwrap();

        let set = root.cast::<kinds::Set>().unwrap();
        assert_eq!(set.len(), 3);
    }

    #[test]
    fn test_vector_iterator() {
        let input = "[1 2 3 4 5]";
        let root = Doc::parse(input).unwrap();

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
        let input = "[1 2 3]";
        let root = Doc::parse(input).unwrap();

        let vec = root.cast::<kinds::Vector>().unwrap();
        let iter = vec.iter();

        assert_eq!(iter.len(), 3);
    }

    #[test]
    fn test_keyword() {
        let input = ":hello";
        let root = Doc::parse(input).unwrap();

        let keyword = root.cast::<kinds::Keyword>().unwrap();
        assert_eq!(format!("{}", keyword), ":hello");
    }

    #[test]
    fn test_namespaced_keyword() {
        let input = ":ns/name";
        let root = Doc::parse(input).unwrap();

        let keyword = root.cast::<kinds::Keyword>().unwrap();
        assert_eq!(format!("{}", keyword), ":ns/name");
    }

    #[test]
    fn test_parse_error() {
        let input = "[1 2";
        let result = Doc::parse(input);

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert_eq!(err.kind, EdnError::UnexpectedEof);
    }

    #[test]
    fn test_deref_coercion() {
        let input = r#""test""#;
        let root = Doc::parse(input).unwrap();

        // Should be able to call Edn methods directly on Doc via Deref
        assert!(root.is_string());
        let str_val = root.cast::<kinds::String>().unwrap();
        assert_eq!(str_val.as_str(), "test");
    }

    #[test]
    fn test_borrow_trait() {
        use std::borrow::Borrow;

        let input = "42";
        let root = Doc::parse(input).unwrap();

        let borrowed: &Edn<'static, ()> = root.borrow();
        assert!(borrowed.is_integer());
    }

    #[test]
    fn test_as_ref_trait() {
        let input = "42";
        let root = Doc::parse(input).unwrap();

        let referenced: &Edn<'static, ()> = root.as_ref();
        assert!(referenced.is_integer());
    }

    #[test]
    fn test_ratio() {
        let input = "22/7";
        let root = Doc::parse(input).unwrap();

        let ratio = root.cast::<kinds::Ratio>().unwrap();
        let (num, denom) = ratio.get();
        assert_eq!(num, 22);
        assert_eq!(denom, 7);
    }

    #[test]
    fn test_character() {
        let input = r#"\a"#;
        let root = Doc::parse(input).unwrap();

        let ch = root.cast::<kinds::Char>().unwrap();
        let c: char = ch.into();
        assert_eq!(c, 'a');
    }

    #[test]
    fn test_nested_structures() {
        let input = r#"[{:name "Alice"} {:name "Bob"}]"#;
        let root = Doc::parse(input).unwrap();

        let vec = root.cast::<kinds::Vector>().unwrap();
        assert_eq!(vec.len(), 2);

        let first = vec.get(0).unwrap();
        let first_map = first.cast::<kinds::Map>().unwrap();

        let name = first_map.get_by_keyword("name").unwrap();
        let name_str = name.cast::<kinds::String>().unwrap();
        assert_eq!(name_str.as_str(), "Alice");
    }

    #[test]
    fn test_bigdec() {
        let input = "123.456M";
        let root = Doc::parse(input).unwrap();

        let bigdec_edn = root.cast::<kinds::BigDec>().unwrap();
        let bignum = bigdec_edn.to_bignum();
        assert_eq!(bignum.value, "123.456");
        assert_eq!(bignum.negative, false);
        assert_eq!(bignum.radix, None);

        // Test negative BigDec
        let neg_input = "-99.99M";
        let neg_root = Doc::parse(neg_input).unwrap();
        let neg_bigdec_edn = neg_root.cast::<kinds::BigDec>().unwrap();
        let neg_bignum = neg_bigdec_edn.to_bignum();
        assert_eq!(neg_bignum.value, "99.99");
        assert_eq!(neg_bignum.negative, true);
        assert_eq!(neg_bignum.radix, None);
    }
}
