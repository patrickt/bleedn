#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

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
}

// Include the generated bindings
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub struct Value(Edn<()>);

impl Value {
    pub fn parse(_input: &str) -> Result<Self, EdnError> {
        todo!()
    }

    pub fn edn_string<'a>(&'a self) -> Option<&'a Edn<kinds::String>> {
        todo!()
    }

    pub fn edn_vector<'a>(&'a self) -> Option<&'a Edn<kinds::Vector>> {
        todo!()
    }

    pub fn edn_double<'a>(&'a self) -> Option<&'a Edn<kinds::Double>> {
        todo!()
    }

    pub fn edn_bool<'a>(&'a self) -> Option<&'a Edn<kinds::Bool>> {
        todo!()
    }

    pub fn is_nil(&self) -> bool {
        self.0.is_nil()
    }

    pub fn is_string(&self) -> bool {
        self.0.is_string()
    }
}

impl Drop for Value {
    fn drop(&mut self) {
        unsafe { edn_free(self.0.inner.as_ptr()) }
    }
}

// todo: I think this has to be Edn<'a, T>
pub struct Edn<T> {
    inner: NonNull<edn_value_t>,
    _phantom: PhantomData<T>,
}

impl<T> Edn<T> {
    pub unsafe fn downcast_unchecked<U>(&self) -> Edn<U> {
        Edn {
            inner: self.inner,
            _phantom: PhantomData,
        }
    }

    pub fn is_nil(&self) -> bool {
        todo!()
    }

    pub fn is_string(&self) -> bool {
        todo!()
    }

    pub fn is_number(&self) -> bool {
        todo!()
    }

    pub fn is_integer(&self) -> bool {
        todo!()
    }

    pub fn is_collection(&self) -> bool {
        todo!()
    }

    pub fn as_f64(&self) -> Option<f64> {
        unsafe {
            let mut value = 0f64;
            let ok = edn_number_as_double(self.inner.as_ptr(), &value);
            if ok {
                Some(value)
            } else {
                None
            }
        }
    }
}

impl Edn<kinds::String> {
    pub fn compare(&self, val: &str) -> Ordering {
        todo!()
    }

    pub fn as_c_str<'a>(&'a self) -> &'a CStr {
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

    pub fn as_str<'a>(&'a self) -> &'a str {
        self.as_c_str()
            .to_str()
            .expect("Invariant violated: bad UTF-8 in Edn<String>")
    }
}

impl Edn<kinds::Vector> {
    pub fn get<'a>(&'a self, _idx: usize) -> Option<&'a Edn<()>> {
        todo!()
    }
}

impl Into<f64> for Edn<kinds::Double> {
    fn into(self) -> f64 {
        unsafe {
            let mut val = 0f64;
            let ok = edn_double_get(self.inner.as_ptr(), &raw mut val);
            if !ok {
                panic!("Invariant violated: bad f64 in Edn<Double>")
            }
            val
        }
    }
}

impl Into<i64> for Edn<kinds::Int64> {
    fn into(self) -> i64 {
        unsafe {
            let mut val = 0i64;
            let ok = edn_int64_get(self.inner.as_ptr(), &raw mut val);
            if !ok {
                panic!("Invariant violated: bad f64 in Edn<Double>")
            }
            val
        }
    }
}

impl Edn<kinds::BigInt> {
    pub fn digit_string<'a>(&'a self) -> &'a str {
        unsafe {
            let mut len = 0usize;
            let c_ptr = edn_bigint_get(self.inner.as_ptr(), &raw mut len, null, null) as *const u8;
            if c_ptr.is_null() {
                panic!("Invariant violated: NULL string inside an Edn<String>")
            }
            let slice = std::slice::from_raw_parts(c_ptr, len);
            CStr::from_bytes_with_nul_unchecked(slice)
        }
    }
}

impl Edn<kinds::BigInt> {
    pub fn decimal_string<'a>(&'a self) -> &'a str {
        unsafe {
            let mut len = 0usize;
            let c_ptr = edn_bigdec_get(self.inner.as_ptr(), &raw mut len, null) as *const u8;
            if c_ptr.is_null() {
                panic!("Invariant violated: NULL string inside an Edn<String>")
            }
            let slice = std::slice::from_raw_parts(c_ptr, len);
            CStr::from_bytes_with_nul_unchecked(slice)
        }
    }
}

pub struct NamespacedStr<'a> {
    namespace: Option<&'a str>,
    name: &'a str,
}

impl Edn<kinds::Keyword> {
    fn keyword<'a>(&'a self) -> NamespacedStr<'a> {
        unsafe {
            let mut namespace_ptr: *const c_char = null();
            let mut name_ptr: *const c_char = null();
            let mut namespace_len = 0usize;
            let mut name_len = 0usize;
            let ok = edn_keyword_get(
                self.inner.as_ptr(),
                &raw mut namespace_ptr,
                &raw mut namespace_len,
                &raw mut name_ptr,
                &raw mut name_len,
            );
            if !ok {
                panic!("Invariant violated: bad keyword data in Edn<Keyword>")
            }
            let mut namespace: Option<&str> = None;
            if !namespace_ptr.is_null() {
                let slice = std::slice::from_raw_parts(namespace_ptr as *const u8, namespace_len);
                ns_str = Some(
                    CStr::from_bytes_with_nul_unchecked(slice)
                        .to_str()
                        .expect("Invariant violated: bad UTF-8"),
                );
            }
            let slice = std::slice::from_raw_parts(name_ptr as *const u8, name_len);
            let name = CStr::from_bytes_with_nul_unchecked(slice)
                .to_str()
                .expect("Invariant violated: bad UTF-8");
            NamespacedStr { namespace, name }
        }
    }
}

impl Edn<kinds::Map> {
    pub fn len(&self) -> usize {
        todo!()
    }

    pub fn contains_key<U>(&self, _val: Edn<U>) -> bool {
        todo!()
    }

    pub fn lookup<U>(&'a self, _key: Edn<U>) -> Option<>
}

impl Edn<kinds::Set> {
    pub fn len(&self) -> usize {
        todo!()
    }

    pub fn contains<U>(&self, item: Edn<U>) -> bool {
        todo!()
    }

    pub fn iter(&self) -> impl Iterator<Item = Edn<()>> {
        todo!()
    }
}

impl Edn<kinds::Symbol> {
    pub fn symbol<'a>(&'a self) -> NamespacedStr<'a> {
        todo!()
    }
}

// impl Into<(i64, i64)> for Edn<kinds::Ratio> {
//     fn into(self) -> (i64, i64) {
//         unsafe {
//             let mut numerator = 0i64;
//             let mut denominator = 0i64;
//             let ok = edn_ratio_get(
//                 self.inner.as_ptr(),
//                 &raw mut numerator,
//                 &raw mut denominator,
//             );
//             if !ok {
//                 panic!("Invariant violated: bad ratio value in Edn<Ratio>")
//             }
//             (numerator, denominator)
//         }
//     }
// }

impl Into<char> for Edn<kinds::Char> {
    fn into(self) -> char {
        unsafe {
            let mut val = 0u32;
            let ok = edn_character_get(self.inner.as_ptr(), &raw mut val);
            if !ok {
                panic!("Invariant violated: bad char value in Edn<Char>")
            }
            char::from_u32_unchecked(val)
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
