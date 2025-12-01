#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use ordered_float::OrderedFloat;
use std::collections::{BTreeMap, BTreeSet};
use std::ffi::{c_char, CStr};

// Include the generated bindings
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    BigInt(bool, u8, String),
    Float(OrderedFloat<f64>),
    BigDec(bool, u8, String),
    Character(char),
    String(String),
    Symbol(String, String),
    Keyword(String, String),
    Ratio(i64, i64),
    List(Vec<Box<Value>>),
    Vector(Vec<Box<Value>>),
    Map(BTreeMap<Value, Value>),
    Set(BTreeSet<Value>),
    Tagged(String, Box<Value>),
}

impl From<*const edn_value_t> for Value {
    fn from(value: *const edn_value_t) -> Self {
        let typ = unsafe { edn_type(value) };
        use edn_type_t::*;
        match typ {
            EDN_TYPE_NIL => Value::Nil,
            EDN_TYPE_BOOL => {
                let mut val = false;
                unsafe {
                    edn_bool_get(value, &raw mut val);
                }
                Value::Bool(val)
            }
            EDN_TYPE_INT => {
                let mut val = 0i64;
                unsafe {
                    edn_int64_get(value, &raw mut val);
                }
                Value::Int(val)
            }
            EDN_TYPE_BIGINT => {
                let mut neg = false;
                let mut radix = 0u8;
                unsafe {
                    let c_str_ptr =
                        edn_bigint_get(value, std::ptr::null_mut(), &raw mut neg, &raw mut radix)
                            as *const c_char;
                    let c_str = CStr::from_ptr(c_str_ptr);
                    let string = c_str.to_string_lossy().into_owned();
                    Value::BigInt(neg, radix, string)
                }
            }
            EDN_TYPE_FLOAT => {
                let mut val = 0.0f64;
                unsafe {
                    edn_double_get(value, &raw mut val);
                }
                Value::Float(OrderedFloat(val))
            }
            EDN_TYPE_BIGDEC => {
                let mut neg = false;
                unsafe {
                    let c_str_ptr =
                        edn_bigdec_get(value, std::ptr::null_mut(), &raw mut neg) as *const c_char;
                    let c_str = CStr::from_ptr(c_str_ptr);
                    let string = c_str.to_string_lossy().into_owned();
                    Value::BigDec(neg, 10, string)
                }
            }
            EDN_TYPE_CHARACTER => {
                let mut codepoint = 0u32;
                unsafe {
                    edn_character_get(value, &raw mut codepoint);
                }
                Value::Character(char::from_u32(codepoint).unwrap_or('\u{FFFD}'))
            }
            EDN_TYPE_STRING => unsafe {
                let c_str_ptr = edn_string_get(value, std::ptr::null_mut()) as *const c_char;
                let c_str = CStr::from_ptr(c_str_ptr);
                let string = c_str.to_string_lossy().into_owned();
                Value::String(string)
            },
            EDN_TYPE_SYMBOL => {
                let mut ns_ptr: *const c_char = std::ptr::null();
                let mut name_ptr: *const c_char = std::ptr::null();
                unsafe {
                    edn_symbol_get(
                        value,
                        &raw mut ns_ptr,
                        std::ptr::null_mut(),
                        &raw mut name_ptr,
                        std::ptr::null_mut(),
                    );
                    let ns = if ns_ptr.is_null() {
                        String::new()
                    } else {
                        let c_str = CStr::from_ptr(ns_ptr);
                        c_str.to_string_lossy().into_owned()
                    };
                    let name = {
                        let c_str = CStr::from_ptr(name_ptr);
                        c_str.to_string_lossy().into_owned()
                    };
                    Value::Symbol(ns, name)
                }
            }
            EDN_TYPE_KEYWORD => {
                let mut ns_ptr: *const c_char = std::ptr::null();
                let mut name_ptr: *const c_char = std::ptr::null();
                unsafe {
                    edn_keyword_get(
                        value,
                        &raw mut ns_ptr,
                        std::ptr::null_mut(),
                        &raw mut name_ptr,
                        std::ptr::null_mut(),
                    );
                    let ns = if ns_ptr.is_null() {
                        String::new()
                    } else {
                        let c_str = CStr::from_ptr(ns_ptr);
                        c_str.to_string_lossy().into_owned()
                    };
                    let name = {
                        let c_str = CStr::from_ptr(name_ptr);
                        c_str.to_string_lossy().into_owned()
                    };
                    Value::Keyword(ns, name)
                }
            }
            EDN_TYPE_LIST => {
                let count = unsafe { edn_list_count(value) };
                let mut list = Vec::with_capacity(count);
                for i in 0..count {
                    let item = unsafe { edn_list_get(value, i) };
                    list.push(Box::new(Value::from(item as *const edn_value_t)));
                }
                Value::List(list)
            }
            EDN_TYPE_VECTOR => {
                let count = unsafe { edn_vector_count(value) };
                let mut vector = Vec::with_capacity(count);
                for i in 0..count {
                    let item = unsafe { edn_vector_get(value, i) };
                    vector.push(Box::new(Value::from(item as *const edn_value_t)));
                }
                Value::Vector(vector)
            }
            EDN_TYPE_MAP => {
                let count = unsafe { edn_map_count(value) };
                let mut map = BTreeMap::new();
                for i in 0..count {
                    let key = unsafe { edn_map_get_key(value, i) };
                    let val = unsafe { edn_map_get_value(value, i) };
                    map.insert(
                        Value::from(key as *const edn_value_t),
                        Value::from(val as *const edn_value_t),
                    );
                }
                Value::Map(map)
            }
            EDN_TYPE_SET => {
                let count = unsafe { edn_set_count(value) };
                let mut set = BTreeSet::new();
                for i in 0..count {
                    let item = unsafe { edn_set_get(value, i) };
                    set.insert(Value::from(item as *const edn_value_t));
                }
                Value::Set(set)
            }
            EDN_TYPE_TAGGED => {
                let mut tag_ptr: *const c_char = std::ptr::null();
                let mut tagged_value: *mut edn_value_t = std::ptr::null_mut();
                unsafe {
                    edn_tagged_get(
                        value,
                        &raw mut tag_ptr,
                        std::ptr::null_mut(),
                        &raw mut tagged_value,
                    );
                    let c_str = CStr::from_ptr(tag_ptr);
                    let tag = c_str.to_string_lossy().into_owned();
                    let val = Value::from(tagged_value as *const edn_value_t);
                    Value::Tagged(tag, Box::new(val))
                }
            }
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

impl From<edn_result_t> for Result<Value, EdnError> {
    fn from(result: edn_result_t) -> Self {
        if result.error == edn_error_t::EDN_OK {
            Ok(Value::from(result.value as *const edn_value_t))
        } else {
            let message = if result.error_message.is_null() {
                String::from("Unknown error")
            } else {
                unsafe {
                    let c_str = CStr::from_ptr(result.error_message);
                    c_str.to_string_lossy().into_owned()
                }
            };

            Err(EdnError {
                kind: EdnErrorKind::from(result.error),
                line: result.error_line,
                column: result.error_column,
                message,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_parsing() {
        let input = "42\0";
        let result = unsafe { edn_read(input.as_ptr() as *const c_char, 0) };

        assert_eq!(result.error, edn_error_t::EDN_OK);
        assert!(!result.value.is_null());

        unsafe {
            let value_type = edn_type(result.value);
            assert_eq!(value_type, edn_type_t::EDN_TYPE_INT);

            let mut int_val: i64 = 0;
            let success = edn_int64_get(result.value, &mut int_val);
            assert!(success);
            assert_eq!(int_val, 42);

            edn_free(result.value);
        }
    }

    #[test]
    fn test_result_conversion_success() {
        let input = "42\0";
        let result = unsafe { edn_read(input.as_ptr() as *const c_char, 0) };
        let rust_result: Result<Value, EdnError> = result.into();

        assert!(rust_result.is_ok());
        match rust_result.unwrap() {
            Value::Int(val) => assert_eq!(val, 42),
            _ => panic!("Expected Int value"),
        }
    }

    #[test]
    fn test_result_conversion_error() {
        let input = "{:key\0"; // Incomplete map - missing closing brace
        let result = unsafe { edn_read(input.as_ptr() as *const c_char, 0) };
        let rust_result: Result<Value, EdnError> = result.into();

        assert!(rust_result.is_err());
        let error = rust_result.unwrap_err();
        assert_eq!(error.kind, EdnErrorKind::UnexpectedEof);
        assert!(error.line >= 1);
        assert!(error.column >= 1);
        assert!(!error.message.is_empty());
    }

    #[test]
    fn test_complex_value_conversion() {
        let input = r#"[1 2.5 "hello" :keyword true nil]"#;
        let input_cstr = format!("{}\0", input);
        let result = unsafe { edn_read(input_cstr.as_ptr() as *const c_char, 0) };
        let rust_result: Result<Value, EdnError> = result.into();

        assert!(rust_result.is_ok());
        match rust_result.unwrap() {
            Value::Vector(vec) => {
                assert_eq!(vec.len(), 6);
                assert!(matches!(*vec[0], Value::Int(1)));
                assert!(matches!(*vec[1], Value::Float(_)));
                assert!(matches!(*vec[2], Value::String(_)));
                assert!(matches!(*vec[3], Value::Keyword(_, _)));
                assert!(matches!(*vec[4], Value::Bool(true)));
                assert!(matches!(*vec[5], Value::Nil));
            }
            _ => panic!("Expected Vector value"),
        }
    }
}
