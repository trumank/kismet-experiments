use paste_buffer::{Parser, Serializer};
use std::fs;

fn assert_str_eq_with_context(left: &str, right: &str, msg: &str) {
    if left == right {
        return;
    }

    // Find first difference
    let mut first_diff = None;
    let left_chars: Vec<char> = left.chars().collect();
    let right_chars: Vec<char> = right.chars().collect();

    for i in 0..left_chars.len().min(right_chars.len()) {
        if left_chars[i] != right_chars[i] {
            first_diff = Some(i);
            break;
        }
    }

    let diff_pos = first_diff.unwrap_or_else(|| left_chars.len().min(right_chars.len()));

    // Show context around difference (50 chars before and after)
    let start = diff_pos.saturating_sub(50);
    let end = (diff_pos + 150).min(left_chars.len().max(right_chars.len()));

    let left_context: String = left_chars
        .get(start..end.min(left_chars.len()))
        .map(|chars| chars.iter().collect())
        .unwrap_or_default();
    let right_context: String = right_chars
        .get(start..end.min(right_chars.len()))
        .map(|chars| chars.iter().collect())
        .unwrap_or_default();

    panic!(
        "{}\n\nStrings differ at position {} (char index)\n\
            Length: left={}, right={}\n\
            Context around difference:\n\
            Left  [...{}...]\n\
            Right [...{}...]\n\
            \n\
            Left char at diff: {:?}\n\
            Right char at diff: {:?}",
        msg,
        diff_pos,
        left.len(),
        right.len(),
        left_context.escape_debug(),
        right_context.escape_debug(),
        left_chars.get(diff_pos),
        right_chars.get(diff_pos),
    );
}

fn test_roundtrip(path: &str) {
    let content = fs::read_to_string(path).unwrap();

    let mut parser = Parser::new(content.clone());
    let paste_buffer = parser.parse().expect("Failed to parse");

    let mut serializer = Serializer::new();
    let serialized = serializer.serialize(&paste_buffer);

    assert_str_eq_with_context(&content, &serialized, "Round trip failed");
}

#[test]
fn test_parse_test1_cp() {
    test_roundtrip("tests/test1.cp")
}

#[test]
fn test_parse_test2_cp() {
    test_roundtrip("tests/test2.cp")
}

#[test]
#[ignore = "requires parsing sub-objects which is not yet implemented"]
fn test_parse_test3_cp() {
    test_roundtrip("tests/test3.cp")
}

#[test]
#[ignore = "nested UMG objects"]
fn test_parse_test4_cp() {
    test_roundtrip("tests/test4.cp")
}

#[test]
#[ignore = "nested UMG objects"]
fn test_parse_test5_cp() {
    test_roundtrip("tests/test5.cp")
}

#[test]
fn test_parse_test6_cp() {
    test_roundtrip("tests/test6.cp")
}

#[test]
fn test_parse_test7_cp() {
    test_roundtrip("tests/test7.cp")
}

#[test]
fn test_parse_test8_cp() {
    test_roundtrip("tests/test8.cp")
}

#[test]
fn test_parse_test9_cp() {
    test_roundtrip("tests/test9.cp")
}

#[test]
fn test_parse_test10_cp() {
    test_roundtrip("tests/test10.cp")
}
