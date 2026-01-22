use csgsi_rust::CSGSI;
use serde_json::Value;
use std::{fs, path::Path};
use std::sync::{Arc, Mutex};

fn read_fixture(rel_path: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(rel_path);
    fs::read_to_string(&path).unwrap_or_else(|e| panic!("failed to read {:?}: {}", path, e))
}

#[test]
fn digest_from_file_emits_data() {
    let json_str = read_fixture("tests/fixtures/sample.json");
    let raw: Value = serde_json::from_str(&json_str).expect("fixture must be valid JSON");

    let mut gsi = CSGSI::new();

    let events: Arc<Mutex<Vec<Value>>> = Arc::new(Mutex::new(Vec::new()));
    let events_clone = Arc::clone(&events);

    gsi.on("data", move |arg, _| {
        events_clone.lock().unwrap().push(arg.clone());
    });

    let parsed = gsi.digest(&raw).expect("digest() returned None");

    println!("{}", serde_json::to_string(&parsed).unwrap());

    assert!(!parsed.players.is_empty(), "players should not be empty");
    assert!(!parsed.map.name.is_empty(), "map.name should not be empty");

    let ev = events.lock().unwrap();
    assert!(!ev.is_empty(), "expected at least one 'data' event");
}

#[test]
fn digest_returns_none_if_missing_required_fields() {
    let mut gsi = CSGSI::new();
    let raw: Value = serde_json::json!({"hello": "world"});
    assert!(gsi.digest(&raw).is_none());
}
