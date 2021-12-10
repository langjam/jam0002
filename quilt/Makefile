.ONESHELL:
.PHONY: check clean build fmt clippy test

check: build fmt clippy test

clean:
	cargo clean

build:
	cargo build

fmt:
	cargo fmt -- --check

clippy:
	cargo clippy --all-targets -- --deny warnings

test:
	cargo test -- --nocapture
