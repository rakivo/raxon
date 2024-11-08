OUTPUT := raxon
MAGIC_OUTPUT := magic.rs
RUSTFLAGS := --edition=2021
CARGOFLAGS :=
CODEGENFLAGS :=

ifeq ($(RELEASE), 1)
	TARGET_DIR := ./target/release/$(OUTPUT)
	CODEGENFLAGS += -C opt-level=3
	CARGOFLAGS += --release
else
	TARGET_DIR := ./target/debug/$(OUTPUT)
	CODEGENFLAGS += -C opt-level=1
endif

all: print_magic $(OUTPUT)

$(OUTPUT): main.rs $(wildcard ./*.rs)
	RUSTFLAGS="$(CODEGENFLAGS)" cargo build $(CARGOFLAGS)
	mv $(TARGET_DIR) .

print_magic: print_magic.rs
	rustc $(RUSTFLAGS) $(CODEGENFLAGS) $< -o $@
	./$@ > $(MAGIC_OUTPUT)

clean:
	rm $(MAGIC_OUTPUT) $(OUTPUT)
