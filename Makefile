OUTPUT := raxon
MAGIC_OUTPUT := magic.rs
RUSTFLAGS := --edition=2021

ifeq ($(RELEASE), 1)
	RUSTFLAGS += -C opt-level=3
else
	RUSTFLAGS += -C opt-level=1
endif

all: print_magic $(OUTPUT)

$(OUTPUT): main.rs $(wildcard ./*.rs)
	rustc $(RUSTFLAGS) $< -o $@

print_magic: print_magic.rs
	rustc $(RUSTFLAGS) $< -o $@
	./$@ > $(MAGIC_OUTPUT)

clean:
	rm $(MAGIC_OUTPUT) $(OUTPUT)
