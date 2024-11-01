OUTPUT := raxon
RUSTFLAGS := --edition=2021

ifeq ($(RELEASE), 1)
	RUSTFLAGS += -C opt-level=3
else
	RUSTFLAGS += -C opt-level=1
endif

$(OUTPUT): main.rs $(wildcard ./*.rs)
	rustc $(RUSTFLAGS) $< -o $@
