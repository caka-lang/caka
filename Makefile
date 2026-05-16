.PHONY: all build

all: build

setup:
	cd lib/codegen && meson setup build

build: clean
	meson compile -C lib/codegen/build
	cp lib/codegen/build/libcakabackend.a bin/
	dune build @all

clean:
	rm bin/libcakabackend.a || true
	dune clean

test: clean build
	dune test
