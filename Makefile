STACK_BIN_PATH=~/.local/bin

GRASS=bin/grass
PLANT=bin/plant
GRASS_ML=bin/grass_ml

SBCL=sbcl

all: out/rot13.w

out/rot13.ml: src/rot13.cl $(wildcard src/*.cl)
	mkdir -p out
	$(SBCL) --script $< > $@.tmp
	mv $@.tmp $@

out/rot13.w: out/rot13.ml $(PLANT)
	$(PLANT) $< -o $@

out/main.ml: src/main.cl $(wildcard src/*.cl)
	mkdir -p out
	$(SBCL) --script $< > $@.tmp
	mv $@.tmp $@

out/main.w: out/main.ml $(PLANT)
	$(PLANT) $< -O -o $@

run: out/rot13.w $(GRASS_ML)
	$(GRASS_ML) $<

out/putchar.ml: examples/putchar.cl $(wildcard src/*.cl)
	mkdir -p out
	$(SBCL) --script $< > $@.tmp
	mv $@.tmp $@

out/putchar.asm.w: out/putchar.ml $(PLANT)
	$(PLANT) $< -O -o $@

out/asm-standalone.w: out/main.w examples/asm.w
	cat $^ > $@

run-asm: out/asm-standalone.w $(GRASS_ML)
	$(GRASS_ML) $<


build/grass.ml:
	mkdir -p build
	wget http://panathenaia.halfmoon.jp/alang/grass/grass.ml

ocamlc-env:
	opam switch create 3.09.3
	eval $(opam env --switch=3.09.3)

$(GRASS_ML): build/grass.ml
	# If compilation fails, try running `make ocamlc-env` to use an older version of ocamlc
	ocamlc -o $@ $<

build/Grassy/stack.yaml:
	mkdir -p build
	cd build; git clone https://github.com/susisu/Grassy

$(GRASS): build/Grassy/stack.yaml
	mkdir -p bin
	cd build/Grassy; stack install
	cp $(STACK_BIN_PATH)/grass $@

$(PLANT): build/Grassy/stack.yaml
	mkdir -p bin
	cd build/Grassy; stack install
	cp $(STACK_BIN_PATH)/plant $@
