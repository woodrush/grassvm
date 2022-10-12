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
	cd build; git clone https://gist.github.com/woodrush/3d85a6569ef3c85b63bfaf9211881af6
	cd build; cp 3d85a6569ef3c85b63bfaf9211881af6/grass.ml .

$(GRASS_ML): build/grass.ml
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
