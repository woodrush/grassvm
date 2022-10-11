STACK_BIN_PATH=~/.local/bin

GRASS=bin/grass
PLANT=bin/plant

SBCL=sbcl

all: out/rot13.w

out/rot13.ml: src/rot13.cl $(wildcard src/*.cl)
	mkdir -p out
	$(SBCL) --script $< > $@.tmp
	mv $@.tmp $@

out/rot13.w: out/rot13.ml $(PLANT)
	$(PLANT) $< -o $@

out/a.opt.w: out/grassvm.ml $(PLANT)
	$(PLANT) $< -O -o $@

run: out/rot13.w $(GRASS)
	$(GRASS) $<

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
