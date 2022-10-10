STACK_BIN_PATH=~/.local/bin

GRASS=bin/grass
PLANT=bin/plant

all: a.w

a.w: input.ml $(PLANT)
	@$(PLANT) $< -o $@

run: a.w $(GRASS)
	@$(GRASS) $<

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
