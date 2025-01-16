

.PHONY: subs
subs: build/subs

build/subs: stack-build | build
	fd frequent-substring-exe .stack-work/install --type f | xargs -I{} cp {} build/subs

.PHONY: stack-build
stack-build:
	stack build

build:
	mkdir build

clean:
	rm -rf build



