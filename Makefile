.PHONY: intall-deps
install-deps:
	esy install

.PHONY: build
build: install-deps
	esy build dune build

.PHONY: test
test: build
	./test_all.sh

.PHONY: test-watch
test-watch:
	./test_watch.sh
