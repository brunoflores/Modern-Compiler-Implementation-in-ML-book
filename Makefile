.PHONY: intall-deps
install-deps:
	esy install

.PHONY: build
build: install-deps
	esy build dune build

.PHONY: build-watch
build-watch: install-deps
	esy build dune build --watch

.PHONY: test
test: build
	cd test/lexer && \
	./test_all.sh && \
	cd ../../

	cd test/parser && \
	./test_all.sh && \
	cd ../../

.PHONY: test-watch
test-watch:
	./test_watch.sh
