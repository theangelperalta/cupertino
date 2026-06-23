DUMP    := roswell/cupertino
INSTALL := $(HOME)/.roswell/bin/cupertino

.PHONY: deps build install verify test

deps:
	qlot install

build: deps
	qlot exec ros build roswell/cupertino.ros

test: deps
	qlot exec ros run -- --non-interactive \
	  --eval '(ql:quickload :cupertino/tests :silent t)' \
	  --eval '(uiop:quit (if (rove:run-suite :cupertino/tests) 0 1))'

install: build
	ln -sf "$(CURDIR)/$(DUMP)" "$(INSTALL)"

verify:
	@which cupertino && file "$$(which cupertino)"
