DUMP    := roswell/cupertino
INSTALL := $(HOME)/.roswell/bin/cupertino

.PHONY: deps build install verify

deps:
	qlot install

build: deps
	qlot exec ros build roswell/cupertino.ros

install: build
	ln -sf "$(CURDIR)/$(DUMP)" "$(INSTALL)"

verify:
	@which cupertino && file "$$(which cupertino)"
