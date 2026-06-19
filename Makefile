DUMP    := roswell/cupertino
INSTALL := $(HOME)/.roswell/bin/cupertino

.PHONY: build install verify

build:
	ros build roswell/cupertino.ros

install: build
	ln -sf "$(CURDIR)/$(DUMP)" "$(INSTALL)"

verify:
	@which cupertino && file "$$(which cupertino)"
