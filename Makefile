EMACS   ?= emacs
EASK    ?= eask
PACKAGE = git-sync-mode

.PHONY: all clean test install

install:
	$(EASK) install --all

all: install
	$(EASK) exec $(EMACS) -Q --batch -f batch-byte-compile *.el

clean:
	rm -f *.elc

test: all
	$(EASK) test ert ./tests/**-tests.el

COVERAGE_DIR := .coverage

clean-coverage:
	rm -rf $(COVERAGE_DIR)
