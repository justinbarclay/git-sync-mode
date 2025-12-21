EMACS   ?= emacs
EASK    ?= eask
PACKAGE = git-sync-mode
DEPS_FILE = .eask-deps

.PHONY: all clean test install install-dev coverage

install: Eask
	$(EASK) install --all

$(DEPS_FILE): Eask
	$(EASK) install-deps --dev
	@touch $@

install-dev: $(DEPS_FILE)

all: install
	$(EASK) exec $(EMACS) -Q --batch -f batch-byte-compile *.el

clean:
	rm -f *.elc

test: install-dev
	$(MAKE) clean
	$(EASK) test ert-runner

coverage: install-dev
	$(MAKE) clean
	UNDERCOVER_FORCE=true $(EASK) test ert-runner

COVERAGE_DIR := .coverage

clean-coverage:
	rm -rf $(COVERAGE_DIR)
