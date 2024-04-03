EMACS ?= emacs
PACKAGE_DIR=./test/.elpa

# Run all tests by default.
MATCH ?=

.PHONY: test clean install

default: all

# Remove compiled files
clean-compiled:
	rm -f *.elc

# Remove test dependencies, compiled files
clean: clean-compiled
	rm -rf $(PACKAGE_DIR)

# Install test dependencies
install: 
	$(EMACS) --batch -L . -L ./test -l ./test/robby-test-env.el -eval '(robby--install-test-deps)'

# Run unit tests
# Example usage to run all tests matching ^robby--history.*:
# 	make test MATCH=robby--history
test: install
	$(EMACS) --batch -L . -L ./test \
      -l ./test/robby-test-env.el \
      -l ./test/robby-actions-test.el \
      -l ./test/robby-grounding-fns-test.el \
      -l ./test/robby-history-test.el \
      -l ./test/robby-logging-test.el \
      -l ./test/robby-providers-test.el \
      -l ./test/robby-request-test.el \
      -l ./test/robby-test-env.el \
      -l ./test/robby-utils-test.el \
      -l ./test/robby-validation-test.el \
      -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'

EL_FILES := $(wildcard *.el)

# Run checkdoc on elisp files. To do this, we run checkdoc-file via -eval on every .el file in EL_FILES
checkdoc:
	for FILE in ${EL_FILES}; do $(EMACS) --batch -L . -l ./test/robby-test-env.el -eval "(checkdoc-file \"$$FILE\")" ; done

compile: install clean-compiled
	$(EMACS) --batch -L . -l ./test/robby-test-env.el -f batch-byte-compile robby-*.el

lint: install
	$(EMACS) --batch -L . -l ./test/robby-test-env.el -f package-lint-batch-and-exit robby-*.el

all: test checkdoc compile lint
