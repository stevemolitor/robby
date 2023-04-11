EMACS ?= emacs

# Run all tests by default.
MATCH ?=

.PHONY: test

# Example usage to run all tests matching ^robby--history.*:
# make test MATCH=robby--history
test:
	$(EMACS) --batch -L . -l robby-tests.el -eval '(ert-run-tests-batch-and-exit "$(MATCH)")'
