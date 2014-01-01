.PHONY : test test-function

EMACS ?= emacs
LOADPATH = -L .
LOAD_HELPER = -l test/test-helper.el

test:
	$(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) -l test/function.el \
		-f ert-run-tests-batch-and-exit

test-function:
	$(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) -l test/function.el
		-f ert-run-tests-batch-and-exit
