# Makefile for inline-review

EMACS ?= emacs

.PHONY: test clean

test:
	$(EMACS) -batch --eval "(add-to-list 'load-path \".\")" \
		-l ert -l test/inline-review-test.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
