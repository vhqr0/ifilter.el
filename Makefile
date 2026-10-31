EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch -L . -l ifilter-tests.el -f ert-run-tests-batch-and-exit < /dev/null

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile ifilter.el

clean:
	rm -f *.elc
