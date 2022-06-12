EMACS ?= emacs

.PHONY: check
check:
	$(EMACS) -Q --batch -L . -l taco -l taco-tests \
	--eval '(ert-run-tests-batch-and-exit t)'
