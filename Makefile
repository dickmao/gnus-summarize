SHELL := /bin/bash
EMACS ?= emacs
ifeq ($(shell command -v uv 2>/dev/null),)
$(error uv not found)
endif
INSTALLDIR ?= package-user-dir
SRC := $(shell git ls-files gnus-summarize*.el)
TESTSRC := $(shell git ls-files test*.el)

.venv:
	uv venv

.PHONY: install-py
install-py: .venv $(wildcard *.py)
	uv pip install --quiet --force-reinstall --editable .

.PHONY: compile
compile:
	$(EMACS) -batch -l debbugs \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . \
	  -f batch-byte-compile $(ELSRC) $(TESTSRC); \
	  (ret=$$? ; rm -f $(ELSRC:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

.PHONY: test
test: compile
	$(EMACS) --batch -L lisp -L test $(patsubst %.el,-l %,$(notdir $(TESTSRC))) -f ert-run-tests-batch

.PHONY: test
test: compile
	$(EMACS) --batch --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . $(patsubst %.el,-l %,$(notdir $(TESTSRC))) \
	  -f ert-run-tests-batch

.PHONY: dist-clean
dist-clean:
	( \
	set -e; \
	PKG_NAME=`$(EMACS) -batch -L . -l debbugs-summarize-package --eval "(princ (debbugs-summarize-package-name))"`; \
	rm -rf $${PKG_NAME}; \
	rm -rf $${PKG_NAME}.tar; \
	)

.PHONY: dist
dist: dist-clean
	$(EMACS) -batch -L . -l debbugs-summarize-package -f debbugs-summarize-package-inception
	( \
	set -e; \
	PKG_NAME=`$(EMACS) -batch -L . -l debbugs-summarize-package --eval "(princ (debbugs-summarize-package-name))"`; \
	rsync -R $(ELSRC) commit-prompt.txt $${PKG_NAME} && \
	tar cf $${PKG_NAME}.tar $${PKG_NAME}; \
	)

define install-recipe
	$(MAKE) dist
	( \
	set -e; \
	INSTALL_PATH=$(1); \
	if [[ "$${INSTALL_PATH}" == /* ]]; then INSTALL_PATH=\"$${INSTALL_PATH}\"; fi; \
	PKG_NAME=`$(EMACS) -batch -L . -l debbugs-summarize-package --eval "(princ (debbugs-summarize-package-name))"`; \
	$(EMACS) --batch -l package --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" \
	  -f package-initialize \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote debbugs-summarize) package-alist)))" \
	  -f package-refresh-contents \
	  --eval "(package-install-file \"$${PKG_NAME}.tar\")"; \
	PKG_DIR=`$(EMACS) -batch -l package --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'debbugs-summarize package-alist))))"`; \
	)
	$(MAKE) dist-clean
endef

.PHONY: install
install:
	( \
	set -e; \
	INSTALL_PATH=$(INSTALLDIR); \
	if [[ "$${INSTALL_PATH}" == /* ]]; then INSTALL_PATH=\"$${INSTALL_PATH}\"; fi; \
	1>/dev/null 2>/dev/null $(EMACS) --batch \
	  --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" \
	  -f package-initialize -l project-claude \
	  --eval "(or (version-list-<= '(0 0 1) \
	   (package-desc-version (car (alist-get 'project-claude package-alist)))) \
	   (error))" || $(MAKE) INSTALLDIR=$(INSTALLDIR) install-project-claude \
	)
	$(call install-recipe,$(INSTALLDIR))
