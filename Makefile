SHELL := /bin/bash
EMACS ?= emacs
ifeq ($(shell command -v uv 2>/dev/null),)
$(error uv not found)
endif
INSTALLDIR ?= package-user-dir
PYSRC := $(shell git ls-files *.py)
ELSRC := $(shell git ls-files gnus-summarize*.el nn*.el)
TESTSRC := $(shell git ls-files test*.el)

.PHONY: compile
compile: deps/archives/gnu/archive-contents
	$(EMACS) -batch -l gnus \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(put 'gnus-select-method 'byte-obsolete-variable nil)" \
	  --eval "(put 'when-let 'byte-obsolete-info nil)" \
	  --eval "(put 'if-let 'byte-obsolete-info nil)" \
	  --eval "(put 'gnus-secondary-select-methods 'byte-obsolete-variable nil)" \
	  --eval "(setq package-user-dir (expand-file-name \"deps\"))" \
	  -f package-initialize \
	  -L . \
	  -f batch-byte-compile $(ELSRC) $(TESTSRC); \
	  (ret=$$? ; rm -f $(ELSRC:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

uv.lock:
	uv sync -q

.PHONY: install-py
install-py: .venv $(wildcard *.py)
	uv pip install --quiet --force-reinstall --editable .

deps/archives/gnu/archive-contents: gnus-summarize.el
	$(call install-recipe,$(CURDIR)/deps)
	rm -rf deps/gnus-summarize* # just keep deps

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
	PKG_NAME=`$(EMACS) -batch -L . -l gnus-summarize-package --eval "(princ (gnus-summarize-package-name))"`; \
	rm -rf $${PKG_NAME}; \
	rm -rf $${PKG_NAME}.tar; \
	)

.PHONY: dist
dist: dist-clean
	$(EMACS) -batch -L . -l gnus-summarize-package -f gnus-summarize-package-inception
	( \
	set -e; \
	PKG_NAME=`$(EMACS) -batch -L . -l gnus-summarize-package --eval "(princ (gnus-summarize-package-name))"`; \
	rsync -R $(ELSRC) $(PYSRC) Makefile pyproject.toml chat-prompt.txt $${PKG_NAME} && \
	tar cf $${PKG_NAME}.tar $${PKG_NAME}; \
	)

define install-recipe
	$(MAKE) dist
	( \
	set -e; \
	INSTALL_PATH=$(1); \
	if [[ "$${INSTALL_PATH}" == /* ]]; then INSTALL_PATH=\"$${INSTALL_PATH}\"; fi; \
	PKG_NAME=`$(EMACS) -batch -L . -l gnus-summarize-package --eval "(princ (gnus-summarize-package-name))"`; \
	$(EMACS) --batch -l package --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" \
	  -f package-initialize \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote gnus-summarize) package-alist)))" \
	  -f package-refresh-contents \
	  --eval "(package-install-file \"$${PKG_NAME}.tar\")"; \
	PKG_DIR=`$(EMACS) -batch -l package --eval "(setq package-user-dir (expand-file-name $${INSTALL_PATH}))" -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'gnus-summarize package-alist))))"`; \
	GIT_DIR=`git rev-parse --show-toplevel`/.git $(MAKE) -C $${PKG_DIR} uv.lock; \
	)
	$(MAKE) dist-clean
endef

.PHONY: install
install:
	$(call install-recipe,$(INSTALLDIR))

README.rst: README.in.rst gnus-summarize.el
	grep ';;' gnus-summarize.el \
	  | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	  | sed -e 's/^\s*;;\s\?/   /g' \
	  | bash readme-sed.sh "COMMENTARY" README.in.rst > README.rst
