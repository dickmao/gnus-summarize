include epkg.mk
epkg.mk:
	emacs --batch -l package -f package-initialize -l epkg -f epkg-copy-mk

export VERSION := $(shell git describe --tags --abbrev=0 2>/dev/null || echo 0.0.1)
SHELL := /bin/bash
EMACS ?= emacs
ifeq ($(shell command -v uv 2>/dev/null),)
$(error uv not found)
endif
INSTALLDIR ?= package-user-dir
PYSRC := $(shell git ls-files *.py)
ELSRC := $(shell git ls-files gnus-summarize*.el nn*.el)
TESTSRC := $(shell git ls-files test*.el)

EPKG_FILES := $(ELSRC) $(PYSRC) Makefile pyproject.toml chat-prompt.txt
EPKG_EL := $(ELSRC) $(TESTSRC)
EPKG_MAIN := gnus-summarize.el
EPKG_TEST_EL := $(TESTSRC)

.PHONY: compile
compile: epkg-compile

uv.lock:
	uv sync -q

.PHONY: install-py
install-py: .venv $(wildcard *.py)
	uv pip install --quiet --force-reinstall --editable .

.PHONY: test
test: compile epkg-test

.PHONY: dist-clean
dist-clean: epkg-dist-clean

.PHONY: dist
dist: ghostty-vt-module.so epkg-dist

.PHONY: install
install: epkg-install

README.rst: README.in.rst gnus-summarize.el
	grep ';;' gnus-summarize.el \
	  | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	  | sed -e 's/^\s*;;\s\?/   /g' \
	  | bash readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: retag
retag:
	2>/dev/null git tag -d $(VERSION) || true
	2>/dev/null git push --delete origin $(VERSION) || true
	git tag $(VERSION)
	git push origin $(VERSION)
