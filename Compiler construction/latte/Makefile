all: build

DIR := $(shell pwd)
BNFC_BIN ?= $(shell which bnfc)
CABAL_BIN ?= $(shell which cabal)
CLANG_BIN ?= $(shell which clang)
LLVM_AS_BIN ?= $(shell which llvm-as)
CABAL_INSTALL_OPTS ?= --install-method=copy --installdir=$( shell pwd ) --overwrite-policy=always
STUDENTS_BIN_PATH ?= /home/students/inf/PUBLIC/MRJP/bin/

ifeq (, $(CABAL_BIN))
	CABAL_BIN := $(STUDENTS_BIN_PATH)cabal
endif

ifeq (, $(BNFC_BIN))
	BNFC_BIN := $(STUDENTS_BIN_PATH)bnfc
endif

verify-bnfc: $(or $(BNFC_BIN), $(error "BNFC not found")) 
verify-bnfc: bnfc_version := $(shell $(BNFC_BIN) --version)
verify-bnfc:
	$(if $(bnfc_version) != "" && $(bnfc_version) >= 2.9.2,@echo "BNFC version OK $(bnfc_version)",$(error "BNFC version required >= 2.9.2"))
.PHONY: verify-bnfc

lib: 
	$(CLANG_BIN) -O2 -o lib/runtime.ll -emit-llvm -S lib/runtime.c && $(LLVM_AS_BIN) -o lib/runtime.bc lib/runtime.ll
.PHONY: lib

grammar: verify-bnfc
	cd ./src/grammar && $(BNFC_BIN) -d --functor Latte.cf && rm -f Latte/Test.hs
.PHONY: grammar

build:
	$(CABAL_BIN) install $(CABAL_INSTALL_OPTS)
	ln -f latc_llvm latc
.PHONY: build

clean:
	$(CABAL_BIN) clean
	rm -f latc_llvm latc
.PHONY: clean