all: build

DIR := $(shell pwd)
BNFC_BIN ?= $(shell which bnfc)
CABAL_BIN ?= $(shell which cabal)
CABAL_INSTALL_OPTS ?= --install-method=copy --installdir=$(DIR) --overwrite-policy=always
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

grammar: verify-bnfc
	cd ./src/grammar && $(BNFC_BIN) -d Instant.cf && rm -f Instant/Test.hs
.PHONY: grammar

build:
	$(CABAL_BIN) update && $(CABAL_BIN) install $(CABAL_INSTALL_OPTS)
.PHONY: build

clean:
	cabal clean
	rm -f insc_jvm insc_llvm
.PHONY: clean