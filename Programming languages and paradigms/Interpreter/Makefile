all: interpreter

EXEC ?= interpreter
BUILD_DIRS ?= app src/Interpreter src/Typechecker src/Common grammar/Grammar
BNFC_BIN := $(shell which bnfc)
CABAL_BIN := $(shell which cabal)
STUDENTS_BIN_PATH := /home/students/inf/PUBLIC/MRJP/bin/

ifeq (, $(CABAL_BIN))
	CABAL_BIN := $(STUDENTS_BIN_PATH)cabal
endif

ifeq (, $(BNFC_BIN))
	BNFC_BIN := $(STUDENTS_BIN_PATH)bnfc
endif

interpreter: dependencies build

build: app/Main.hs src/**/*.hs grammar/**/*.hs
	ghc $< -iapp:src:grammar -o $(EXEC)	

.PHONY: dependencies	
dependencies:
	cabal update && \
	cabal install mtl lens

.PHONY: verify_bnfc
verify_bnfc: bnfc_version := $(shell $(BNFC_BIN) --version)
verify_bnfc:
	$(if $(bnfc_version) && "$(bnfc_version)" >= 2.9.2,@echo "BNFC version OK $(bnfc_version)",$(error "BNFC version required >= 2.9.1"))

.PHONY: grammar
grammar: verify_bnfc
	 cd grammar && $(BNFC_BIN) -d -m --functor grammar.cf && make

# $1 - dir
define cleandir
	rm -rf interpreter $(1)/*.hi $(1)/*.o $(1)/*.dyn_hi $(1)/*.dyn_o

endef

.PHONY: clean
clean:
	$(foreach dir,$(BUILD_DIRS),$(call cleandir,$(dir)))
	rm -f $(EXEC)
