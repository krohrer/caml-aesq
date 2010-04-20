OCAMLMAKEFILE = ./OCamlMakefile

PACKS = extlib

OCAMLFLAGS = -w Aelz

# LIB_PACK_NAME = ansi
SOURCES = ansi.ml ansi.mli lazyStream.ml lazyStream.mli
DOC_FILES = ansi.mli

RESULT = ansi

all:ncl bcl htdoc

-include $(OCAMLMAKEFILE)
