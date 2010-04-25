.DEFAULT: all

OCAMLMAKEFILE = ./OCamlMakefile

RESULT = aesq
TEST = ansi_test

PACKS = extlib

OCAMLFLAGS = -w Aelz
LIB_PACK_NAME = $(RESULT)
SOURCES = lazyStream.mli lazyStream.ml ansi.mli ansi.ml text.mli text.ml

OCAMLDOCFLAGS = -intro aesq.doc -t "CAML-Aesq"
DOC_FILES = lazyStream.mli ansi.mli text.mli

LIBINSTALL_FILES = ansi.mli ansi.cmi lazyStream.mli lazyStream.cmi text.mli text.cmi $(RESULT).cmi $(RESULT).cma $(RESULT).cmxa $(RESULT).a

all:ncl bcl htdoc
install:libinstall
uninstall:libuninstall

-include $(OCAMLMAKEFILE)

test:$(TOPRESULT)
	./$(TOPRESULT) $(TEST).ml
