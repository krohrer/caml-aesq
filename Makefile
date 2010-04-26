.DEFAULT: all

OCAMLMAKEFILE = ./OCamlMakefile

RESULT = aesq
TEST = test

OCAMLFLAGS = -w Aelz
LIB_PACK_NAME = $(RESULT)
SOURCES = lazyList.mli lazyList.ml ansi.mli ansi.ml text.mli text.ml

OCAMLDOCFLAGS = -intro aesq.doc -t "CAML-Aesq"
DOC_FILES = lazyList.mli ansi.mli text.mli

LIBINSTALL_FILES = ansi.mli ansi.cmi lazyList.mli lazyList.cmi text.mli text.cmi $(RESULT).cmi $(RESULT).cma $(RESULT).cmxa $(RESULT).a

all:ncl bcl htdoc
install:libinstall
uninstall:libuninstall

-include $(OCAMLMAKEFILE)

test:$(TOPRESULT)
	./$(TOPRESULT) $(TEST).ml
