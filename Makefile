.DEFAULT: all

OCAMLMAKEFILE = ./OCamlMakefile

RESULT = aesq

PACKS = extlib inspect

OCAMLFLAGS = -w Aelz
LIB_PACK_NAME = $(RESULT)
SOURCES = lazyStream.mli lazyStream.ml ansi.mli ansi.ml text.mli text.ml

OCAMLDOCFLAGS = -intro aesq.doc -t "CAML-Aesq"
# DOC_FILES = format.mli lazyStream.mli

# LIBINSTALL_FILES = format.mli lazyStream.mli format.cmi lazyStream.cmi $(RESULT).cmi $(RESULT).cma $(RESULT).cmxa $(RESULT).a lib$(RESULT)_stubs.a dll$(RESULT)_stubs.so

all:ncl bcl htdoc
install:libinstall
uninstall:libuninstall

-include $(OCAMLMAKEFILE)
