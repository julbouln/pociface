OCAMLMAKEFILE = OCamlMakefile

INCDIRS=../poclow ../extlib-1.3 ../xml-light2.1 ../lua-ml ../poccore
LIBS=poclow xml-light extLib str unix lua-std poccore

SOURCES = interface.ml iface_event.ml

OCAMLOPT=ocamlopt.opt

RESULT  = pociface

all : ncl

include $(OCAMLMAKEFILE)