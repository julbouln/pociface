OCAMLMAKEFILE = OCamlMakefile

INCDIRS=../poclow ../extlib-1.3 ../xml-light2.1 ../lua-ml ../poccore
LIBS=poclow xml-light extLib str unix lua-std poccore

SOURCES = iface_object.ml iface_container.ml iface_text.ml iface_button.ml iface_menu.ml iface_misc.ml interface.ml iface_event.ml iface_theme.ml iface_xml.ml

OCAMLOPT=ocamlopt.opt

RESULT  = pociface

all : ncl

include $(OCAMLMAKEFILE)