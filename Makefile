OCAMLMAKEFILE = OCamlMakefile

INCDIRS=../poclow ../extlib-1.3 ../xml-light2.1 ../lua-ml ../poccore
LIBS=poclow xml-light extLib str unix lua-std poccore

SOURCES = iface_properties.ml iface_object.ml iface_container.ml iface_text.ml iface_button.ml iface_menu.ml iface_window.ml iface_misc.ml iface_theme.ml iface_xml.ml interface.ml iface_event.ml 

OCAMLOPT=ocamlopt.opt

RESULT  = pociface

all : ncl

include $(OCAMLMAKEFILE)