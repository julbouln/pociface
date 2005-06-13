OCAMLMAKEFILE = OCamlMakefile

PACKS=poccore

LIBINSTALL_FILES=*.cmi *.cmo *.cmx *.a pociface.cma pociface.cmxa

SOURCES = iface_properties.ml iface_object.ml iface_container.ml iface_text.ml iface_button.ml iface_scrollbar.ml iface_menu.ml iface_tool.ml iface_window.ml iface_misc.ml iface_theme.ml iface_xml.ml interface.ml iface_event.ml

OCAMLDOC=ocamlfind ocamldoc -package "$(PACKS)"
DOC_FILES=$(SOURCES)

RESULT  = pociface

all : ncl bcl

include $(OCAMLMAKEFILE)