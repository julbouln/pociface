OCAMLMAKEFILE = OCamlMakefile

PACKS=poccore

LIBINSTALL_FILES=*.cmi *.cmx *.a pociface.cmxa

SOURCES = iface_properties.ml iface_object.ml iface_container.ml iface_text.ml iface_button.ml iface_menu.ml iface_tool.ml iface_window.ml iface_misc.ml iface_theme.ml iface_xml.ml interface.ml iface_event.ml

OCAMLDOC=ocamlfind ocamldoc -package "$(PACKS)"
DOC_FILES=$(SOURCES)

RESULT  = pociface

all : ncl

include $(OCAMLMAKEFILE)