OCAMLMAKEFILE = OCamlMakefile



#INCDIRS=../lua-ml 
#LIBS=xml-light str unix lua-std

PACKS=poccore

LIBINSTALL_FILES=*.cmi *.cmx *.a pociface.cmxa
#LIB_PACK_NAME=pociface


SOURCES = iface_properties.ml iface_object.ml iface_container.ml iface_text.ml iface_button.ml iface_menu.ml iface_tool.ml iface_window.ml iface_misc.ml iface_theme.ml iface_xml.ml interface.ml iface_event.ml

DOC_FILES=$(SOURCES)

RESULT  = pociface

all : ncl

include $(OCAMLMAKEFILE)