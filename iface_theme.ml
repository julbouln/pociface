open Low;;

open Medias;;

open Oxml;;

open Iface_object;;
open Iface_properties;;

class xml_iface_style_parser=
object(self)
  inherit xml_parser
  val mutable nm=""
  val mutable props=new iface_properties

  method get_val=(nm,props)

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | _ -> ()
   
  method parse_child k v=
    match k with
      | "properties" -> let p=(new xml_iface_props_parser) in p#parse v;props<-p#get_val
      | _ -> ()

end;;

let default_pattern bgcol bordcol=
  let bg=(tile_box 24 24 bgcol) in
  let fg=(tile_rect 24 24 bordcol) in
    tile_set_alpha fg 255 255 255;
    tile_put_to fg bg 0 0;
    tile_free fg;
    bg;;

let get_default_style n=

  let props=new iface_properties in
    (match n with
       | "iface_text_edit" ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->new graphic_pattern "default:text_edit" (default_pattern(200,200,200) (0,0,0))));
	     ("font",IPropFont (new font_object "none" 8));
	     ("foreground_color",IPropColor (0,0,0))
	   ]
       | "iface_text_edit_box" ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->new graphic_pattern "default:text_edit" (default_pattern(200,200,200) (0,0,0))));
	     ("font",IPropFont (new font_object "none" 8));
	     ("foreground_color",IPropColor (0,0,0))
	   ]
       | "iface_hcontainer" ->props#from_list
	   [
	     ("valign",IPropAlign IAlignMiddle);
	     ("halign",IPropAlign IAlignMiddle);
	     ("fixed_size",IPropBool false);
	     ("symmetric_size",IPropBool false) 
	   ]
       | "iface_vcontainer" ->props#from_list
	   [
	     ("valign",IPropAlign IAlignMiddle);
	     ("halign",IPropAlign IAlignMiddle);
	     ("fixed_size",IPropBool false); 
	     ("symmetric_size",IPropBool false) 
	   ]
     
       | "iface_menu" ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->(new graphic_pattern "default" (default_pattern(128,128,128) (0,0,0)))));
	   ]
       | "iface_menubar" ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->(new graphic_pattern "default" (default_pattern(128,128,128) (0,0,0)))));
	   ]
       | _ ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->new graphic_pattern "default" (default_pattern(128,128,128) (0,0,0))));
	     ("pattern_normal",IPropPattern  (fun()->new graphic_pattern "default" (default_pattern(128,128,128) (0,0,0))));
	     ("pattern_clicked",IPropPattern  (fun()->new graphic_pattern "default_clicked" (default_pattern(64,64,64) (0,0,0))));
	     ("font",IPropFont (new font_object "none" 8));
	     ("foreground_color",IPropColor (0,0,0))
	   ];
    );
    props;;
      

exception Iface_style_not_found of string;;

class iface_theme (hs:(string,iface_properties) Hashtbl.t)=
object
  val mutable styles=hs

  method get_style n=
    (try
       Hashtbl.find styles n 
     with Not_found -> get_default_style n)
(*raise (Iface_style_not_found n))*)


end;;

class xml_iface_theme_parser=
object(self)
  inherit [iface_properties] xml_stringhash_parser "iface_style" (fun()->new xml_iface_style_parser)

  method get_val=new iface_theme self#get_hash

end;;


let iface_theme_from_xml f=
  let iface_xml=new xml_node (Xml.parse_file f) in
  let p=new xml_iface_theme_parser in
    p#parse iface_xml;
    p#get_val;;
