open Low;;

open Medias;;

open Oxml;;

open Iface_object;;

class iface_style (pn:unit->graphic_pattern) (pc:unit->graphic_pattern) (f:font_object) (fgc:color) (bgc:color)=
object
  val mutable pattern_normal=pn
  method get_pattern_normal=pattern_normal()
  method get_pattern_normal_f=pattern_normal

  val mutable pattern_clicked=pc
  method get_pattern_clicked=pattern_clicked()
  method get_pattern_clicked_f=pattern_clicked

  val mutable fnt=f
  method get_fnt=fnt

  val mutable foreground_color=fgc
  method get_foreground_color=foreground_color
 
  val mutable background_color=bgc
  method get_background_color=background_color

end;;



class iface_style_parser=
object(self)
  inherit xml_parser
  val mutable nm=""
  val mutable pn=(fun()->new graphic_pattern "default" (tile_box 24 24 (128,128,128)))
  val mutable pc=(fun()->new graphic_pattern "default" (tile_box 24 24 (64,64,64)))
  val mutable fgc=(0,0,0)
  val mutable bgc=(0,0,0)
  val mutable fnt=new font_object "none" 8

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | _ -> ()
   
  method parse_child k v=
    match k with
      | "pattern_normal" -> let p=(new xml_tile_parser) in p#parse v;pn<-(fun()->(new graphic_pattern p#get_file p#get_val))    
      | "pattern_clicked" -> let p=(new xml_tile_parser) in p#parse v;pc<-(fun()->(new graphic_pattern p#get_file p#get_val))
      | "foreground_color" -> let p=(new xml_color_parser ) in p#parse v;fgc<-p#get_val;
      | "background_color" -> let p=(new xml_color_parser ) in p#parse v;bgc<-p#get_val;
      | "font" -> let p=(new xml_font_parser ) in p#parse v;fnt<-p#get_val
      | _ -> ()

  method get_val=(nm,new iface_style pn pc fnt fgc bgc)

end;;


exception Iface_style_not_found of string

let default_pattern bgcol bordcol=
  let bg=(tile_box 24 24 bgcol) in
  let fg=(tile_rect 24 24 bordcol) in
    tile_set_alpha fg 255 255 255;
    tile_put_to fg bg 0 0;
    tile_free fg;
    bg;;

let get_default_style n=
  match n with
    | "iface_text_edit" ->new iface_style 
	(fun()->new graphic_pattern "default_normal:text_edit" (default_pattern(200,200,200) (0,0,0)))
	  (fun()->new graphic_pattern "default_clicked" (default_pattern(64,64,64) (0,0,0)))
	  (new font_object "none" 8)
	  (0,0,0) (200,200,200)
    | "iface_text_edit_box" ->new iface_style 
	(fun()->new graphic_pattern "default_normal:text_edit" (default_pattern(200,200,200) (0,0,0)))
	  (fun()->new graphic_pattern "default_clicked" (default_pattern(64,64,64) (0,0,0)))
	  (new font_object "none" 8)
	  (0,0,0) (200,200,200)
    | _ -> new iface_style 
	(fun()->new graphic_pattern "default_normal" (default_pattern(128,128,128) (0,0,0)))
	  (fun()->new graphic_pattern "default_clicked" (default_pattern(64,64,64) (0,0,0)))
	  (new font_object "none" 8)
	  (0,0,0) (200,200,200)


class iface_theme (hs:(string,iface_style) Hashtbl.t)=
object
  val mutable styles=hs

  method get_style n=
    (try
       Hashtbl.find styles n 
     with Not_found -> get_default_style n)
(*raise (Iface_style_not_found n))*)


end;;

class iface_theme_parser=
object(self)
  inherit [iface_style] xml_stringhash_parser "iface_style" (fun()->new iface_style_parser)

  method get_val=new iface_theme self#get_hash

end;;


let iface_theme_from_xml f=
  let iface_xml=new xml_node (Xml.parse_file f) in
  let p=new iface_theme_parser in
    p#parse iface_xml;
    p#get_val;;
