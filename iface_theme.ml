open Low;;

open Medias;;

open Oxml;;


class iface_style (pn:tile) (pc:tile) (f:font_object) (fgc:color) (bgc:color)=
object
  val mutable pattern_normal=pn
  method get_pattern_normal=pattern_normal

  val mutable pattern_clicked=pc
  method get_pattern_clicked=pattern_clicked

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
  val mutable pn=tile_empty()
  val mutable pc=tile_empty()
  val mutable fgc=(0,0,0)
  val mutable bgc=(0,0,0)
  val mutable fnt=new font_object "none" 0

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | _ -> ()
   
  method parse_child k v=
    match k with
      | "pattern_normal" -> let p=(new xml_tile_parser) in p#parse v;pn<-p#get_val    
      | "pattern_clicked" -> let p=(new xml_tile_parser) in p#parse v;pc<-p#get_val    
      | "foreground_color" -> let p=(new xml_color_parser ) in p#parse v;fgc<-p#get_val;
      | "background_color" -> let p=(new xml_color_parser ) in p#parse v;bgc<-p#get_val;
      | "font" -> let p=(new xml_font_parser ) in p#parse v;fnt<-p#get_val
      | _ -> ()

  method get_val=(nm,new iface_style pn pc fnt fgc bgc)

end;;


exception Iface_style_not_found of string

class iface_theme (hs:(string,iface_style) Hashtbl.t)=
object
  val mutable styles=hs

  method get_style n=
    (try
       Hashtbl.find styles n 
     with Not_found -> raise (Iface_style_not_found n))


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
