open Low;;
open Medias;;
open Oxml;;

type iface_object_valign=
  | VAlignTop
  | VAlignBottom
  | VAlignMiddle
;;
type iface_object_halign=
  | HAlignLeft
  | HAlignRight
  | HAlignMiddle
;;



(* TEST *)
(*
type iface_align=
  | IAlignTop
  | IAlignBottom
  | IAlignLeft
  | IAlignRight
  | IAlignMiddle
;;

type iface_padding=
  | IPaddingTop of int
  | IPaddingBottom of int
  | IPaddingLeft of int
  | IPaddingRight of int
;;

type iface_margin=
  | IMarginTop of int
  | IMarginBottom of int
  | IMarginLeft of int
  | IMarginRight of int

type iface_prop=
  | IPropAlign of iface_align
  | IPropPadding of iface_padding
  | IPropMargin of iface_margin
  | IPropPattern of tile
  | IPropColor of color
  | IPropFont of font_object
  | IPropNil;;
      
class iface_properties=
object
  val mutable props=Hashtbl.create 2
  method add_prop (n:string) (p:iface_prop)=Hashtbl.add props n p
  method set_prop n p=Hashtbl.replace props n p
  method del_prop n=Hashtbl.remove props n
  method get_prop n=Hashtbl.find props n
end;;



(*
 <iface_properties name="normal">
  <iface_pattern path="medias/iface/motif_bouton0.png"/>
 </iface_properties>

*)

class xml_iface_props_parser=
object
  inherit xml_parser as super
  val mutable nm=""

  val mutable props=new iface_properties

  method get_val=props

  method parse_child k v=
    match k with
      | "iface_pattern" -> props#add_prop nm (IPropPattern (let p=(new xml_tile_parser) in p#parse v;p#get_val))
      | "iface_color" -> props#add_prop nm (IPropColor (let p=(new xml_color_parser ) in p#parse v;p#get_val))
      | "iface_font" -> props#add_prop nm (IPropFont (let p=(new xml_font_parser ) in p#parse v;p#get_val))
      | _ -> props#add_prop nm (IPropNil)

  method parse_attr k v=
    match k with
      | "name" -> nm<-v
      | _ -> ()

end;;

*)
