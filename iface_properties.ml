open Low;;
open Medias;;
open Oxml;;


(* DEPRECATED *)

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



(* NEW *)

exception Bad_iface_prop;;
exception Iface_prop_not_found of string;;

type iface_align=
  | IAlignTop
  | IAlignBottom
  | IAlignLeft
  | IAlignRight
  | IAlignMiddle
  | IAlignNil
;;

type iface_prop=
  | IPropAlign of iface_align
  | IPropPadding of int
  | IPropMargin of int
  | IPropPattern of graphic_pattern
  | IPropPatternFun of (unit -> graphic_pattern)
  | IPropColor of color
  | IPropFont of font_object
  | IPropNil;;

let iprop_align p=match p with
  | IPropAlign x->x
  | _ -> raise Bad_iface_prop;;

let iprop_padding p=match p with
  | IPropPadding x->x
  | _ -> raise Bad_iface_prop;;

let iprop_margin p=match p with
  | IPropMargin x->x
  | _ -> raise Bad_iface_prop;;

let iprop_pattern p=match p with
  | IPropPattern x->x
  | _ -> raise Bad_iface_prop;;

let iprop_pattern_fun p=match p with
  | IPropPatternFun x->x
  | _ -> raise Bad_iface_prop;;
      
let iprop_color p=match p with
  | IPropColor x->x
  | _ -> raise Bad_iface_prop;;

let iprop_font p=match p with
  | IPropFont x->x
  | _ -> raise Bad_iface_prop;;

class iface_properties=
object(self)
  val mutable props=Hashtbl.create 2
  method add_prop (n:string) (p:iface_prop)=Hashtbl.add props n p
  method set_prop n p=Hashtbl.replace props n p
  method del_prop n=Hashtbl.remove props n
  method is_prop n=Hashtbl.mem props n
  method get_prop n=
    (try 
       Hashtbl.find props n
     with Not_found -> raise (Iface_prop_not_found n))
  method foreach_prop f=
    Hashtbl.iter f props
  method from_list l=
    List.iter 
      ( fun (n,p)->
	  self#add_prop n p;
      ) l;

  method merge (pr:iface_properties)=
    pr#foreach_prop
      ( 
	fun n p->
	  if self#is_prop n=false then
	    self#add_prop n p;
      )

end;;




(** XML part *)
(*
<iface_properties>
 <prop_pattern name="pattern_normal" path="medias/iface/motif_bouton0.png"/>
 <prop_pattern name="pattern_clicked" path="medias/iface/motif_bouton1.png"/>
 <prop_margin name="margin_left" size="4"/>
 <prop_color name="foreground_color" r="0" g="0" b="0"/>
 <prop_align name="valign" align="left"/>
</iface_properties>
*)

class xml_iface_prop_parser=
object(self)
  inherit xml_parser as super

  val mutable name=""
  val mutable value=IPropNil

  method get_val=(name,value)

  method parse n=
    super#parse n;
    match n#get_tag with
      | "prop_pattern" -> 
	  let p=(new xml_string_parser "path") in 
	    p#parse n;
	    value<-IPropPattern (new graphic_pattern_file p#get_val)
      | "prop_pattern_fun" -> 
	  let p=(new xml_string_parser "path") in 
	    p#parse n;
	    value<-IPropPatternFun (fun()-> (new graphic_pattern_file p#get_val))
      | "prop_font" ->
	  let p=(new xml_font_parser) in 
	    p#parse n;
	    value<-IPropFont p#get_val
      | "prop_color" ->
	  let p=(new xml_color_parser) in 
	    p#parse n;
	    value<-IPropColor p#get_val
      | "prop_margin" ->
	  let p=(new xml_int_parser "size") in 
	    p#parse n;
	    value<-IPropMargin p#get_val
      | "prop_padding" ->
	  let p=(new xml_int_parser "size") in 
	    p#parse n;
	    value<-IPropPadding p#get_val
      | "prop_align" ->
	  let p=(new xml_string_parser "align") in 
	    p#parse n;
	    value<-IPropAlign
	      (
		match p#get_val with
		  | "left" -> IAlignLeft		
		  | "right" -> IAlignRight
		  | "top" -> IAlignTop
		  | "middle" -> IAlignMiddle
		  | "bottom" -> IAlignBottom
		  | _ -> IAlignNil
	      )	      

      | _ ->()


  method parse_attr k v=
    match k with
      | "name" -> name<-v
      | _ -> ();

  method parse_child k v=()

end;;

class xml_iface_props_parser=
object(self)
  inherit xml_parser as super
  val mutable props=new iface_properties

  method get_val=props

  method parse_attr k v=()
  method parse_child k v=
    let p=new xml_iface_prop_parser in
      p#parse v;
      let prop=p#get_val in
      props#add_prop (fst prop) (snd prop)

end;;

