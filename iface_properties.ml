(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Value_xml;;
open Value_xmlparser;;

open Core_xml;;
open Core_font;;
open Core_medias;;
open Core_graphic;;


(** Interface properties *)
(** Properties are used in all interface object to configure them *)


(** {2 Exceptions} *)

exception Bad_iface_prop;;
exception Iface_prop_not_found of string;;

(** {2 Types} *)

(** Alignement type *)
type iface_align=
  | IAlignTop
  | IAlignBottom
  | IAlignLeft
  | IAlignRight
  | IAlignMiddle
  | IAlignNil
;;

(** Property type *)
type iface_prop=
  | IPropBool of bool
  | IPropColor of (int*int*int)

  | IPropAlign of iface_align
  | IPropPadding of int
  | IPropMargin of int

  | IPropGraphic of (unit-> graphic_object)
  | IPropPattern of (unit -> graphic_pattern)
  | IPropFont of (font_t)

  | IPropNil;;

(** {2 Type convertion functions} *)

let iprop_align p=match p with
  | IPropAlign x->x
  | _ -> raise Bad_iface_prop;;

let iprop_padding p=match p with
  | IPropPadding x->x
  | _ -> raise Bad_iface_prop;;

let iprop_margin p=match p with
  | IPropMargin x->x
  | _ -> raise Bad_iface_prop;;

let iprop_graphic p=match p with
  | IPropGraphic x->x()
  | _ -> raise Bad_iface_prop;;

let iprop_pattern p=match p with
  | IPropPattern x->x()
  | _ -> raise Bad_iface_prop;;

let iprop_pattern_fun p=match p with
  | IPropPattern x->x
  | _ -> raise Bad_iface_prop;;
      
let iprop_color p=match p with
  | IPropColor x->x
  | _ -> raise Bad_iface_prop;;

let iprop_font p=match p with
  | IPropFont x->x
  | _ -> raise Bad_iface_prop;;

let iprop_bool p=match p with
  | IPropBool x->x
  | _ -> raise Bad_iface_prop;;


(** {2 Classes} *)

(** Properties management main class *)
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

(** xml iface property parser *)
class xml_iface_prop_parser drawing_vault=
object(self)
  inherit xml_parser as super

  val mutable name=""
  val mutable value=IPropNil

  method get_val=(name,value)

  method parse n=
    super#parse n;
    match n#tag with
      | "prop_graphic" -> 
	  let p=(new xml_string_parser "path") in 
	    p#parse n;
	    let p2=(new xml_size_parser) in 
	      p2#parse n;
	    value<-IPropGraphic (fun()->(new graphic_from_file drawing_vault p#get_val p2#get_w p2#get_h))
      | "prop_pattern" -> 
	  let p=(new xml_string_parser "path") in 
	    p#parse n;
	    value<-IPropPattern (fun()-> (new graphic_pattern_file drawing_vault p#get_val))
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

      | "prop_bool" ->
	  let p=(new xml_string_parser "is") in 
	    p#parse n;
	    value<-IPropBool (match p#get_val with
				| "true" -> true
				| "false" -> false
				| _ -> false)

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

(** xml iface properties parser *)
class xml_iface_props_parser drawing_vault=
object(self)
  inherit xml_parser as super
  val mutable props=new iface_properties

  method get_val=props

  method parse_attr k v=()
  method parse_child k v=
    let p=new xml_iface_prop_parser drawing_vault in
      p#parse v;
      let prop=p#get_val in
      props#add_prop (fst prop) (snd prop)

end;;

