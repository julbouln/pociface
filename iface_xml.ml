(*
    pociface - create your own interface
    Copyright (C) 2003,2004,2005 POC 

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

open Video;;

open Oxml;;
open Medias;;
open Olua;;

open Iface_properties;;
open Iface_object;;
open Iface_text;;
open Iface_button;;
open Iface_container;;
open Iface_menu;;
open Iface_window;;


open Iface_theme;;


(** generic iface object parser *)
class xml_iface_object_parser=
object (self)
  inherit xml_parser
  val mutable layer=0
  val mutable nm=""
  method get_type=nm

  val mutable id=""
  val mutable x=0
  val mutable y=0
  val mutable w=0
  val mutable h=0
  val mutable show=false

  val mutable lua=""

  val mutable props=new iface_properties

  val mutable theme=new iface_theme (Hashtbl.create 2)
  method set_theme t=theme<-t

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | "id" ->id<-v
      | _ -> ()
   
  method parse_child k v=
    match k with
      | "properties" -> let p=(new xml_iface_props_parser) in p#parse v;props<-p#get_val
      | "layer" -> let p=(new xml_int_parser "pos" ) in p#parse v;layer<-p#get_val;
      | "size" -> let p=(new xml_size_parser ) in p#parse v;w<-p#get_w;h<-p#get_h;
      | "position" -> let p=(new xml_point_parser ) in p#parse v;x<-p#get_x;y<-p#get_y;
      | "script" -> lua<-v#get_pcdata;
      | "show" -> show<-true

      | _ -> ()

  method init_object o=
    o#set_layer layer;
    o#move x y;
    if show then
      o#show();
    o#move (video#f_size_w x) (video#f_size_h y);
    o#set_lua lua;
    
  method get_val=
    let ofun()=
      let o=new iface_object w h in
	self#init_object o;
	o	  
    in      
      (id,ofun)

end;;




(** iface button parser *)
class xml_iface_button_parser=
object(self)
  inherit xml_iface_object_parser as super

  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
	let o=
	  new iface_pbutton id 
	    (iprop_pattern (props#get_prop "pattern_normal")) 
	    (iprop_pattern (props#get_prop "pattern_clicked")) 
	in
	super#init_object o;
	o
    in

      (id,ofun)
end;;

(** iface button with label parser *)
class xml_iface_button_with_label_parser=
object(self)
  inherit xml_iface_object_parser as super
 
  val mutable txt=""

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "text" -> let p=(new xml_string_parser "str") in p#parse v;txt<-p#get_val
      | _ -> ()    

  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=
	new iface_pbutton_with_label id 
	    (iprop_pattern (props#get_prop "pattern_normal")) 
	    (iprop_pattern (props#get_prop "pattern_clicked")) 
	    (iprop_font (props#get_prop "font")) 
	    (iprop_color (props#get_prop "foreground_color")) 
	  txt in
	super#init_object o;
	o
    in
      (id,ofun)
end;;

(** iface label parser *)
class xml_iface_label_parser=
object(self)
  inherit xml_iface_object_parser as super
 
  val mutable txt=""

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "text" -> let p=(new xml_string_parser "str") in p#parse v;txt<-p#get_val
      | _ -> ()    

  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=
	new iface_label_static 
	  (iprop_font (props#get_prop "font")) 
	  (iprop_color (props#get_prop "foreground_color")) 
	  txt
      in
	  super#init_object o;
	o
      in
	(id,ofun)
end;;


(** iface text edit parser *)
class xml_iface_text_edit_parser=
object(self)
  inherit xml_iface_object_parser as super
 
  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=	
	new iface_text_edit id 
	    (iprop_pattern (props#get_prop "pattern")) 
	    (iprop_font (props#get_prop "font")) 
	    (iprop_color (props#get_prop "foreground_color")) 
	  (video#f_size_w w) in
	super#init_object o;
	o
    in
      (id,ofun)
end;;

(** iface text edit box parser *)
class xml_iface_text_box_parser=
object(self)
  inherit xml_iface_object_parser as super

  val mutable l=1

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "lines" -> let p=(new xml_int_parser "n") in p#parse v;l<-p#get_val
      | _ -> ()    

 
  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=
	new iface_text_box id 
	  (iprop_pattern (props#get_prop "pattern")) 
	  (iprop_font (props#get_prop "font")) 
	  (iprop_color (props#get_prop "foreground_color")) 
	  (video#f_size_w w) l in
	super#init_object o;
	o
    in
      (id,ofun)
end;;


(** iface text edit box parser *)
class xml_iface_text_edit_box_parser=
object(self)
  inherit xml_iface_object_parser as super

  val mutable l=1

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "lines" -> let p=(new xml_int_parser "n") in p#parse v;l<-p#get_val
      | _ -> ()    

 
  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=	
	new iface_text_edit_box id 
	  (iprop_pattern (props#get_prop "pattern")) 
	  (iprop_font (props#get_prop "font")) 
	  (iprop_color (props#get_prop "foreground_color")) 
	  (video#f_size_w w) l in
	super#init_object o;
	o
    in
      (id,ofun)
end;;

(** iface password edit parser *)
class xml_iface_password_edit_parser=
object(self)
  inherit xml_iface_object_parser as super
 
  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=
	new iface_password_edit id 
	  (iprop_pattern (props#get_prop "pattern")) 
	  (iprop_font (props#get_prop "font")) 
	  (iprop_color (props#get_prop "foreground_color")) 
	  (video#f_size_w w) in
	super#init_object o;
	o
    in
      (id,ofun)
end;;

(** iface graphic object parser *)
class xml_iface_graphic_object_parser=
object(self)
  inherit xml_iface_object_parser as super

  val mutable file="none"

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "file" -> let p=(new xml_string_parser "path") in p#parse v;file<-p#get_val    
      | _ -> ()    
 
  method get_val=
    let ofun()=
      let o=new iface_graphic_file_object file w h in
	super#init_object o;
	o
    in
      (id,ofun)
end;;

(** iface vcontainer parser *)
class xml_iface_vcontainer_parser get_obj=
object(self)
  inherit xml_iface_object_parser as super

  val mutable container=Array.create 1 "none"

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "container" -> let p=(new xml_stringlist_parser "iface_object" (fun()->new xml_string_parser "id")) in p#parse v;container<-p#get_array
      | _ -> ()    
 
  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=new iface_vcontainer 
	      (
		let a=DynArray.create() in
		  Array.iter
		    (
			fun n->
			  let co=(get_obj n) in
			  DynArray.add a co
		    ) container;
		  DynArray.to_array a
		      
	      ) in
	  o#set_valign (iprop_align (props#get_prop "valign"));
	  o#set_halign (iprop_align (props#get_prop "halign"));
	  o#set_fixed_size (iprop_bool (props#get_prop "fixed_size"));
(*
	  if (iprop_bool (props#get_prop "fixed_size")) then (
	    print_string "fixed_size!";print_newline()
	  ) else (
	    print_string "non fixed_size!";print_newline()
	  );	    
*)
	  o#reset_size();
	super#init_object (o:>iface_object);
	(o:>iface_object)
    in
      (id,ofun)
end;;

(** iface hcontainer parser *)
class xml_iface_hcontainer_parser get_obj=
object(self)
  inherit xml_iface_object_parser as super

  val mutable container=Array.create 1 "none"

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "container" -> let p=(new xml_stringlist_parser "iface_object" (fun()->new xml_string_parser "id")) in p#parse v;container<-p#get_array
      | _ -> ()    
 
  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=new iface_hcontainer 
	      (
		let a=DynArray.create() in
		  Array.iter
		    (
			fun n->
			  let co=(get_obj n) in
			  DynArray.add a co
		    ) container;
		  DynArray.to_array a
		      
	      ) in
	  o#set_valign (iprop_align (props#get_prop "valign"));
	  o#set_halign (iprop_align (props#get_prop "halign"));
	  o#set_fixed_size (iprop_bool (props#get_prop "fixed_size"));
	super#init_object (o:>iface_object);
	(o:>iface_object)
    in
      (id,ofun)
end;;

(** iface menu parser *)
type xml_iface_menu_t=
  | IMenu of (string * xml_iface_menu_t list)
  | IMenuEntry of string;;

class xml_iface_menu_t_parser=
object
  inherit xml_parser
  
  val mutable nid="none"
  method get_nid=nid

  val mutable childs=DynArray.create()

(*  val mutable menu_t=IMenuEntry "none" *)
  method get_val=(nid,DynArray.to_list childs)
 
  method parse_attr k v=
    match k with
      | "id" ->nid<-v
      | _ -> ()

  method parse_child k v=
    match k with
      | "menu" -> let p=new xml_iface_menu_t_parser in p#parse v;DynArray.add childs (IMenu p#get_val)
      | "menu_entry" -> let p=new xml_string_parser "id" in p#parse v;DynArray.add childs (IMenuEntry p#get_val)
      | _ -> ()


end;;

let to_menu mt layer get_obj=
    let rec xmlmenu_to_menu men=
      match men with
	| IMenu (n,m)->
(*	    print_string ("parse MENU "^n);print_newline(); *)
	    
	    let co=(get_obj n) in
	      Menu (co,
		    let a=DynArray.create() in
		      List.iter (
			fun cm->
			  DynArray.add a (xmlmenu_to_menu cm)
		      ) m;
		      let l=DynArray.to_list a in List.rev l
		   )
	| IMenuEntry n->
(*	    print_string ("parse MENUENTRY "^n);print_newline();*)
	    MenuEntry (
	      let co=(get_obj n) in
		co
	    )
    in
    let menu=xmlmenu_to_menu (IMenu mt) in
      menu;;

(** iface graphic object parser *)
class xml_iface_menu_parser get_obj=
object(self)
  inherit xml_iface_object_parser as super

  val mutable menu_t=("none",[])


  method parse_child k v=
    super#parse_child k v;
    match k with
      | "menu" -> let p=(new xml_iface_menu_t_parser) in p#parse v;menu_t<-p#get_val
      | _ -> ()    
 
  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=
	new iface_menu id 
	  (iprop_pattern_fun (props#get_prop "pattern")) 
	  MenuRight 
	  (match to_menu menu_t layer get_obj with Menu m->m) in
	super#init_object (o:>iface_object);
	(o:>iface_object)
    in
      (id,ofun)
end;;


class xml_iface_menu_t_list_parser=
object(self)
  inherit [string * xml_iface_menu_t list,xml_iface_menu_t_parser] xml_list_parser "menu" (fun()->new xml_iface_menu_t_parser) 
end;;


(** iface graphic object parser *)
class xml_iface_menubar_parser get_obj=
object(self)
  inherit xml_iface_object_parser as super

  val mutable menu_t_arr=[||]

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "menubar" -> let p=(new xml_iface_menu_t_list_parser) in p#parse v;menu_t_arr<-p#get_array
      | _ -> ()    
 
  method get_val=
    let ofun()=

      let st=theme#get_style nm in
	props#merge st;
      let o=new iface_menubar id 
	(iprop_pattern_fun (props#get_prop "pattern")) 
	(
	  let a=DynArray.create() in
	  Array.iter (
	    fun menu ->
	      DynArray.add a (to_menu menu layer get_obj)
	  ) menu_t_arr;
	    let l=DynArray.to_list a in List.rev l

	) in
	super#init_object (o:>iface_object);
	(o:>iface_object)
    in
      (id,ofun)
end;;


(** iface window parser *)
class xml_iface_window_parser get_obj=
object(self)
  inherit xml_iface_object_parser as super

  val mutable cid=""
  val mutable txt=""
  method parse_child k v=
    super#parse_child k v;
    match k with
      | "iface_object" -> let p=(new xml_string_parser "id") in p#parse v;cid<-p#get_val
      | "text" -> let p=(new xml_string_parser "str") in p#parse v;txt<-p#get_val
      | _ -> ()    

 
  method get_val=
    let ofun()=
      let st=theme#get_style nm in
	props#merge st;
      let o=	
	new iface_window id 
	  (iprop_pattern (props#get_prop "pattern_title")) 
	  (iprop_pattern (props#get_prop "pattern_title_min")) 
	  (iprop_pattern (props#get_prop "pattern_background")) 
	  (iprop_font (props#get_prop "font")) 
	  (iprop_color (props#get_prop "foreground_color")) 
	  (iprop_graphic (props#get_prop "close_button"))
	  (iprop_graphic (props#get_prop "minimize_button"))
	  (iprop_graphic (props#get_prop "maximize_button"))
	  txt
	  (get_obj cid) in
	super#init_object o;
	o
    in
      (id,ofun)
end;;



exception Xml_iface_parser_not_found of string;;

class xml_iface_parser=
object(self)
  inherit xml_parser

  val mutable objs=DynArray.create()

  val mutable obj_parsers=Hashtbl.create 2
  method parser_add (n:string) (p:unit->xml_iface_object_parser)=Hashtbl.add obj_parsers n p
  method parser_is n=Hashtbl.mem obj_parsers n
  method parser_get n=
    (try
       Hashtbl.find obj_parsers n
     with
	 Not_found -> raise (Xml_iface_parser_not_found n))

  val mutable theme=new iface_theme (Hashtbl.create 2)
  method get_style s=theme#get_style s    
  
  method parse_attr k v=
    match k with
      | "theme" -> theme<-iface_theme_from_xml v
      | _ -> ()

  method parse_child k v=
    match k with
      | "iface_object" -> let p=new xml_iface_object_parser in p#parse v;
	  if self#parser_is p#get_type then
	    let sp=(self#parser_get p#get_type)() in
	      sp#set_theme theme;
	      sp#parse v;
	      DynArray.add objs sp#get_val
      | _ ->()

  method init (add_obj:string->iface_object->unit) (get_interp:lua_interp)=
    DynArray.iter (
      fun (n,o)->
	print_string ("IFACE_XML: add object "^n);print_newline();
	let no=o() in	  	  
	  add_obj n (no);				 
	  no#lua_register get_interp;
    ) objs;
      
end;;
