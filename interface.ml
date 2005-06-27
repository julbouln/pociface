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


open Str;;

open Value_common;;
open Value_val;;
open Value_lua;;
open Value_xml;;

open Core_val;;
open Core_rect;;
open Core_video;;
open Core_medias;;
open Core_event;;
open Core_drawing;;
open Core_stage;;
open Core_type;;
open Core_cursor;;
open Core_type;;

open Binding;;


open Iface_object;;
open Iface_container;;
open Iface_text;;
open Iface_button;;
open Iface_theme;;
open Iface_xml;;

(** Interface handler *)

exception Iface_object_not_found of string;;

class iface_types=
object(self)
  inherit [iface_object] obj_types
end;;

(** main iface class *)
class interface=
  object (self)
    inherit [iface_object] generic_object_handler as super

    inherit lua_object as lo
    method get_id="interface"

    val mutable types=new iface_types

    val mutable canvas=new canvas

    method clear()=
      super#clear();
      canvas#clear();

    method init_default_pattern()=
      drawing_vault#add_drawing_fun_from_list "default_pattern_fun"
	(
	  fun vl->
	    let col=color_of_val (List.nth vl 0) in
	      [|default_pattern col|]
	);
  
      drawing_vault#add_drawing_fun_from_list "default_pattern_clicked_fun"
	(
	  fun vl->
	    let col=color_of_val (List.nth vl 0) in
	      [|default_pattern_clicked col|]
	);
    
      drawing_vault#add_cache_from_drawing_fun "default_pattern:simple"
	[
	  `String "default_pattern_fun"; 
	  `Color(172,172,172)
	];
      
      drawing_vault#add_cache_from_drawing_fun "default_pattern_clicked:simple"
	[
	  `String "default_pattern_clicked_fun"; 
	  `Color(128,128,128)
	];
      
      drawing_vault#add_cache_from_drawing_fun "default_pattern_text:simple"
	[
	  `String "default_pattern_fun";
	  `Color(200,200,200)
	];
    


    initializer 
      self#init_default_pattern();
      self#lua_init();()

    val mutable iface_parser=new xml_iface_parser 

    method init_all_parser p=
	p#parser_add "iface_button" (fun()->new xml_iface_button_parser);
	p#parser_add "iface_label" (fun()->new xml_iface_label_parser);
	p#parser_add "iface_button_with_label" (fun()->new xml_iface_button_with_label_parser);
	p#parser_add "iface_button_with_icon" (fun()->new xml_iface_button_with_icon_parser);
	p#parser_add "iface_text_box" (fun()->new xml_iface_text_box_parser);
	p#parser_add "iface_text_edit_box" (fun()->new xml_iface_text_edit_box_parser);
	p#parser_add "iface_text_edit" (fun()->new xml_iface_text_edit_parser);
	p#parser_add "iface_password_edit" (fun()->new xml_iface_password_edit_parser);
	p#parser_add "iface_graphic_object" (fun()->new xml_iface_graphic_object_parser);
	p#parser_add "iface_vcontainer" (fun()->new xml_iface_vcontainer_parser self#iface_get_object);
	p#parser_add "iface_hcontainer" (fun()->new xml_iface_hcontainer_parser self#iface_get_object);
	p#parser_add "iface_menu" (fun()->new xml_iface_menu_parser self#iface_get_object);
	p#parser_add "iface_menubar" (fun()->new xml_iface_menubar_parser self#iface_get_object);
	p#parser_add "iface_color_toolbox" (fun()->new    xml_iface_color_toolbox_parser);
	p#parser_add "iface_window" (fun()->new xml_iface_window_parser self#iface_get_object);
	p#parser_add "iface_window_manager" (fun()->new xml_iface_window_manager_parser self#iface_get_object);
	p#parser_add "iface_vscrollbar" (fun()->new xml_iface_vscrollbar_parser self#iface_get_object);
	p#parser_add "iface_sprite" (fun()->new xml_iface_sprite_parser);

    method init_parser()=      
      let p=iface_parser in
	p#init_parsers self#init_all_parser

    method init_from_xml f=
      let iface_xml=xml_node_from_file f in
	self#init_parser();
	iface_parser#parse iface_xml;
        iface_parser#init types#add_object_type self#iface_add_object

    method init_from_xml_node iface_xml=
	self#init_parser();
	iface_parser#parse iface_xml;
        iface_parser#init types#add_object_type self#iface_add_object

    method lua_init()=
      lua#set_val (OLuaVal.String "set_focus") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#set_focus);
      lua#set_val (OLuaVal.String "show_object") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#show_object);
      lua#set_val (OLuaVal.String "hide_object") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#hide_object);
      lua#set_val (OLuaVal.String "object_get_text") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) self#object_get_text);
      lua#set_val (OLuaVal.String "add_object_from_type") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#iface_add_object_from_type);

      lua#set_val (OLuaVal.String "add_object_from_type_to") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string **-> OLuaVal.string **->> OLuaVal.unit) self#iface_add_object_from_type_to);

      lua#set_val (OLuaVal.String "delete_object") (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#iface_del_object);
      lua#set_val (OLuaVal.String "delete_object_from") (OLuaVal.efunc (OLuaVal.string **-> OLuaVal.string  **->> OLuaVal.unit) self#iface_del_object_from);


      lo#lua_init()

    val mutable focus="none"


(** general iface functions *)

(* NEW : add iface_object from type *)
    method iface_add_object_from_type (name:string) (t:string) x y=
      let ot=types#get_object_type t in
	self#iface_add_object name (ot);
	let o=self#get_object name in
	  o#lua_init();
	  o#move x y

    method iface_add_object_from_type_to (name:string) (t:string) (c:string)=
      let ot=types#get_object_type t in
	self#iface_add_object name (ot);
	let o=self#get_object name in
	  o#lua_init();
	let co=self#get_object c in
	  co#add_child o

    method iface_add_object (name:string) (obj:iface_object)=
      self#lua_parent_of name (obj:>lua_object);
      self#add_object (Some name) obj;
      canvas#add_obj (obj:>canvas_object);

    method iface_del_object name=
      lo#get_lua#del_val (OLuaVal.String name);
      canvas#del_obj (self#iface_get_object name:>canvas_object);
      self#delete_object name;

    method iface_del_object_from (name:string) (c:string)=
      let co=self#get_object c in
	co#del_child name;
	self#iface_del_object name;


    method iface_get_object n=
      (try 
	 self#get_object n
       with Object_not_found no-> 
	   raise (Iface_object_not_found n)
      )

    method iface_is_object n=
      self#is_object n
    

(** functions on object *)

    method object_get_text n=
      let o=self#iface_get_object n in
	o#get_data_text

    method object_set_text n t=
      let o=self#iface_get_object n in
	o#set_data_text t


    method set_focus f=
      focus<-f;
      self#unfocus_all();
      if f<>"none" then (
      let o=self#iface_get_object focus in
	o#set_focused true;
      )
    method get_focus=focus

    method unfocus_all()=
      let f n obj=obj#set_focused false in
	self#foreach_object f;

    method show_object n=
      let o=self#iface_get_object n in
	o#show();

    method hide_object n=
      let o=self#iface_get_object n in
	o#hide();

    method show_all()=
      let f n obj=obj#show() in
      self#foreach_object f;


    method hide_all()=
      let f n obj=obj#hide() in
      self#foreach_object f;

    method iface_get_object_name_at_position x y=
      let l=ref 0 in
      let t=ref ("none") in
      let f n obj=
	if x > obj#get_vrect#get_x 
	    && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	    && y > obj#get_vrect#get_y 
	    && y < (obj#get_vrect#get_h + obj#get_vrect#get_y) 
	    && obj#is_showing==true 
	then (
	  if !l<=obj#get_layer then (
	    t:=n;
	    l:=obj#get_layer;
	  )
	)
      in
      self#foreach_object f;
      !t


    method iface_foreach_object_at_position x y f=
      let l=ref 0 in
      let t=ref ("none") in
      let f n obj=
	if x > obj#get_vrect#get_x 
	    && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	    && y > obj#get_vrect#get_y 
	    && y < (obj#get_vrect#get_h + obj#get_vrect#get_y) 
	    && obj#is_showing==true 
	then (
	  f obj
	)
      in

      let s p1 p2=
	let (k1,o1)=p1 and
	    (k2,o2)=p2 in
	  (match o1#get_layer with
	    | x when x < o2#get_layer -> -1
	    | x when x = o2#get_layer -> 0
	    | x when x > o2#get_layer -> 1
	    | _ -> 0)
      in
	self#foreach_object_sorted s f;

(*	self#foreach_object f *)

    method iface_get_object_at_position x y=
      self#iface_get_object (self#iface_get_object_name_at_position x y)

(** iface events *)

    method mouseover x y=
      let mo=DynArray.create() in
	self#iface_foreach_object_at_position x y (
	fun obj->
	  if obj#is_showing==true then 
	    (
	      DynArray.add mo obj#get_id;
	      obj#on_mouseover x y; 
	      ignore(obj#get_lua#exec_val_fun (OLuaVal.String "on_mouseover") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];)
	    );
	);
	
	let is_mo n=
	  let r=ref false in
	    DynArray.iter
	      ( fun cn->
		  if cn=n then r:=true
	      ) mo;
	    !r
	in

	let f i obj=
	  if is_mo i=false  then 
            obj#on_mouseout x y in     

	  self#foreach_object f;

    method mouseout x y=
      self#foreach_object (
	fun k o->
	  if o#is_showing==true then ( 
	    o#on_mouseout x y;
	    ignore(o#get_lua#exec_val_fun (OLuaVal.String "on_mouseout") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];)
	  )
      )

    method click x y=
      self#iface_foreach_object_at_position x y (
	fun o->
	  if o#is_showing==true then ( 
	    if o#grab_focus then
	      self#set_focus o#get_id;
	    
	    o#on_click x y;
	    ignore(o#get_lua#exec_val_fun (OLuaVal.String "on_click") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];)
	      
	  )
      )


    method release x y=
      let n=(self#iface_get_object_name_at_position x y) in

      let f i obj=
	if i=n then (
	  if obj#is_showing==true then (
	    obj#on_release x y;
	    ignore(obj#get_lua#exec_val_fun (OLuaVal.String "on_release") [OLuaVal.Number (float_of_int x);OLuaVal.Number (float_of_int y)];)
	  );
	);
	let ro=obj#get_release in
	  obj#set_release (function()->());
	  
	  obj#on_release x y;
	  obj#set_release ro in	
	
	self#foreach_object f;



    method keypress e=
      if self#get_focus<>"none" then (
	let o=self#iface_get_object self#get_focus in
	  if o#is_showing==true then (
	    o#on_keypress e;
	    ignore(o#get_lua#exec_val_fun (OLuaVal.String "on_keypress") [OLuaVal.String (string_of_key (fst e))])
	  )
      )

    method keyrelease e=
      if self#get_focus<>"none" then (
	let o=self#iface_get_object self#get_focus in
	  if o#is_showing==true then (
	    o#on_keyrelease e;
	    ignore(o#get_lua#exec_val_fun (OLuaVal.String "on_keyrelease") [OLuaVal.String (string_of_key (fst e))])
	  )
      )
    
    method get_data x y=
      (self#iface_get_object_at_position x y)#get_data;

    method set_data x y d=
      (self#iface_get_object_at_position x y)#set_data d;


    method move_all x y=
      self#foreach_object (fun n o->
		      let ox=o#get_rect#get_x and oy=o#get_rect#get_y in
			o#move (ox + x) (oy + y)
		   );
      

    method update()=      
      canvas#refresh 0 0 0 0;


	if focus<> "none" then (
	  let fo=self#iface_get_object focus in
	    if fo#is_showing then (
(*	      let t=tile_rect (fo#get_rect#get_w+2) (fo#get_rect#get_h+2) (200,200,200) in
		tile_put t (fo#get_rect#get_x-1) (fo#get_rect#get_y-1);
		tile_free t;
*)
	    )

	)
  end;;

open Core_graphic;;

class iface_stage curs (v:xml_node)=
object(self)
  inherit stage curs as super

  val mutable iface=new interface 
  method get_iface=iface

  (** graphic echange *)
  method get_graphic (id:string) (gid:string)=
    let o=iface#get_object id in
      o#get_graphic gid
  method add_graphic (id:string) (gid:string) (go:graphic_object)=
    let o=iface#get_object id in
      o#add_graphic gid go
  method delete_graphic (id:string) (gid:string)=
    let o=iface#get_object id in
      o#delete_graphic gid


  method on_load()=
    iface#clear();
    iface#init_from_xml_node v;
(*    iface#init_from_xml file *)
    super#on_load();

(*  method on_leave()=
    iface#clear();
*)
  method on_loop()=
    super#on_loop();


  method on_loop_graphic()=
    iface#update();

  method ev_parser e=
    super#ev_parser e;
    (match e with
       | EventMouse em ->
	   (match em with
	      | MouseMotion(x,y) -> 
		  iface#mouseover x y;
	      | MouseRelease(x,y,but) -> 
		  iface#release x y; 
	      | MouseClick(x,y,but) -> 
		  iface#click x y; 
	      | _ -> ()
	   )
       | EventKeyboard ek->
	 (match ek with
	    | KeyboardPress (k,uk)-> 
		iface#keypress (k,uk)
	    | KeyboardRelease (k,uk)-> 
		iface#keyrelease (k,uk)
	    | _ -> ()
	 )
       | _ -> ()
    )

  method lua_init()=
    self#lua_parent_of "iface" (iface:>lua_object);
    super#lua_init();
  
end;;

open Core_stage;;
open Core_xml;;

class xml_iface_stage_parser=
object (self)
  inherit xml_stage_parser as super

  val mutable ifa=new xml_node

  method parse_child k v=
    super#parse_child k v;
    match k with
      | "iface" -> ifa<-v
      | _ ->()
(** object initial init *)
  method init_object o=
    o#set_lua_script (lua);

  method get_val=
    let ofun()=
      let o=
	self#init_cursor();
	new iface_stage curs ifa
      in
	self#init_object (o:>stage);
	(o:>stage)	  
    in      
      (id,ofun)

end;;

let xml_iface_stages_parser()=
  let p=xml_factory_stages_parser() in
    p#parser_add "iface_stage" (fun()->new xml_iface_stage_parser);
    p;;

