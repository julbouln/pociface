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

open Str;;

open Generic;;
open Rect;;
open Low;;
open Video;;
open Medias;;
open Music;;
open Event_manager;;

open Otype;;
open Olua;;
open Oxml;;

open Iface_object;;
open Iface_container;;
open Iface_text;;
open Iface_button;;
open Iface_theme;;
open Iface_xml;;
(** GUI objects class definitions *)


exception Iface_object_not_found of string;;

(** main iface class *)
class interface bgfile w h=
  object (self)
    val mutable background=new graphic_scr_resized_object w h bgfile false false 
    val mutable interp=new lua_interp
    method get_interp=interp
 
    val mutable object_array=Array.make 1000 (new iface_object 32 32) 
    val mutable cur_object=1
    val mutable object_hash=let a =Hashtbl.create 2 in Hashtbl.add a "none" 0;a
    val mutable effect_a=[|0;1;2;3;4|]
    val mutable effect=0;
    val mutable nrect=new rectangle 0 0 0 0;
    val mutable moving=false

    initializer 
      self#init_lua()


    method init_lua()=
      interp#set_module_val "iface" "set_focus" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#set_focus);
      interp#set_module_val "iface" "show_object" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#show_object);
      interp#set_module_val "iface" "hide_object" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#hide_object);
      interp#set_module_val "iface" "object_get_text" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) self#object_get_text);


    val mutable focus="none"

    method set_focus f=
      focus<-f;
      self#unfocus_all();
      let o=self#get_object_char focus in
	o#set_focused true;

    method get_focus=focus


    method unfocus_all()=
      let f obj=obj#set_focused false in
      Array.iter f object_array;


    method get_moving=moving

    method set_effect n=
      effect<-n;


    method get_cur_obj=cur_object

      
    method object_get_text n=
      let o=self#get_object_char n in
	o#get_data_text

    method object_set_text n t=
      let o=self#get_object_char n in
	o#set_data_text t


    method show_object n=
      let o=self#get_object_char n in
	o#show();

    method hide_object n=
      let o=self#get_object_char n in
	o#hide();


    method show_all()=
      let f obj=obj#show() in
      Array.iter f object_array;


    method hide_all()=
      let f obj=obj#hide() in
      Array.iter f object_array;

    method get_object_num_at_position x y=

      let t=ref (0) in
      let f i obj=
	if x > obj#get_vrect#get_x 
	    && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	    && y > obj#get_vrect#get_y 
	    && y < (obj#get_vrect#get_h + obj#get_vrect#get_y) 
	    && obj#is_showing==true 
	then
	  t:=i;
      in
      Array.iteri f object_array;
      !t

    method get_object_at_position x y=
      self#get_object_num (self#get_object_num_at_position x y)

    method get_object_num n=object_array.(n)

    method get_object_char n=object_array.(try Hashtbl.find object_hash n with Not_found -> raise (Iface_object_not_found n))
    method is_object n=(Hashtbl.mem object_hash n)
    method get_object_hash=object_hash

    method add_object obj=
      object_array.(cur_object)<-obj;
      cur_object<-cur_object+1

    method add_object_n name obj=
      obj#set_id name;
      Hashtbl.add object_hash name cur_object;
      object_array.(cur_object)<-obj;
      cur_object<-cur_object+1

    method del_object num=
      Array.blit object_array (num+1) object_array (num) (cur_object - num);
      cur_object<-cur_object-1

    method mouseover x y=
      let o=(self#get_object_at_position x y) in
      if o#is_showing==true then 
	(
(*	ignore (interp#parse (o#get_id^".on_mouseover("^string_of_int x^","^string_of_int y^")")) ; *)
	o#on_mouseover x y; 
	);
      let n=self#get_object_num_at_position x y in

      let f i obj=
	if i<> n then
         obj#on_mouseout x y in


      Array.iteri f object_array;


    method mouseout x y=
      let o=(self#get_object_at_position x y) in
      if o#is_showing==true then ( 
(*	ignore (interp#parse (o#get_id^".on_mouseout("^string_of_int x^","^string_of_int y^")")) ; *)
	o#on_mouseout x y;
      )
    method click x y=
      let o=(self#get_object_at_position x y) in
      if 
	o#is_showing==true then ( 
	  o#on_click x y;
	  ignore (interp#parse (o#get_id^".on_click("^string_of_int x^","^string_of_int y^")")) ;

      )

    method keypress e=
      let o=self#get_object_char self#get_focus in
	if o#is_showing==true then (
(*	 ignore (interp#parse (o#get_id^".on_keypress("^string_of_int x^","^string_of_int y^")")) ; *)
	o#on_keypress e;
	)

    method release x y=
      let o=(self#get_object_at_position x y) in
      if o#is_showing==true then (
	o#on_release x y;
	ignore (interp#parse (o#get_id^".on_release("^string_of_int x^","^string_of_int y^")")) ;

  );
      let f i obj=
	let ro=obj#get_release in
	 obj#set_release (function()->());

	 obj#on_release x y;
	obj#set_release ro in	
      Array.iteri f object_array;

    
    method get_data x y=
      (self#get_object_at_position x y)#get_data;

    method set_data x y d=
      (self#get_object_at_position x y)#set_data d;


    method move_all x y=
      moving<-true;
      nrect#set_position x y;
      let bx=background#get_rect#get_x and
	  by=background#get_rect#get_y in
      background#move (bx + x) (by + y);

      for i=0 to cur_object do
	let o=object_array.(i) in
	let ox=o#get_rect#get_x and oy=o#get_rect#get_y in
	    o#move (ox + x) (oy + y)
      done;
      
    method rewind_all()=
      moving<-false;
      let bx=background#get_rect#get_x and
	  by=background#get_rect#get_y in
      background#move (bx - nrect#get_x) (by - nrect#get_y);

      for i=0 to cur_object do
	let o=	object_array.(i) in
	let ox=o#get_rect#get_x and oy=o#get_rect#get_y in
	    o#move (ox - nrect#get_x) (oy - nrect#get_y)
      done;
      nrect#set_position 0 0;

    method update()=      
      background#put();
      let f obj=
	obj#put()
	 in
      Array.iter f object_array;

	if focus<> "none" then (
	  let fo=self#get_object_char focus in
	    if fo#is_showing then (
	  let t=tile_rect (fo#get_rect#get_w+2) (fo#get_rect#get_h+2) (0,0,0) in
	    tile_put t (fo#get_rect#get_x-1) (fo#get_rect#get_y-1);
	    tile_free t;
	    )
	)
  end;;

class iface_object_types=
object
  inherit [iface_object] obj_types (new iface_object 0 0)
end;;

(* some functions *)
let iface_add_object iface obj=
  iface#add_object (obj);
  let nbut=iface#get_cur_obj - 1 in
  let o=(iface#get_object_num nbut) in
  o;;


(* NEW *)

(** main iface class *)
class interface_NEW=
  object (self)
    inherit [iface_object] generic_object_handler
    val mutable canvas=new canvas_NEW

    val mutable interp=new lua_interp
    method get_interp=interp

    initializer 
      self#init_lua()

    val mutable iface_parser=new xml_iface_parser 
    method iface_add_xml_parser p f=iface_parser#parser_add p f

    method init_from_xml f=
      let iface_xml=new xml_node (Xml.parse_file f) in
      let p=iface_parser in
	p#parser_add "iface_button" (fun()->new xml_iface_button_parser);
	p#parser_add "iface_label" (fun()->new xml_iface_label_parser);
	p#parser_add "iface_button_with_label" (fun()->new xml_iface_button_with_label_parser);
	p#parser_add "iface_text_edit_box" (fun()->new xml_iface_text_edit_box_parser);
	p#parser_add "iface_text_edit" (fun()->new xml_iface_text_edit_parser);
	p#parser_add "iface_password_edit" (fun()->new xml_iface_password_edit_parser);
	p#parser_add "iface_graphic_object" (fun()->new xml_iface_graphic_object_parser);
	p#parser_add "iface_vcontainer" (fun()->new xml_iface_vcontainer_parser self#iface_get_object);
	p#parser_add "iface_hcontainer" (fun()->new xml_iface_hcontainer_parser self#iface_get_object);
	p#parser_add "iface_menu" (fun()->new xml_iface_menu_parser self#iface_get_object);
	p#parser_add "iface_menubar" (fun()->new xml_iface_menubar_parser self#iface_get_object);
	p#parse iface_xml;
        p#init self#iface_add_object self#get_interp

    method init_lua()=
      interp#set_module_val "iface" "set_focus" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#set_focus);
      interp#set_module_val "iface" "show_object" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#show_object);
      interp#set_module_val "iface" "hide_object" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.unit) self#hide_object);
      interp#set_module_val "iface" "object_get_text" (OLuaVal.efunc (OLuaVal.string **->> OLuaVal.string) self#object_get_text);
      interp#set_global_val "exit" (OLuaVal.efunc (OLuaVal.int **->> OLuaVal.unit) exit);
      
    val mutable focus="none"

(** general iface functions *)

    method iface_add_object (name:string) (obj:iface_object)=
      self#add_object (Some name) obj;
      canvas#add_obj (obj:>canvas_object);

    method iface_del_object name=
      self#delete_object name;
      canvas#del_obj (self#iface_get_object name:>canvas_object)

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
      self#foreach_object f;


    method iface_get_object_at_position x y=
      self#iface_get_object (self#iface_get_object_name_at_position x y)

(** iface events *)

    method mouseover x y=
(*
	let f i obj=
	  if i<>n then 
            obj#on_mouseout x y in     
	  Hashtbl.iter f objects;
*)
      let mo=DynArray.create() in
      self#iface_foreach_object_at_position x y (
	fun obj->
	  if obj#is_showing==true then 
	    (
	      DynArray.add mo obj#get_id;
	      obj#on_mouseover x y; 
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
(*      let o=(self#iface_get_object_at_position x y) in *)
(*      self#iface_foreach_object_at_position x y ( *)
      self#foreach_object (
	fun k o->
	  if o#is_showing==true then ( 
	    o#on_mouseout x y;
	  )
      )

    method click x y=
(*      let o=(self#iface_get_object_at_position x y) in *)
      self#iface_foreach_object_at_position x y (
	fun o->
      if o#is_showing==true then ( 
	o#on_click x y;
	ignore (interp#parse (o#get_id^".on_click("^string_of_int x^","^string_of_int y^")")) ;

      )
      )


    method release x y=
      let n=(self#iface_get_object_name_at_position x y) in

      let f i obj=
	if i=n then (
	  if obj#is_showing==true then (
	    obj#on_release x y;
	    ignore (interp#parse (obj#get_id^".on_release("^string_of_int x^","^string_of_int y^")")) ;
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
(*	    ignore (interp#parse (o#get_id^".on_keypress("^string_of_int x^","^string_of_int y^")")) ;  *)
	    o#on_keypress e;
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
	      let t=tile_rect (fo#get_rect#get_w+2) (fo#get_rect#get_h+2) (200,200,200) in
		tile_put t (fo#get_rect#get_x-1) (fo#get_rect#get_y-1);
		tile_free t;
	    )
	)
  end;;



class xml_interface_parser=
object(self)
  inherit xml_parser

  val mutable iface=new interface_NEW 
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
  
  method get_iface=iface

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

  method get_val=
      DynArray.iter (fun ol->
		       List.iter (
			 fun (n,l,o)->
			   print_string ("IFACE_XML: add object "^n);print_newline();
			   iface#iface_add_object n (o());				 
			   
			   let l2=(n^"={};\n")^l in
			     print_string l2;
			     iface#get_interp#parse l2;()
		       )ol;
		       ) objs;
    iface
      
end;;




let iface_from_xml f=
    let iface_xml=new xml_node (Xml.parse_file f) in
    let p=new xml_interface_parser in
      p#parser_add "iface_button" (fun()->new xml_iface_button_parser);
      p#parser_add "iface_label" (fun()->new xml_iface_label_parser);
      p#parser_add "iface_button_with_label" (fun()->new xml_iface_button_with_label_parser);
      p#parser_add "iface_text_edit" (fun()->new xml_iface_text_edit_parser);
      p#parser_add "iface_password_edit" (fun()->new xml_iface_password_edit_parser);
      p#parser_add "iface_graphic_object" (fun()->new xml_iface_graphic_object_parser);
      p#parser_add "iface_vcontainer" (fun()->new xml_iface_vcontainer_parser p#get_iface#iface_get_object);
      p#parser_add "iface_hcontainer" (fun()->new xml_iface_hcontainer_parser p#get_iface#iface_get_object);
      p#parser_add "iface_menu" (fun()->new xml_iface_menu_parser p#get_iface#iface_get_object);
      p#parser_add "iface_menubar" (fun()->new xml_iface_menubar_parser p#get_iface#iface_get_object);
      p#parse iface_xml;
      
      p#get_val;;




