(*
    Battle For Rashitoul - The ultimate strategy/arcade game
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

open Rect;;
open Low;;
open Video;;
open Medias;;
open Music;;
open Event_manager;;

open Otype;;
open Olua;;

open Iface_object;;
open Iface_container;;
open Iface_text;;
open Iface_button;;

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
	 ignore (interp#parse (o#get_id^".on_click("^string_of_int x^","^string_of_int y^")")) ;
	o#on_click x y;
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
	 ignore (interp#parse (o#get_id^".on_release("^string_of_int x^","^string_of_int y^")")) ;
      o#on_release x y;
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

