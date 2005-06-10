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

open Value_lua;;

open Core_rect;;
open Core_medias;;

open Core_event;;

open Iface_object;;
open Iface_text;;


(** Interface buttons *)

(** sample graphic button object *)
class iface_graphic_button gr w h=
  object
    inherit iface_graphic_object gr w h as super
    val mutable is_clicked=false
    val mutable is_mouseover=false

    method on_mouseover x y=super#on_mouseover x y;is_mouseover<-true
    method on_mouseout x y=super#on_mouseout x y;is_mouseover<-false

    method on_click x y=
      is_clicked<-true;
      graphic#set_cur_drawing 1;
      super#on_click x y

    method on_release x y=
      is_clicked<-false;
      graphic#set_cur_drawing 0;
      super#on_release x y
  end;;

(** sample button object with pattern *)
class iface_pbutton rid npattern cpattern=
object(self)
  inherit iface_object 0 0 as super

  method grab_focus=true

  val mutable ngr=new iface_pgraphic_object npattern
  val mutable cgr=new iface_pgraphic_object cpattern

  val mutable is_clicked=false
  val mutable is_mouseover=false

  method on_mouseover x y=super#on_mouseover x y;is_mouseover<-true
  method on_mouseout x y=super#on_mouseout x y;is_mouseover<-false

  val mutable push=false;

  method private set_push()=
    push<-true;

  method private set_normal()=
    push<-false;

  method private set_clicked()=
    is_clicked<-true

  method private set_released()=
    is_clicked<-false

  method on_click x y=
    if push then
      (if is_clicked then is_clicked<-false else is_clicked<-true)
    else
      is_clicked<-true;

    super#on_click x y
      
  method on_release x y=
    if push=false then
      is_clicked<-false;
    super#on_release x y 

  method show()=
    super#show();
    ngr#show();
    cgr#show();

  method hide()=
    super#hide();
    ngr#hide();
    cgr#hide();

  method move x y=
    super#move x y;
    ngr#move x y;
    cgr#move x y;

  method put()=
    if is_clicked then
      cgr#put()
    else
      ngr#put()

  method resize nw nh=
    ngr#resize nw nh;
    cgr#resize nw nh;
    rect#set_size ngr#get_rect#get_w ngr#get_rect#get_h;    


  method lua_init()=
    lua#set_val (OLuaVal.String "set_push") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#set_push);
    lua#set_val (OLuaVal.String "set_normal") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#set_normal);
    super#lua_init();
    lua#set_val (OLuaVal.String "set_clicked") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#set_clicked);
    lua#set_val (OLuaVal.String "set_released") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#set_released);

end;;


(** button with label and pattern *)
class iface_pbutton_with_label rid npattern cpattern fnt_t tcol txt=
object(self)
    inherit iface_pbutton rid npattern cpattern as super

   

    val mutable label=
      (new iface_label_static fnt_t tcol txt)
	
    method private init_size()=
      let (bw,bh)=ngr#border_size in

(*	print_string "PBUTTON: ";print_int (label#get_rect#get_h mod bh);print_newline(); *)
	super#resize (label#get_rect#get_w+(bw*2)) (label#get_rect#get_h+(bh*2))

    initializer
      self#init_size();



    method show()=
      super#show();
      label#show();
      
    method hide()=
      super#hide();
      label#show();

    method move x y=
      super#move x y;
      let (bw,bh)=ngr#border_size in
      let ah=(label#get_rect#get_h mod bh)/2 and
	  aw=(label#get_rect#get_w mod bw)/2 in
	label#move (x+bw-aw) (y+bh-ah);

    method put()=

      super#put();
      if showing==true then 
	(
	 label#put();
	)

  end;;


(** button with label and pattern *)
class iface_pbutton_with_icon rid npattern cpattern gr w h=
object(self)
    inherit iface_pbutton rid npattern cpattern as super

    val mutable icon=
      new iface_graphic_file_object gr w h
	
    method private init_size()=
      let (bw,bh)=ngr#border_size in

(*	print_string "PBUTTON: ";print_int (label#get_rect#get_h mod bh);print_newline(); *)
	super#resize (icon#get_rect#get_w+(bw*2)) (icon#get_rect#get_h+(bh*2))

    initializer
      self#init_size();



    method show()=
      super#show();
      icon#show();
      
    method hide()=
      super#hide();
      icon#show();

    method move x y=
      super#move x y;
      let (bw,bh)=ngr#border_size in
      let ah=(icon#get_rect#get_h mod bh)/2 and
	  aw=(icon#get_rect#get_w mod bw)/2 in
	icon#move (x+bw-aw) (y+bh-ah);

    method put()=

      super#put();
      if showing==true then 
	(
	 icon#put();
	)

  end;;

