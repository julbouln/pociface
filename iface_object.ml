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

open Low;;

open Rect;;
open Generic;;
open Medias;;
open Vfs;;

open Event_manager;;

open Olua;;

(** parent widget *)
class iface_object w h=
object(self)
  inherit canvas_object

  val mutable parent=None
  method set_parent (p:iface_object option)=parent<-p
  method get_parent=parent

    val mutable embed=false
    method set_embed e=embed<-e
    method get_embed=embed

    val mutable data=0
    val mutable data1=0
    val mutable data_text=""
    val mutable showing=false

    val mutable click=(function()->())
    val mutable release=(function()->())
    val mutable mouseover=(function()->())
    val mutable mouseout=(function()->())

    val mutable focused=false

    method set_focused f=focused<-f


    method on_keypress (e:event)=()
    method on_keyrelease (e:event)=()
	
    method on_click (x : int) (y : int)=click()
    method on_release (x : int) (y : int)=release()
    method on_mouseover (x : int) (y : int)=mouseover()
    method on_mouseout (x : int) (y : int)=mouseout()


    method append_click c=
      let oclick=click in
      let nclick()=oclick();c() in
	click<- nclick;

    method prepend_click (c:unit->unit)=
      let oclick=click in
      let nclick()=c();oclick() in
	click<- nclick;

    method set_click c=click<-c
    method set_release r=release<-r

    method get_click=click
    method get_release=release

    method set_mouseover c=mouseover<-c
    method set_mouseout c=mouseout<-c
      
    method is_showing=showing
    method show()=showing<-true
    method hide()=showing<-false
      
    method move x y=rect#set_position x y
    method resize w h=rect#set_size w h

    method get_rect=rect
    method get_vrect=self#get_rect

    method put()=()

    method set_data d=data<-d
    method get_data=data
    method set_data1 d=data1<-d
    method get_data1=data1
    method set_data_text d=data_text<-d
    method get_data_text=data_text

(* lua *)
    val mutable lua=""
    method set_lua l=lua<-l
    method get_lua=lua

    method lua_register (interp:lua_interp)=
      let lcode=(id^"={};\n") in
	interp#parse lcode;();

	interp#set_module_val id "on_click" (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));
	interp#set_module_val id "on_release" (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));

(*
	interp#set_module_val id "get_w" (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_w));
	interp#set_module_val id "get_h" (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_h));
	interp#set_module_val id "get_x" (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_x));	
	interp#set_module_val id "get_y" (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->rect#get_y));
*)
	interp#set_module_val id "show" (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#show);
	interp#set_module_val id "hide" (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#hide);
	interp#set_module_val id "move" (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#move);

	interp#set_module_val id "set_data_text" (OLuaVal.efunc (OLuaVal.string  **->> OLuaVal.unit) self#set_data_text);
	interp#set_module_val id "get_data_text" (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_data_text));

	interp#parse lua;();
	  
end;;

(** graphic object widget *)
class iface_graphic_object gr w h=
  object (self)
    inherit iface_object (gr#get_rect#get_w) (gr#get_rect#get_h) as super
    val mutable graphic=gr
    method move x y=
      super#move x y;
      graphic#move x y
    method put()=
      if showing==true then
	graphic#put()
    method get_rect=graphic#get_rect
  end;;



(** graphic object from file widget *)
class iface_graphic_file_object file w h=
  object (self)
    inherit iface_graphic_object (new graphic_scr_resized_object w h file false false) w h as super

  end;;

(** graphic object from file widget with var color *)
class iface_graphic_colored_object file w h un uc=
  object (self)
    inherit iface_graphic_object (new graphic_object_colored w h file false false un uc) w h as super

  end;;




(** patterned iface object*)
class iface_pgraphic_object (graph:graphic_pattern)=
object(self)
  inherit iface_object 0 0 as super

  val mutable gr=graph

  method border_size=
    ((gr#get_crect#get_w),(gr#get_crect#get_h))

  method move x y=
    super#move x y;
    gr#move x y;

  method resize nw nh=
    gr#get_rect#set_size nw nh;
    let (rw,rh)=gr#real_size in
      rect#set_size rw rh;

  method put()=
    if self#is_showing then (
      gr#put();
    )
end;; 


(** graphic object widget *)
class iface_graphic_object_NEW gr=
  object (self)
    inherit iface_object (gr#get_rect#get_w) (gr#get_rect#get_h) as super
    val mutable graphic=gr

    method move x y=
      super#move x y;
      graphic#move x y

    method put()=
      if showing==true then
	graphic#put()
  end;;


class iface_icon icon w h=
  object
    inherit iface_graphic_file_object icon w h as super
  end;;
