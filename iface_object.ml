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


open Rect;;
open Generic;;
open Medias;;
open Graphic;;
open Event;;

open Olua;;

(** Interface generic objects *)

(** parent object *)
class iface_object w h=
object(self)
  inherit generic_object
  inherit canvas_object
  inherit lua_object as lo

  initializer
    rect#set_size w h

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
    

  method grab_focus=false
  val mutable focused=false    
  method set_focused f=focused<-f
    
    
  method on_keypress (e:(key_type*key_type))=()
  method on_keyrelease (e:(key_type*key_type))=()
    
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

  method lua_init()=
    lua#set_val (OLuaVal.String "on_click") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));
    lua#set_val (OLuaVal.String "on_release") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));

    lua#set_val (OLuaVal.String "on_mouseover") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));
    lua#set_val (OLuaVal.String "on_mouseout") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) (fun x y->()));

    lua#set_val (OLuaVal.String "show") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#show);
    lua#set_val (OLuaVal.String "hide") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.unit) self#hide);
    lua#set_val (OLuaVal.String "move") (OLuaVal.efunc (OLuaVal.int **-> OLuaVal.int **->> OLuaVal.unit) self#move);
    lua#set_val (OLuaVal.String "set_data_text") (OLuaVal.efunc (OLuaVal.string  **->> OLuaVal.unit) self#set_data_text);
    lua#set_val (OLuaVal.String "get_data_text") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.string) (fun()->self#get_data_text));
    lo#lua_init();

    

     
	  
end;;

(** graphic object *)
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



(** graphic object from file *)
class iface_graphic_file_object file w h=
  object (self)
    inherit iface_graphic_object (new graphic_from_file file w h) w h
(*(new graphic_scr_resized_object w h file false false) w h as super*)

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


