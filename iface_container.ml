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

open Iface_object;;

(** container widget *)
class iface_container c=
  object (self)
    inherit iface_object 0 0 as super
    val mutable content=c

    initializer
      self#reset_size();

    method private foreach f=
      Array.iter f content;
      
    method reset_size()=()

    method get_rect=rect
      
    method private foreachi f=
      Array.iteri f content;

    method show()=
      super#show();
      self#foreach (let f obj=obj#show() in f)

    method hide()=
      super#hide();
      self#foreach (let f obj=obj#hide() in f)

    method put()=
      super#put();
      self#foreach (fun obj->
		      if obj#get_embed then
			obj#put()
		   )

    method on_click x y=
      self#foreach (
	fun obj->
	  if obj#get_embed then (
	    if x > obj#get_vrect#get_x 
	      && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	      && y > obj#get_vrect#get_y 
	      && y < (obj#get_vrect#get_h + obj#get_vrect#get_y)
	    then (
	      
	      obj#on_click x y;
	    ) 
	  )
      );
      super#on_click x y;

    method on_release x y=
      self#foreach (
	fun obj->
	  if obj#get_embed then (
	    if x > obj#get_vrect#get_x 
	      && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	      && y > obj#get_vrect#get_y 
	      && y < (obj#get_vrect#get_h + obj#get_vrect#get_y)
	    then (
	      
	      obj#on_release x y;
	    ) 
	  )
      );
      super#on_release x y;

    method on_mouseover x y=
      self#foreach (
	fun obj->
	  if obj#get_embed then (
	    if x > obj#get_vrect#get_x 
	      && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	      && y > obj#get_vrect#get_y 
	      && y < (obj#get_vrect#get_h + obj#get_vrect#get_y)
	    then (
	      
	      obj#on_mouseover x y;
	    ) else obj#on_mouseout x y;
	  )
      );
      super#on_mouseover x y;


    method on_mouseout x y=
      self#foreach (
	fun obj->
	  if obj#get_embed then (
	    if x > obj#get_vrect#get_x 
	      && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	      && y > obj#get_vrect#get_y 
	      && y < (obj#get_vrect#get_h + obj#get_vrect#get_y)
	    then (
	      
	      obj#on_mouseout x y;
	    ) 
	  )
      );
      super#on_mouseover x y;

  end;;


(** vertical container widget *)
class iface_vcontainer c=
  object (self)
    inherit iface_container c as super

    val mutable vrect=new rectangle 0 0 0 0 
    method get_vrect=vrect

    method reset_size()=
      let w=ref 0 in
      let h=ref 0 in
      self#foreach (
      let f obj=
	h:=!h+obj#get_rect#get_h;
	if obj#get_rect#get_w> !w then
	  w:=obj#get_rect#get_w
      in f
      );
	rect#set_size !w !h;

      let vw=ref 0 in
      let vh=ref 0 in	
      self#foreach (
      let f obj=
	vh:=!vh+obj#get_vrect#get_h;
	if obj#get_vrect#get_w> !vw then
	  vw:=obj#get_vrect#get_w
      in f
     );
	vrect#set_size !vw !vh;
	
    method move x y=
      super#move x y;
      self#foreachi (
	fun i obj->
	  obj#move x (y+ (obj#get_rect#get_h*i))
      )


  end;;

(** vertical container widget *)
class iface_hcontainer c=
  object (self)
    inherit iface_container c as super


    val mutable vrect=new rectangle 0 0 0 0
    method get_vrect=vrect

    method reset_size()=
      let w=ref 0 in
      let h=ref 0 in
      self#foreach (
      let f obj=
	w:=!w+obj#get_rect#get_w;
	if obj#get_rect#get_h> !h then
	  h:=obj#get_rect#get_h
      in f
     );
      rect#set_size !w !h;
	

      let vw=ref 0 in
      let vh=ref 0 in	
      self#foreach (
      let f obj=
	vw:=!vw+obj#get_vrect#get_w;
	if obj#get_vrect#get_h> !vh then
	  vh:=obj#get_vrect#get_h
      in f
     );
	vrect#set_size !vw !vh;

    method move x y=
      super#move x y;
      self#foreachi (
      fun i obj->
	obj#move (x+(obj#get_rect#get_w*i)) y
     )
  end;;

