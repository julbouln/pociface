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

open Iface_properties;;

(** container widget *)
class iface_container c=
  object (self)
    inherit iface_object 0 0 as super
    val mutable content=c

(** properties *)

    val mutable valign=IAlignMiddle
    val mutable halign=IAlignMiddle
    val mutable fixed_size=false
    val mutable symmetric_size=false

    method set_valign v=valign<-v
    method set_halign h=halign<-h

    method set_fixed_size i=
      fixed_size<-i

    method set_symmetric_size i=
      symmetric_size<-i



    val mutable vrect=new rectangle 0 0 0 0 
    method get_vrect=vrect

    method resize w h=
      super#resize w h;
      vrect#set_size w h;

    method set_layer l=
      layer<-l;
      self#foreach
	(
	  fun o->
	    o#set_layer (layer+1)
	);

    initializer
      self#init_childs();
      self#init_layer 1;

    method private init_childs()=
      self#foreachi
	(
	  fun i o->
	    o#set_parent (Some (self:>iface_object))
	)

    method private init_layer l=
      self#foreach
	(
	  fun o->
	    o#set_layer (self#get_layer+l)
	);
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
	    if obj#get_vrect#is_position x y then (
	      
	      obj#on_click x y;
	    ) 
	  )
      );
      super#on_click x y;

    method on_release x y=
      self#foreach (
	fun obj->
	  if obj#get_embed then (
	    if obj#get_vrect#is_position x y then (
	      
	      obj#on_release x y;
	    ) 
	  )
      );
      super#on_release x y;

    method on_mouseover x y=
      self#foreach (
	fun obj->
	  if obj#get_embed then (
	    if obj#get_vrect#is_position x y then (	      
	      obj#on_mouseover x y;
	    ) else obj#on_mouseout x y;
	  )
      );
      super#on_mouseover x y;


    method on_mouseout x y=
      self#foreach (
	fun obj->
	  if obj#get_embed then (
	      obj#on_mouseout x y;
	  )
      );
      super#on_mouseout x y;

    method pos_from_align (orect:rectangle)=
      let (mw,mh)=if symmetric_size then (self#max_size()) else (rect#get_w,rect#get_h) in
(*
      let mw=self#get_rect#get_w and
	  mh=self#get_rect#get_h in
*)
	  (
	    (match halign with
	       | IAlignLeft -> 0
	       | IAlignRight -> mw - orect#get_w
	       | IAlignMiddle -> mw/2 - orect#get_w/2
	       | _ -> print_string "IFACE_CONTAINER: warning bad halign";print_newline();0
	    ),
	    (match valign with
	       | IAlignTop -> 0
	       | IAlignBottom -> mh - orect#get_h
	       | IAlignMiddle -> mh/2 - orect#get_h/2
	       | _ -> print_string "IFACE_CONTAINER: warning bad valign";print_newline();0
	    )
	  )

    method move x y=
      super#move x y;
      vrect#set_position x y;

(** private *)

    method private max_size()=
      let w=ref 0 in
      let h=ref 0 in
      self#foreach (
	fun obj->
	  if obj#get_rect#get_h> !h then
	    h:=obj#get_rect#get_h;
	  if obj#get_rect#get_w> !w then
	    w:=obj#get_rect#get_w
      );

	(!w,!h)

    method private vmax_size()=
      let w=ref 0 in
      let h=ref 0 in
      self#foreach (
	fun obj->
	  if obj#get_vrect#get_h> !h then
	    h:=obj#get_vrect#get_h;
	  if obj#get_vrect#get_w> !w then
	    w:=obj#get_vrect#get_w
      );
	(!w,!h)

  end;;


(** vertical container widget *)
class iface_vcontainer c=
  object (self)
    inherit iface_container c as super

(* set childs size *)
    method resize (w:int) (h:int)=

      if fixed_size || w=0 || h=0 then
	self#reset_size()
      else 
	(
	  super#resize w h;

	  let ih=h/(Array.length c) in

	    self#foreachi
	      (
		fun i o->
		  let oh=o#get_rect#get_h and
		      ow=o#get_rect#get_w in

		    o#resize (if symmetric_size then w else ow) (if symmetric_size then ih else (h/rect#get_h * oh))
	      )
	)
	
(* set size from childs *)  
    method reset_size()=
      let w=ref 0 in
      let h=ref 0 in
      let (mw,mh)=self#max_size() in	
      self#foreach (
      let f obj=
	h:=!h+(if symmetric_size then mh else obj#get_rect#get_h);
	if obj#get_rect#get_w> !w then
	  w:=obj#get_rect#get_w
      in f
      );
	rect#set_size !w !h;

      let vw=ref 0 in
      let vh=ref 0 in	
      let (vmw,vmh)=self#vmax_size() in	
      self#foreach (
      let f obj=
	vh:=!vh+(if symmetric_size then vmh else obj#get_vrect#get_h);
	if obj#get_vrect#get_w> !vw then
	  vw:=obj#get_vrect#get_w
      in f
     );
	vrect#set_size !vw !vh;

	
    method move x y=
      super#move x y;
      let (mw,mh)=self#max_size() in
      let h=ref 0 in

      self#foreachi (
	fun i obj->	  
	  let (ax,ay)=self#pos_from_align obj#get_rect in
	  obj#move 
	    (x+ax) 
	    (y+( (if symmetric_size then (mh*i)+ay else (!h))));
	    h:= !h+obj#get_rect#get_h;

      );

	if symmetric_size=false then (
	  self#foreachi (
	    fun i obj->
	      
	      let (_,ay)=self#pos_from_align (new rectangle 0 0 0 !h) in
		obj#move obj#get_rect#get_x (obj#get_rect#get_y+ay);
	  )
	);

  end;;

 (** horizontal container widget *)

class iface_hcontainer c=
  object (self)
    inherit iface_container c as super

(* set childs size *)
    method resize (w:int) (h:int)=

      if fixed_size || w=0 || h=0 then
	self#reset_size()
      else 
	(
	  super#resize w h;

	  let iw=w/(Array.length c) in

	    self#foreachi
	      (
		fun i o->
		  let oh=o#get_rect#get_h and
		      ow=o#get_rect#get_w in

		    o#resize (if symmetric_size then iw else (w/rect#get_w * ow)) (if symmetric_size then h else oh) 
	      )
	)
	
(* set size from childs *)  
    method reset_size()=
      let w=ref 0 in
      let h=ref 0 in
      let (mw,mh)=self#max_size() in	
      self#foreach (
      let f obj=
	w:=!w+(if symmetric_size then mw else obj#get_rect#get_w);
	if obj#get_rect#get_h> !h then
	  h:=obj#get_rect#get_h
      in f
     );
      rect#set_size !w !h;
	

      let vw=ref 0 in
      let vh=ref 0 in	
      let (vmw,vmh)=self#vmax_size() in	
      self#foreach (
      let f obj=
	vw:=!vw+(if symmetric_size then vmw else obj#get_vrect#get_w);
	if obj#get_vrect#get_h> !vh then
	  vh:=obj#get_vrect#get_h
      in f
     );
	vrect#set_size !vw !vh;

    method move x y=
      super#move x y;
      let (mw,mh)=self#max_size() in
      let w=ref 0 in
      self#foreachi (
      fun i obj->
	let (ax,ay)=self#pos_from_align obj#get_rect in
(*	obj#move (x+(mw*i)+ax) (y+ay)*)
	  obj#move 
	    (x+( (if symmetric_size then (mw*i)+ax else (!w))))
	    (y+ay); 
	    w:= !w+obj#get_rect#get_w;
     );

	if symmetric_size=false then (
	  self#foreachi (
	    fun i obj->
	      
	      let (ax,_)=self#pos_from_align (new rectangle 0 0 !w 0) in
		obj#move (obj#get_rect#get_x+ax) (obj#get_rect#get_y);
	  )
	);

  end;;

