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

    method get_rect=rect
    method get_vrect=self#get_rect

    method put()=()

    method set_data d=data<-d
    method get_data=data
    method set_data1 d=data1<-d
    method get_data1=data1
    method set_data_text d=data_text<-d
    method get_data_text=data_text
	  
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


(** special graphic resize with 9 tiles *)
class iface_rgraphic_object rid ptile=
object(self)
  inherit iface_object 0 0 as super


  val mutable gr=new graphic_generic_object rid
  val mutable crect=new rectangle 0 0 0 0

  method resize nw nh=
    rect#set_size nw nh;

  method border_size=
    ((crect#get_w),(crect#get_h))

  method private init()=
(*    let gen=new graphic_real_object (rid^"/gen") (ptile) in *)
    let cw=tile_get_w ptile and
	ch=tile_get_h ptile in
      crect#set_size (cw/3) (ch/3);

    gr<-new graphic_from_func rid (
      fun()->
	let ta=tile_split ptile crect#get_w crect#get_h in
	  for i=0 to (Array.length (ta))-1 do
	    tile_set_alpha ta.(i) 255 255 255; 
	  done;
	  ta
    );



  initializer
    self#init()

(*

036
147
258

*)

  method put()=
    if self#is_showing then (
      let cw=rect#get_w/crect#get_w and
	  ch=rect#get_h/crect#get_h in    
	for i=0 to cw do
	  for j=0 to ch do
	    (match (i,j) with
	       | (0,0) -> gr#set_cur_tile 0
	       | (0,ih) when ih=ch ->gr#set_cur_tile 2
	       | (0,_) ->gr#set_cur_tile 1
	       | (iw,0) when iw=cw -> gr#set_cur_tile 6
	       | (_,0) ->gr#set_cur_tile 3
	       | (iw,ih) when iw=cw && ih=ch -> gr#set_cur_tile 8
	       | (_,ih) when ih=ch ->gr#set_cur_tile 5
	       | (iw,_) when iw=cw ->gr#set_cur_tile 7
	       | (_,_) ->gr#set_cur_tile 4
	    );
	    gr#move (rect#get_x + (i*crect#get_w)) (rect#get_y + (j*crect#get_h));
	    gr#put();
	  done
	done
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
