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
open Medias;;

open Event_manager;;

open Iface_object;;
open Iface_text;;


(** sample button widget *)
class iface_button file w h=
  object
    inherit iface_graphic_file_object file w h as super
    val mutable is_clicked=false
    val mutable is_mouseover=false

    method on_mouseover x y=super#on_mouseover x y;is_mouseover<-true
    method on_mouseout x y=super#on_mouseout x y;is_mouseover<-false

    method on_click x y=
      is_clicked<-true;
      graphic#set_cur_tile 1;
      super#on_click x y

    method on_release x y=
      is_clicked<-false;
      graphic#set_cur_tile 0;
      super#on_release x y
  end;;


(** button with label widget *)
class iface_button_with_label fnt txt file w h=
  object
    inherit iface_button file w h as super
    val mutable label=
      new graphic_real_object ("label/button/"^txt^":"^(string_of_int fnt#get_size)) (fnt#create_text txt (0,0,0))

    method put()=
      super#put();
      if showing==true then 
	(
	 label#move (graphic#get_rect#get_x + ((graphic#get_rect#get_w - label#get_rect#get_w)/2)) (graphic#get_rect#get_y + ((graphic#get_rect#get_h - label#get_rect#get_h)/2));
	 label#put();
	)

  end;;


class iface_button_icon icon w h iw ih=
  object
    inherit iface_button icon w h as super
    val mutable ic=new graphic_real_resized_object (icon^":resized") ((float_of_int w)/.(float_of_int iw)) ((float_of_int h)/.(float_of_int ih)) (tiles_load icon iw ih).(0)

    method put()=
(*      super#put(); *)
      if showing==true then 
	(
	  ic#move (graphic#get_rect#get_x) (graphic#get_rect#get_y);
	  ic#put();
	)

  end;;

(** checkbox widget *)
class iface_checkbox f fnt txt=
  object
    inherit iface_button f 20 20 as super
    val mutable label=
      new graphic_real_object ("label/checkbox/"^txt^":"^(string_of_int fnt#get_size)) (fnt#create_text txt (255,255,255))

    method set_data d=if d=0 then is_clicked<-false else is_clicked<-true
    method get_data=if is_clicked=false then 0 else 1
    
    method put()=
      if is_clicked==true then
	graphic#set_cur_tile 1
      else
	graphic#set_cur_tile 0;
      
      super#put();
      if showing==true then 
	(
	 label#move (graphic#get_rect#get_x + graphic#get_rect#get_w + 10) (graphic#get_rect#get_y + ((graphic#get_rect#get_h - label#get_rect#get_h)/2));
	 label#put();
	)

    method on_click x y=
      if is_clicked==true then (
	is_clicked<-false;
       )
      else (
	is_clicked<-true ;
       );
      click();
    method on_release x y=()


end;;


(* NEW *)

class iface_rbutton rid nptile cptile=
object
  inherit iface_object 0 0 as super

  val mutable ngr=new iface_rgraphic_object (rid^":normal") nptile
  val mutable cgr=new iface_rgraphic_object (rid^":clicked") cptile

  val mutable is_clicked=false
  val mutable is_mouseover=false

  method on_mouseover x y=super#on_mouseover x y;is_mouseover<-true
  method on_mouseout x y=super#on_mouseout x y;is_mouseover<-false

  method on_click x y=
    is_clicked<-true;
    super#on_click x y
      
  method on_release x y=
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

  method private resize nw nh=
    rect#set_size nw nh;
    ngr#resize nw nh;
    cgr#resize nw nh;

    
end;;


(** button with label widget *)
class iface_rbutton_with_label rid nptile cptile fnt tcol txt=
object(self)
    inherit iface_rbutton rid nptile cptile as super

    val mutable label=
      (new iface_label_static fnt tcol txt)
	
    method private init_size()=
      let (bw,bh)=ngr#border_size in
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
      label#move (x+bw) (y+bh);

    method put()=

      super#put();
      if showing==true then 
	(
	 label#put();
	)

  end;;
