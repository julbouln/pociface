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


(** Interface buttons *)

(** sample button object *)
class iface_pbutton rid nptile cptile=
object
  inherit iface_object 0 0 as super

  val mutable ngr=new iface_pgraphic_object nptile
  val mutable cgr=new iface_pgraphic_object cptile

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

  method resize nw nh=
    ngr#resize nw nh;
    cgr#resize nw nh;
    rect#set_size ngr#get_rect#get_w ngr#get_rect#get_h;    
end;;


(** button with label widget *)
class iface_pbutton_with_label rid nptile cptile fnt tcol txt=
object(self)
    inherit iface_pbutton rid nptile cptile as super

    val mutable label=
      (new iface_label_static fnt tcol txt)
	
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

(** {2 DEPRECATED : for BFR compatibility } *)

(** sample button object *)
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
      graphic#set_cur_tile 1;
      super#on_click x y

    method on_release x y=
      is_clicked<-false;
      graphic#set_cur_tile 0;
      super#on_release x y
  end;;

(** button with label object *)
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

(** button icon object *)
class iface_button_icon icon w h iw ih=
  object
    inherit iface_button icon w h as super
    val mutable ic=new graphic_real_resized_object (icon^":resized") ((float_of_int w)/.(float_of_int iw)) ((float_of_int h)/.(float_of_int ih)) (tiles_load icon iw ih).(0)

    initializer
      rect#set_size w h

    method put()=
(*      super#put(); *)
      if showing==true then 
	(
	  ic#move (graphic#get_rect#get_x) (graphic#get_rect#get_y);
	  ic#put();
	)

  end;;

(** checkbox object *)
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

