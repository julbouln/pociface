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


open Drawing;;
open Medias;;
open Graphic;;
open Olua;;
open Binding;;
open Oval;;

open Iface_object;;
open Iface_text;;
open Iface_button;;
open Iface_container;;
open Iface_properties;;


(** Interface tools *)

(** generic tool object *)
class ['a] iface_tool (r:'a) w h=
object(self)
  inherit iface_object w h as super
  val mutable rval=r
  method get_rval=rval
end;;

(** icon tool graphic *)
class ['a] iface_tool_graphic (r:'a) gr=
object(self)
  inherit ['a] iface_tool r 0 0 as super
  val mutable graphic=gr

  initializer
    rect#set_size gr#get_rect#get_w gr#get_rect#get_h

  method move x y=
    super#move (x) (y);
    graphic#move (x) (y);

  method put()=
    super#put();
    if showing then
      graphic#put();
end;;

(** icon tool graphic with label *)
class ['a] iface_tool_graphic_with_label (r:'a) fnt_t label gr=
object(self)
  inherit ['a] iface_tool_graphic r gr as super
  val mutable lab=new iface_label_static fnt_t (0,0,0) label

  method private reset_size()=
    rect#set_size (gr#get_rect#get_w+lab#get_rect#get_w) (gr#get_rect#get_h);

  initializer
    self#reset_size();

  method show()=
    super#show();
    lab#show();

  method hide()=
    super#hide();
    lab#hide();

  method move x y=
    super#move (x) (y);
    lab#move (x+graphic#get_rect#get_w) (y);

  method put()=
    super#put();
    lab#put();
end;;


(** icon tool object *)
class ['a] iface_tool_icon (r:'a) f w h iw ih=
object(self)
  inherit ['a] iface_tool_graphic r 
(*(new graphic_real_resized_object (f^":icon") ((float_of_int w)/.(float_of_int iw)) ((float_of_int h)/.(float_of_int ih)) 
(tiles_load f iw ih).(0)) 
*)
(new graphic_resized_from_file "icon" f 0 w h iw ih)
as super
end;;


(** icon tool icon with label *)
class ['a] iface_tool_icon_with_label (r:'a) fnt_t label f w h iw ih=
object(self)
  inherit ['a] iface_tool_icon (r) f w h iw ih as super
    val mutable lab=new iface_label_static fnt_t (0,0,0) label

  method private reset_size()=
    rect#set_size (graphic#get_rect#get_w+lab#get_rect#get_w) (graphic#get_rect#get_h);

  initializer
    self#reset_size();

  method show()=
    super#show();
    lab#show();

  method hide()=
    super#hide();
    lab#hide();

  method move x y=
    super#move (x) (y);
    lab#move (x+graphic#get_rect#get_w) (y);

  method put()=
    super#put();
    lab#put();
end;;

(** generic toolbox object *)
class virtual ['a] iface_toolbox (iv:'a) (c:('a) iface_tool array) =
object(self)
  inherit iface_vcontainer c as super
  val mutable selected=
    new graphic_from_drawing "selected" "selected" (
      fun()->
	let dr=drawing_vault#new_drawing() in
	  dr#exec_op_create_from_list "rect" 
	    [
	      `Size(32,32);
	      `Color(255,0,0)
	    ];
	[|dr|]
    )
    (*new graphic_real_object "selected" (tile_rect 32 32 (255,0,0))*)

  val mutable show_selected=false
  method move_selected x y=selected#move x y 

  val mutable current_val=iv 
  method get_current=current_val
  method set_current i=
    let o=c.(i) in
      current_val<-o#get_rval;
      self#move_selected o#get_rect#get_x o#get_rect#get_y; 
      show_selected<-true;
(*  initializer 
    selected#move (-32) (-32);
*)
(*    self#reset_size(); *)
(*    self#set_symmetric_size true;
    self#set_fixed_size false;
*)
(*    self#set_halign IAlignLeft; *)

  method move x y=
    let selrx=selected#get_rect#get_x - rect#get_x and
	selry=selected#get_rect#get_y - rect#get_y in

      selected#move (x+selrx) (y+selry);
      vrect#set_position x y;
      super#move x y;
    
      

  method on_click x y=
    print_string "IFACE : toolbox click";print_newline();
    let t=ref (-1) in
      self#foreachi (
	fun i obj->
	  if x > obj#get_vrect#get_x 
	    && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	    && y > obj#get_vrect#get_y 
	    && y < (obj#get_vrect#get_h + obj#get_vrect#get_y) 
	  then
	    t:=i	
      );
      if (!t)<>(-1) then
	self#set_current !t;

      print_int !t;print_newline();
    super#on_click x y;

    
  method put()=
    super#put();
    self#foreach (
      fun o->
	o#put()
    );
    if self#is_showing then
      if show_selected then
	selected#put();

end;;




let string_of_color c=
  let (r,g,b)=c in
  (string_of_int r^","^string_of_int g^","^string_of_int b)

(* tile cw ch *)
(** iface tool color *)
class iface_color_tool re (i:int) (c:color) w h=
object
  inherit [int] iface_tool_graphic i
(*    (new font_object "medias/Vera.ttf" 16) (string_of_color c)  *)
(*    (new graphic_real_object (re^":"^string_of_color c) (tile_box w h c) *)
    (new graphic_from_drawing "color" (re^":"^string_of_color c) 
       (fun()->
	  let dr=drawing_vault#new_drawing() in
	    dr#create w h c;
	    [|dr|]
       )
    ) as super
end;;

(** iface toolbox color *)
class iface_color_toolbox id (vc:v_color)  (set_color:int->unit)=
object(self)
  inherit [int] iface_toolbox (0) 
    (
      let a=DynArray.create() in

      let r=ref 0 in
	  vc#vcolor_foreach (
	    fun c ca ->
	      if !r=0 then (
		Array.iteri (
		  fun i v->
		    DynArray.add a (new iface_color_tool id i v 32 32)
		) ca;
		r:= !r+1;
	      )
	  );

	DynArray.to_array a
    )
    as super

  initializer
    self#reset_size();

  method on_click x y=
    print_string "IFACE : color toolbox click";print_newline();
    super#on_click x y;
    if current_val<>(-1) then (
	set_color current_val;
      )

  method lua_init()=
    lua#set_val (OLuaVal.String "get_val") (OLuaVal.efunc (OLuaVal.unit **->> OLuaVal.int) (fun()->current_val));
    super#lua_init();



end;;





