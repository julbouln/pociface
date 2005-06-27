(*
    pocengine - game/multimedia system
    Copyright (C) 2003-2005 POC 

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
open Olua;;
open Otype;;
open Medias;;
open Video;;

open Iface_object;;
open Iface_text;;
open Iface_container;;

(** DEPRECATED : for BFR compatibility, ignore it! *)

(** graphic object from file with var color *)
class iface_graphic_colored_object file w h un uc=
  object (self)
    inherit iface_graphic_object (new graphic_object_colored w h file false false un uc) w h as super

  end;;

(** {2 DEPRECATED : for BFR compatibility } *)

(** text object *)
class iface_text fnt color txt_s=
  object
    inherit iface_graphic_object (
      let txt=text_split txt_s in
      let cs=match color with 
      |(x,y,z)->(string_of_int x)^(string_of_int y)^(string_of_int z) in

      new graphic_dyn_object ("text:"^(List.nth txt 0)^cs) (List.length txt)
	(function k-> (
	  fnt#create_text (List.nth txt k) color
	 ))	 
     ) 0 0 as super
	
    val txt=text_split txt_s

    initializer
      let cw=ref 0 and
	  ch=ref 0 in
      for i=0 to (List.length txt)-1 do
	let pos=fnt#sizeof_text (List.nth txt i) in
	if !cw<(fst pos) then
	  cw:=(fst pos);
	ch:=!ch + (snd pos);
      done;
      graphic#get_rect#set_size (!cw) (!ch);

    method put()=
      if showing==true then (
	for i=0 to (List.length txt)-1 do
	  let ty=(graphic#get_rect#get_y) in
	  graphic#set_cur_tile i;
	  graphic#move (graphic#get_rect#get_x) (ty+(i*fnt#get_height));
	  graphic#put();	
	  graphic#move (graphic#get_rect#get_x) (ty);
	done;
       )
  end;;




(** Buttons *)

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

(** Menus *)

(** select box widget (DEPRECATED) *)
class iface_selectbox_OLD fnt e=
 object(self)
  inherit iface_vcontainer 
(Array.make (Array.length e) (new iface_label_static fnt (0,63,0) "none" )) as super
   val mutable cur_g=new iface_label_dynamic fnt (0,255,0)
   val mutable cur_entry=0  

       
   initializer
     self#foreachi(let f i obj=content.(i)<-new iface_label_static fnt (0,63,0) (e.(i)) in f);      
     self#set_entry 0;
     self#reset_size();

   method private set_entry i=
      if i <> (-1) then (
	cur_entry<-(i);
	cur_g#set_data_text e.(i);
	cur_g#move (content.(i)#get_rect#get_x) (content.(i)#get_rect#get_y)
       );     


   method on_click x y=
     super#on_click x y;
     let t=ref (-1) in
     self#foreachi (
     let f i obj=
       if x > obj#get_rect#get_x 
	   && x < (obj#get_rect#get_w + obj#get_rect#get_x) 
	   && y > obj#get_rect#get_y 
	   && y < (obj#get_rect#get_h + obj#get_rect#get_y) 
       then
	 t:=i
     in f
    );
     self#set_entry !t

  method on_release x y=()
      
   
   method set_data d=self#set_entry d
   method get_data=cur_entry

   method get_rect=rect

   method move x y=
     super#move x y;
     cur_g#move x y;
  
   method show()=
     super#show();
     cur_g#show()

   method hide()=
     super#hide();
     cur_g#hide()

   method put()=
     let fg=tile_rect (rect#get_w+10) (rect#get_h+10) (255,255,255) and
       bg=tile_box (rect#get_w+10) (rect#get_h+10) (0,0,0) in
     tile_put bg (rect#get_x - 5) (rect#get_y - 5);
     tile_put fg (rect#get_x - 5) (rect#get_y - 5);
     tile_free bg;
     tile_free fg;
       
     super#put();
     cur_g#put()

end;;


(** select box widget *)
class iface_selectbox fnt e=
 object(self)
  inherit iface_vcontainer 
(Array.make (Array.length e) (new iface_label_static fnt (0,63,0) "none" )) as super
   val mutable cur_g=new iface_label_dynamic fnt (0,255,0)
   val mutable cur_entry=0  
   val mutable clicked=false
   val mutable first_x=0
   val mutable first_y=0
   val mutable last=0
   val mutable w=0
   val mutable h=0
       
   initializer
     self#foreachi(let f i obj=content.(i)<-new iface_label_static fnt (0,127,0) (e.(i)) in f);      
     self#set_entry 0;
     self#reset_size();
     w<-rect#get_w;
     h<-rect#get_h;
     rect#set_size w content.(0)#get_rect#get_h

   method private set_entry i=
     if i <> (-1) then (

       cur_g#set_data_text e.(i);

(*       let t=ref (-1) in
	 self#foreachi (
	   let f j obj=
	     if first_x = obj#get_rect#get_x &&
	       first_y = obj#get_rect#get_y then
	       t:=j
	   in f
	 );
*)
	
	   content.(cur_entry)#move (content.(i)#get_rect#get_x) (content.(i)#get_rect#get_y);  
       content.(i)#move (first_x) (first_y);
       cur_entry<-(i);
     );     


   method on_click x y=
     if clicked==false then (
       clicked<-true;			       
       rect#set_size w h
     )
     else (
       super#on_click x y;
       let t=ref (-1) in
	 self#foreachi (
	   let f i obj=
	     if x > obj#get_rect#get_x 
	       && x < (obj#get_rect#get_w + obj#get_rect#get_x) 
	       && y > obj#get_rect#get_y 
	       && y < (obj#get_rect#get_h + obj#get_rect#get_y) 
	     then
	       t:=i
	   in f
	 );
	 self#set_entry !t;
	 clicked<-false;	   	 
	 rect#set_size w content.(cur_entry)#get_rect#get_h
     );
     
   method on_release x y=()

   
   method set_data d=self#set_entry d
   method get_data=cur_entry

   method get_rect=rect

   method move x y=
     first_x<-x;
     first_y<-y;
     super#move x y;
     cur_g#move x y;
  
   method show()=
     super#show();
     cur_g#show()

   method hide()=
     super#hide();
     cur_g#hide()

   method put()=
     
     if clicked==true then (
     let bg=tile_box (w+10) h (55,55,55) in
       tile_put bg (first_x-5) first_y;
       tile_free bg;
     let fg=tile_rect (w+10) h (127,127,127) in
       tile_put fg (first_x-5) first_y;
       tile_free fg;

       super#put();
     );
     let bg=tile_box (w+10) content.(cur_entry)#get_rect#get_h (63,63,63) in
       tile_put bg (first_x-5) first_y;
       tile_free bg;
     let fg=tile_rect (w+10) content.(cur_entry)#get_rect#get_h (127,127,127) in
       tile_put fg (first_x-5) first_y;
       tile_free fg;

     cur_g#put()

end;;


(** select box widget *)
class iface_menulist fnt ol=
 object(self)
  inherit iface_vcontainer ol as super

   val mutable clicked=false
   val mutable first_x=0
   val mutable first_y=0
   val mutable last=0
   val mutable w=0
   val mutable h=0
       
   initializer
     self#set_entry 0;
     self#reset_size();
     w<-rect#get_w;
     h<-rect#get_h;
     rect#set_size w content.(0)#get_rect#get_h

   method private set_entry i=
     if i <> (-1) then (

(*       cur_g#set_data_text e.(i); *)

(*       let t=ref (-1) in
	 self#foreachi (
	   let f j obj=
	     if first_x = obj#get_rect#get_x &&
	       first_y = obj#get_rect#get_y then
	       t:=j
	   in f
	 );
*)
	
(*	   content.(cur_entry)#move (content.(i)#get_rect#get_x) (content.(i)#get_rect#get_y);  
       content.(i)#move (first_x) (first_y);
       cur_entry<-(i);*)
     );     


   method on_click x y=
     if clicked==false then (
       clicked<-true;			       
       rect#set_size w h
     )
     else (
       super#on_click x y;
       let t=ref (-1) in
	 self#foreachi (
	   let f i obj=
	     if x > obj#get_rect#get_x 
	       && x < (obj#get_rect#get_w + obj#get_rect#get_x) 
	       && y > obj#get_rect#get_y 
	       && y < (obj#get_rect#get_h + obj#get_rect#get_y) 
	     then
	       t:=i
	   in f
	 );
	 self#set_entry !t;
	 clicked<-false;	   	 
	 rect#set_size w content.(0)#get_rect#get_h 
     );
     
   method on_release x y=()

   
(*   method set_data d=self#set_entry d 
   method get_data=cur_entry *)

   method get_rect=rect

   method move x y=
     first_x<-x;
     first_y<-y;
     super#move x y;
     
   method show()=
     super#show();
   
   method hide()=
     super#hide();
   
   method put()=
     
     if clicked==true then (
     let bg=tile_box (w+10) h (55,55,55) in
       tile_put bg (first_x-5) first_y;
       tile_free bg;
     let fg=tile_rect (w+10) h (127,127,127) in
       tile_put fg (first_x-5) first_y;
       tile_free fg;

       super#put();
     );
     let bg=tile_box (w+10) content.(0)#get_rect#get_h (63,63,63) in
       tile_put bg (first_x-5) first_y;
       tile_free bg;
     let fg=tile_rect (w+10) content.(0)#get_rect#get_h (127,127,127) in
       tile_put fg (first_x-5) first_y;
       tile_free fg;


end;;

(* FIXME : rename * iface_selectbox2 to iface_selectbox *)
class iface_selectbox2 fnt e=
object
  inherit iface_selectbox fnt e
end;;

exception Iface_object_not_found of string;;



(** volume control widget *)
class iface_volume s e w h=
  let vol=ref 1 in
  object(self)
    inherit iface_graphic_object 
	(new graphic_dyn_object (random_string "iface_volume" 16) s (function k->(
	  tile_box (video#f_size_w w) ((video#f_size_h h)+3*k) (if k< !vol then (255,255,255) else (127,127,127))
	  )))
	w h as super
    initializer
      graphic#get_rect#set_size (((video#f_size_w w)+e)*(s+2)) ((video#f_size_h h)+(s*3))

    method on_click x y=
      let px=(x - graphic#get_rect#get_x) and 
	  py=(y - graphic#get_rect#get_y) in
      vol:=(px)/((video#f_size_w w)+e) + 1 ;
      click()
    method set_data v=vol:=v
    method get_data= !vol
    method put()=
      if showing==true then (
	let x=graphic#get_rect#get_x and
	    y=graphic#get_rect#get_y in

	for i=0 to s do 
	  graphic#set_cur_tile i;
	  graphic#move ((i*((video#f_size_w w)+e))+x) (y - (i*3) + s*3);
	  graphic#put();
	done;
	graphic#move x y;
	)

  end;;


(** main iface class *)
class interface_BFR bgfile w h=
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
	  o#on_click x y;
	  ignore (interp#parse (o#get_id^".on_click("^string_of_int x^","^string_of_int y^")")) ;

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
	o#on_release x y;
	ignore (interp#parse (o#get_id^".on_release("^string_of_int x^","^string_of_int y^")")) ;

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
(*
class iface_object_types=
object
  inherit [iface_object] obj_types (new iface_object 0 0)
end;;
*)
(* some functions *)
let iface_add_object iface obj=
  iface#add_object (obj);
  let nbut=iface#get_cur_obj - 1 in
  let o=(iface#get_object_num nbut) in
  o;;

