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
open Iface_button;;
open Iface_container;;
open Iface_properties;;

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

(** NEW *)


(** IFACE MENU *)

type iface_menu_t=
  | Menu of (iface_object * iface_menu_t list)
  | MenuEntry of (iface_object);;


type iface_menu_position=
  | MenuRight
  | MenuBottom;;

class iface_menu rid ptile (pos:iface_menu_position) (mt:iface_object*iface_menu_t list)=
object(self)
  inherit iface_object 0 0 as super

  val mutable empty=false

  val mutable tobj=(fst mt)
  val mutable fond=new iface_pgraphic_object (ptile())
  val mutable menu=new iface_container ([||])

  val mutable vrect=new rectangle 0 0 0 0
  method get_vrect=vrect

  method set_layer l=
    layer<-l;
    tobj#set_layer (layer+1);
    menu#set_layer (layer+1);
    fond#set_layer (layer);

  method show()=
    super#show();
    tobj#show();
    
  method hide()=
    super#hide();
    tobj#hide();

    self#close_menu();

  method put()=
    fond#put();
    tobj#put();
    menu#put();

  method get_parent=
    if empty=false then
      super#get_parent
    else
      tobj#get_parent

  method set_parent p=
    if empty=false then
      super#set_parent p
    else
      tobj#set_parent p

  method private parent_size=
    match self#get_parent with
      | Some p-> (p#get_rect#get_w,p#get_rect#get_h)
      | None -> (0,0)

  method private parent_position=
    match self#get_parent with
      | Some p-> (p#get_rect#get_x,p#get_rect#get_y)
      | None -> (0,0)

  method private relative_position=
    match self#get_parent with
      | Some p-> (rect#get_x-p#get_rect#get_x,rect#get_y-p#get_rect#get_y)
      | None -> (0,0)

  method private pos_parent_position x y=
    let (pw,ph)=self#parent_size in
    match pos with
      | MenuRight -> ((x+pw),y)
      | MenuBottom ->(x,(y+ph));


  method private pos_parent_size w h=
    let (pw,ph)=self#parent_size in
    match pos with
      | MenuRight -> ((w+pw),h)
      | MenuBottom ->(w,(h+ph));

  method private pos_parent_size_or w h=
    let (pw,ph)=self#parent_size in
      match pos with
	| MenuRight -> ((pw),h)
	| MenuBottom ->(w,(ph))

  method private pos_position x y=
    match pos with
      | MenuRight -> ((x+tobj#get_rect#get_w),y)
      | MenuBottom ->(x,(y+tobj#get_rect#get_h));

  method private pos_size w h=
    match pos with
      | MenuRight -> ((w+tobj#get_rect#get_w),h)
      | MenuBottom ->(w,(h+tobj#get_rect#get_h));


  method move x y=
    super#move x y;
    vrect#set_position x y;
    tobj#move (x) (y);

    let (tpx,tpy)=self#parent_position in
    let (px,py)=((x-tpx),(y-tpy)) in
    let (nx,ny)=self#pos_parent_position x y in 
      fond#move nx ny;

    let (brw,brh)=fond#border_size in
      let ah=(menu#get_rect#get_h mod brh)/2 and
	  aw=(menu#get_rect#get_w mod brw)/2 in
	menu#move (nx+brw-aw) (ny+brh-ah);



  initializer
    
    menu<-new iface_vcontainer
    (
      let sms=DynArray.create() in
      let a=DynArray.create() in
      let i=ref 0 in

	List.iter ( fun v->
		      (match v with
			 | Menu (o,tl)->
			     let sm=new iface_menu (rid^":"^string_of_int !i) ptile MenuRight (o,tl) in
			       sm#set_embed true;
			       DynArray.add a (sm:>iface_object);
			 | MenuEntry o ->DynArray.add a o
		      );

		      i:= !i+1;
		  ) (snd mt);
	if DynArray.empty a then
	  empty<-true;

	DynArray.to_array a    
    );
    menu#set_halign IAlignLeft;
    self#init_size();



  method init_size()=
    menu#reset_size();
    let (brw,brh)=fond#border_size in
    fond#resize (menu#get_rect#get_w+(brw*2)) (menu#get_rect#get_h+(brh*2)); 

    rect#set_size tobj#get_rect#get_w tobj#get_rect#get_h;
    vrect#set_size (tobj#get_rect#get_w) (tobj#get_rect#get_h);


  method reset_size()=
    menu#reset_size();
    let (brw,brh)=fond#border_size in
    if menu#is_showing then (
      let (nw,nh)=self#pos_parent_size menu#get_vrect#get_w menu#get_vrect#get_h in
	vrect#set_size (nw) (nh);
    )
    else
      (
      let (nw,nh)=self#pos_parent_size_or tobj#get_vrect#get_w tobj#get_vrect#get_h in 
(*	let (nw,nh)=(tobj#get_vrect#get_w,tobj#get_vrect#get_h) in *)
	vrect#set_size (nw) (nh);
      )

  method open_menu()=
    if empty=false then (
      fond#show();
      menu#show();
    )
  method close_menu()=
    fond#hide();
    menu#hide();

  method on_mouseover x y=
    let (pnw,pnh)=self#pos_parent_size_or tobj#get_vrect#get_w tobj#get_vrect#get_h in
    let trect=new rectangle (tobj#get_vrect#get_x) (tobj#get_vrect#get_y) (menu#get_vrect#get_x - tobj#get_vrect#get_x+pnw) (menu#get_vrect#get_y - tobj#get_vrect#get_y + pnh) in


    if trect#is_position x y 
      || menu#get_vrect#is_position x y then (
	self#open_menu();
	self#reset_size();  

	if menu#is_showing then 
	  menu#on_mouseover x y; 
      ) else
	(
	  self#on_mouseout x y
	)

  method on_mouseout x y=
    menu#on_mouseout x y;
    self#close_menu();
    self#reset_size();

  method on_click x y=
    menu#on_click x y;
    self#open_menu();
    self#reset_size();




end;;

class iface_menubar rid ptile c=
object(self)
  inherit iface_object 0 0 as super

  val mutable fond=new iface_pgraphic_object (ptile())

  val mutable menus=new iface_container [||]

  val mutable vrect=new rectangle 0 0 0 0 
  method get_vrect=vrect  

  method set_layer l=
    layer<-l;
    menus#set_layer (layer+1);
    fond#set_layer (layer);

  initializer
  menus<-new iface_hcontainer (
    
    let a=DynArray.create() in
    let i=ref 0 in
	List.iter ( fun v->
		      (match v with
			 | Menu (o,tl)->
			     let me=new iface_menu (rid^":"^string_of_int !i) ptile MenuBottom (o,tl) in
			       me#set_embed true;
			       DynArray.add a (me:>iface_object);
			 | MenuEntry o -> DynArray.add a o;
			     
		      );
		      i:= !i+1;
		  ) c;
	DynArray.to_array a        
    );
    menus#set_halign IAlignLeft;
    self#init_size();


  method init_size()=
    menus#reset_size();
    let (brw,brh)=fond#border_size in
    fond#resize (menus#get_rect#get_w+(brw*2)) (menus#get_rect#get_h+(brh*2));

    rect#set_size menus#get_rect#get_w menus#get_rect#get_h;
    vrect#set_size menus#get_vrect#get_w menus#get_vrect#get_h;


  method reset_size()=
    menus#reset_size();
    
    rect#set_size menus#get_rect#get_w menus#get_rect#get_h;
    vrect#set_size menus#get_vrect#get_w menus#get_vrect#get_h;


  method move x y=
    super#move x y;
    vrect#set_position x y;

    let (brw,brh)=fond#border_size in
    let ah=(menus#get_rect#get_h mod brh)/2 and
	aw=(menus#get_rect#get_w mod brw)/2 in
      menus#move (x+brw-aw) (y+brh-ah);
      fond#move x y;

  method put()=
    fond#put();
    menus#put();
  
  method show()=
    fond#show();
    super#show();
    menus#show();
 
  method hide()=
    fond#hide();
    super#hide();
    menus#hide();


  method on_mouseover x y=
    menus#on_mouseover x y;
    self#reset_size();

  method on_mouseout x y=
    menus#on_mouseout x y;
    self#reset_size();

  method on_click x y=
    menus#on_click x y;
    self#reset_size();


end;;

