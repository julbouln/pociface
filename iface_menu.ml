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


open Core_rect;;
open Core_medias;;

open Core_event;;

open Iface_object;;
open Iface_text;;
open Iface_button;;
open Iface_container;;
open Iface_properties;;

(** Interface menus *)

(** {2 Types} *)

type iface_menu_t=
  | Menu of (iface_object * iface_menu_t list)
  | MenuEntry of (iface_object);;

type iface_menu_position=
  | MenuRight
  | MenuBottom;;

(** {2 Classes} *)

(** generic menu object *)
class iface_menu rid pattern (pos:iface_menu_position) (mt:iface_object*iface_menu_t list)=
object(self)
  inherit iface_object 0 0 as super

  val mutable empty=false

  val mutable tobj=(fst mt)
  val mutable fond=new iface_pgraphic_object (pattern())
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
			     let sm=new iface_menu (rid^":"^string_of_int !i) pattern MenuRight (o,tl) in
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

(** menubar object *)
class iface_menubar rid pattern c=
object(self)
  inherit iface_object 0 0 as super

  val mutable fond=new iface_pgraphic_object (pattern())

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
			     let me=new iface_menu (rid^":"^string_of_int !i) pattern MenuBottom (o,tl) in
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

    rect#set_size fond#get_rect#get_w fond#get_rect#get_h; 
(*    rect#set_size menus#get_rect#get_w menus#get_rect#get_h; *)
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


