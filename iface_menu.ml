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



(** IFACE MENU *)

type iface_menu_t=
  | Menu of (iface_object * iface_menu_t list)
  | MenuEntry of (iface_object);;


type iface_menu_position=
  | MenuRight
  | MenuBottom;;

class iface_menu (pos:iface_menu_position) (mt:iface_object*iface_menu_t list) (parent:(iface_object*iface_menu_t list) option)=
object(self)
  val mutable tobj=(fst mt)

(*  val mutable tfond=new iface_rgraphic_object "medias/iface/motif_editor.png" 0 0 *)
  val mutable fond=new iface_rgraphic_object "medias/iface/motif_editor.png" 0 0
	
  inherit iface_vcontainer
    (
      let a=DynArray.create() in
	List.iter ( fun v->
		      (match v with
			 | Menu (o,tl)->
			     DynArray.add a (new iface_menu MenuRight (o,tl) (Some mt):>iface_object)
			 | MenuEntry o ->DynArray.add a o
		      );

		  ) (snd mt);
	DynArray.to_array a    
    ) as super

  method show()=
(*    tfond#show(); *)
    tobj#show();
  
  
  method hide()=
(*    tfond#hide(); *)
    tobj#hide();
    fond#hide();
    super#hide()

  method put()=
    fond#put();
(*    tfond#put(); *)
    tobj#put();
    super#put();

  method is_showing=tobj#is_showing

  val mutable rrect=new rectangle 0 0 0 0

  method get_vrect=
    self#reset_size();
    vrect

  method get_rect=
    self#reset_size();
    rrect

  method private parse_tree nd f=
    let rec parse_tree_t t f=
    List.iter (
      fun v->
	match v with
	  | Menu (o,tl)->f false o;parse_tree_t tl f;	      
	  | MenuEntry o -> f true o
    ) t in
      parse_tree_t nd f


  method is_last()=
    let r=ref false in
      self#parse_tree (snd mt) (fun nr o->r:=nr);
      !r

  method private pos_position x y=
    let (pw,ph)=self#size_from_parent() in
    match pos with
      | MenuRight -> ((x+pw),y)
      | MenuBottom ->(x,(y+tobj#get_rect#get_h));

  method private pos_size w h=
    match pos with
      | MenuRight -> ((w+tobj#get_rect#get_w+32),h)
      | MenuBottom ->(h,(h+tobj#get_rect#get_h));

  method move x y=
    let (nx,ny)=self#pos_position x y in
      fond#move (nx) (ny);
      super#move (nx+8) (ny+8);
(*    tfond#move x y; *)
      tobj#move (x) (y);
      rrect#set_position x y;
      vrect#set_position x y;
    
  method close()=
    fond#hide();
    super#hide();
    

  method reset_size()=
    super#reset_size();
    rrect#set_size (tobj#get_rect#get_w) (tobj#get_rect#get_h);
(*(rect#get_h+tobj#get_rect#get_h + 8 + 16);*)

    let (rnw,rnh)=self#pos_size rect#get_w rect#get_h in
    rect#set_size (rnw) (rnh);

    let (vnw,vnh)=self#pos_size vrect#get_w vrect#get_h in
    vrect#set_size (vnw) (vnh);



  method private init()=
    self#reset_size();
    fond#resize (rect#get_w) (rect#get_h); 
(*    tfond#resize (tobj#get_rect#get_w+16) (tobj#get_rect#get_h);	*)
    let (pw,ph)=self#size_from_parent() in
      fond#resize pw ph;


  method init_tree()=
    self#parse_tree (snd mt) (
      fun r o->
	if r then
	  o#append_click self#close
    );
				
  method get_parent=
    match parent with
      | Some (o,tl)->o
      | None->new iface_object 0 0



  method size_from_parent()=
    match parent with
      | Some v->
	  let (o,tl)=v in
	  let w=ref 0 in
	    List.iter (
	      fun (ct)->
		match ct with
		  | Menu (co,ctl) ->
		    if !w <co#get_rect#get_w then
		      w:=co#get_rect#get_w		
		  | MenuEntry (co) ->
		    if !w <co#get_rect#get_w then
		      w:=co#get_rect#get_w		
	    ) tl;
	    ((!w+16),(rect#get_h+16));	    
      | None->(0,0)
	  
  method init_parent()=
    (
      match parent with
	| Some v->
	    let (o,tl)=v in
	    List.iter (
	      fun ct->
		match ct with
		  | Menu (co,ctl) ->
(*		      if co<>(fst mt) then
		      co#append_click self#close
*)
		      List.iter (		      
			fun cct->			  
			  match cct with
			    | Menu (cco,cctl) ->
				if cco<>(fst mt) then
				cco#append_click self#close
			    | MenuEntry (cco) -> 
				cco#append_click self#close

		      ) ctl;

		  | MenuEntry co ->
		      co#append_click self#close
	    ) tl;
	| None->()
	    
    );

  initializer
    self#init();
(*    self#init_parent(); *)
    self#init_tree(); 


  method is_tobj x y=
    x > tobj#get_rect#get_x 
    && x < (tobj#get_rect#get_w + tobj#get_rect#get_x) 
    && y > tobj#get_rect#get_y 
    && y < (tobj#get_rect#get_h + tobj#get_rect#get_y) 


      
  method on_click x y=


   if self#is_tobj x y then 
      (
	if super#is_showing then
	  (
	    fond#hide();
	    super#hide()
	  )
	else
	  (
	    fond#show();
	    super#show()
	  )
      );


    if self#is_showing then (
      tobj#on_click x y;
(*      super#on_click x y; *)
   );

 
    if super#is_showing then ( 
      self#foreachi (
	(fun i obj->
	   if x > obj#get_vrect#get_x 
	     && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	     && y > obj#get_vrect#get_y 
	     && y < (obj#get_vrect#get_h + obj#get_vrect#get_y) 
	   then (
	     obj#on_click x y;
	   )
	));
    );    



    print_string "IFACE : menu click";print_newline();


(*    click();	     *)


(*    super#on_click x y; *)

(* show hide menu with mouse click *)
 

(*  method on_mouseout x y=
    super#on_mouseout x y;
*)

end;;



class ['a] iface_tool (r:'a) fnt label f w h=
object(self)
  inherit iface_button_icon f 32 32 w h as super
  val mutable lab=new iface_label_static fnt (0,0,0) label

  val mutable rval=r
  method get_rval=rval

  method get_rect=
    self#reset_size();
    rect

  method get_vrect=
    self#reset_size();
    rect



  method private reset_size()=
    rect#set_size (32+lab#get_rect#get_w) (32);


  method show()=
    super#show();
    lab#show();

  method hide()=
    super#hide();
    lab#hide();

  method move x y=
    super#move (x+8) (y+8);
    lab#move (x+36+8) (y+8);

  method put()=
    super#put();
    lab#put();
end;;



class iface_menubar c=
object(self)
  inherit iface_hcontainer (
    
    let a=DynArray.create() in
	List.iter ( fun v->
		      (match v with
			 | Menu (o,tl)->DynArray.add a (new iface_menu MenuBottom (o,tl) (Some (o,tl)) :>iface_object)
			 | _ -> ()
			     
		      );
		      
		  ) c;
	DynArray.to_array a        
    ) as super

  initializer
    self#reset_size();
    
  method on_click x y=    
    super#on_click x y;
    if super#is_showing then (
      self#foreachi (
	(fun i obj->
	   if x > obj#get_vrect#get_x 
	     && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	     && y > obj#get_vrect#get_y 
	     && y < (obj#get_vrect#get_h + obj#get_vrect#get_y) 
	   then (
	     obj#on_click x y;
	   )
	));
    );    


end;;



class virtual ['a] iface_toolbox (iv:'a) (c:('a) iface_tool array) =
object(self)
  inherit iface_vcontainer c as super
  val mutable selected=new graphic_real_object "selected" (tile_rect 32 32 (255,0,0))

  method move_selected x y=selected#move x y

  val mutable current_val=iv 
  method get_current=current_val
  method set_current i=
       let o=c.(i) in
	 current_val<-o#get_rval;
	 self#move_selected o#get_rect#get_x o#get_rect#get_y
  initializer 
    selected#move (-32) (-32);
    self#reset_size();

  method get_vrect=rect
  method on_click x y=
    print_string "IFACE : toolbox click";print_newline();
    let t=ref (-1) in
      self#foreachi (
	let f i obj=
	  if x > obj#get_vrect#get_x 
	    && x < (obj#get_vrect#get_w + obj#get_vrect#get_x) 
	    && y > obj#get_vrect#get_y 
	    && y < (obj#get_vrect#get_h + obj#get_vrect#get_y) 
	  then
	    t:=i
	in f
      );
      if (!t)<>(-1) then
	self#set_current !t;

    super#on_click x y;

	
  method move x y=
    super#move x y;
    
  method put()=
    super#put();
    if self#is_showing then
      selected#put();

end;;

