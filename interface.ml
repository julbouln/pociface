(*
    Battle For Rashitoul - The ultimate strategy/arcade game
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

open Str;;
open Rect;;
open Low;;
open Video;;
open Medias;;
open Music;;
open Event_manager;;

open Otype;;
open Oxml;;
open Olua;;

(** GUI objects class definitions *)


(** parent widget *)
class iface_object w h=
object(self)
    val mutable id="none"
    method set_id i=id<-i
    method get_id=id

(*
    val mutable parent=new iface_object w h
    method set_parent p=parent<-p
    method get_parent=parent
*)

    val mutable data=0
    val mutable data1=0
    val mutable data_text=""
    val mutable showing=false

    val mutable rect=new rectangle 0 0 w h
      

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


(** container widget *)
class iface_container c=
  object (self)
    inherit iface_object 0 0 as super
    val mutable content=c

(*
    method init_content()=
      self#foreach (fun obj->
		      obj#set_parent (self:>iface_object)
		   )

    initializer
      self#init_content();
*)
    method private foreach f=
      Array.iter f content;
      
    method private reset_size()=()

    method get_rect=
      self#reset_size();
      rect
      
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
      self#foreach (let f obj=obj#put() in f)

  end;;


(** vertical container widget *)
class iface_vcontainer c=
  object (self)
    inherit iface_container c as super

    val mutable vrect=new rectangle 0 0 0 0
    method get_vrect=vrect

    method private reset_size()=
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

    method private reset_size()=
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


let text_split s=
  split_delim (regexp "[\n\t]+") s;;



(** text widget *)
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



(** label_static widget *)
class iface_label_static fnt color txt=
  object
    inherit iface_graphic_object  
    (
      new graphic_real_object 

       ("label/static/"^txt^":"^(string_of_int fnt#get_size)^":"
	^string_of_int(match color with (r,v,b) -> r)
	^string_of_int(match color with (r,v,b) -> v)
	^string_of_int(match color with (r,v,b) -> b)
	)
 
       (tile_text fnt#get_font txt color)
	  
	)
	0 0 as super


  end;;


(** label_dynamic widget *)
class iface_label_dynamic fnt color=
  object (self)
    inherit iface_object 0 0 as super

    method put()=
      if showing==true then (
	let tmp=fnt#create_text data_text color in 
        rect#set_size (tile_get_w tmp) (tile_get_h tmp);
	tile_put tmp (rect#get_x) (rect#get_y);
	tile_free tmp      
)
  end;;


(* text entry *)
(*
class iface_text_entry fnt color=
  object (self)
    inherit iface_object 0 0 as super
    val mutable data_text=""
    val mutable last_key=""
val mutable clicked=false

    method put()=
      let c=(!cur_key) in
	(
	  match c with
	    | "none"->();
	    | "return"->();
	    | "backspace"->if last_key<>c && (String.length data_text)>0 then data_text<-(String.sub data_text 0 (String.length data_text-1));
	    | "space"->();
	    | _ ->if last_key<>c then data_text<-data_text^(c);
	);
	last_key<-c;
      if showing==true then (
	if data_text<>"" then (
	let tmp=fnt#create_text data_text color in 
          rect#set_size (tile_get_w tmp) (tile_get_h tmp);
	  tile_put tmp (rect#get_x) (rect#get_y);
	  tile_free tmp      
	)
      )
  end;;
*)

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


(* will be in Poccore.Interface *)

(* outf8 *)
class utf8=
object(self)
  val mutable str=""

  method set v=str<-v
  method get=str

  method length=UTF8.length str
  method sub p l=String.sub str (self#byte_get p) (self#byte_get l)

  method byte_get n=UTF8.nth str n
  method byte_length=String.length str

end;;


class text_edit=
object(self)
  val mutable text=""
  method get_text=text

  val mutable utf=new utf8

  method utf_length=UTF8.length text

  val mutable cur_pos=0;
  method get_cur_pos=cur_pos
  method get_cur_utf_pos=UTF8.nth text cur_pos
  method get_utf_pos n=UTF8.nth text n

  method insert_char c=
    let p1=String.sub text 0 (self#get_cur_utf_pos) and
	p2=String.sub text (self#get_cur_utf_pos) (String.length text - self#get_cur_utf_pos) in
      text<-String.concat "" [p1;c;p2]

  method add_char c=
    text<-String.concat "" [text;c];

  method del_last_char()=
    if (UTF8.length text > 0) then
      text<-String.sub text 0 (UTF8.last text);

  method del_cur_char()=
    if (UTF8.length text > 0) then (
      let p1=String.sub text 0 (self#get_utf_pos (cur_pos-1)) and
	  p2=String.sub text (self#get_cur_utf_pos) (String.length text - self#get_cur_utf_pos) in
	text<-String.concat "" [p1;p2]
    )
  method set_text t=cur_pos<-0;text<-t

  method parse c u=
    match c with
      | KeySpace ->self#add_char " ";cur_pos<-cur_pos + 1;
(*      | KeyChar ch->self#add_char ch *)
      | KeyBackspace ->if cur_pos>0 then (self#del_cur_char();cur_pos<-cur_pos - 1);
      | KeyReturn -> ()
      | KeyShift -> ()
      | KeyUp -> ()
      | KeyDown -> ()
      | KeyLeft -> if cur_pos>0 then cur_pos<-cur_pos - 1
      | KeyRight -> if cur_pos<self#utf_length then cur_pos<-cur_pos + 1
      | KeyEchap -> ()
      | KeyCtrl -> ()
      | KeyAlt -> ()
      | _ ->
	  match u with
	    | KeyUnicode ch->let c=(UTF8.init 1 (fun i->ch)) in
		self#insert_char c;cur_pos<-cur_pos + 1;
	    | _ ->()


end;;


exception Text_error of string;;

(* FIXME must inherit graphic_generic_object *)
class text id fnt=
object(self)
  val mutable graphic=new graphic_generic_object id


  val mutable id=id
  method set_id i=id<-i


  val mutable max_size=16
  method set_max_size s=max_size<-s
  method get_max_size=max_size

  val mutable text=[""]
  method get_text=text

  method private cut_string s=
    let a=DynArray.create() in
    let ss=UTF8.length s/max_size in
      for i=0 to ss do

	let md=UTF8.length s - (i*max_size) in
	let l=if md>=max_size then max_size else md in
	let cs=UTF8.nth s (i*max_size) and
	    ce=UTF8.nth s ((i*max_size) + l) in
	  try 
	    let ns=String.sub s cs (ce-cs)
in
	      if String.length ns>0 then
		DynArray.add a ns
	  with Invalid_argument x -> (raise (Text_error "cut_string"));
      done;
      DynArray.to_list a
	

  method set_text t=
(*    text<-split_delim (regexp "[\n\t]+") t; *)
    text<-[""];
    text<-self#cut_string t;

    graphic<-
    new graphic_dyn_object (id^"/text") (List.length self#get_text)
      (function k-> (
	 fnt#create_text (List.nth self#get_text k) self#get_color
       ));

    let cw=ref 0 and
      ch=ref 0 in
      for i=0 to (List.length (self#get_text))-1 do
	let pos=fnt#sizeof_text (List.nth (self#get_text) i) in
	  if !cw<(fst pos) then
	    cw:=(fst pos);
	  ch:=!ch + (snd pos);
      done;
      graphic#get_rect#set_size (!cw) (!ch);
      
  val mutable color=(0,0,0)
  method get_color=color
  method set_color c=color<-c


  method move x y=graphic#move x y
  method get_rect=graphic#get_rect

  method put()=
    for i=0 to (List.length self#get_text)-1 do
      let ty=(graphic#get_rect#get_y) in
	graphic#set_cur_tile i;
	graphic#move (graphic#get_rect#get_x) (ty+(i*fnt#get_height));
	graphic#put();	
	graphic#move (graphic#get_rect#get_x) (ty);
    done;

end;;


(** text edit widget multiline *)
class iface_text_edit_box fnt color bw il=
  object (self)
    inherit iface_object bw (fnt#get_height) as super

    val mutable lines=il
    method private set_lines l=lines<-l
    method private get_lines=lines

    val mutable text=new text "text_edit" fnt
    val mutable te=new text_edit
   
    method private get_textedit=te

    method on_keypress e=
      (match (parse_key e.ebut) with
      | KeyBackspace -> te#parse (parse_key e.ebut) (parse_unicode e.ey)
      | KeyReturn -> te#parse (parse_key e.ebut) (parse_unicode e.ey)
      | KeyShift -> ()
      | KeyUp -> te#parse (parse_key e.ebut) (parse_unicode e.ey)
      | KeyDown -> te#parse (parse_key e.ebut) (parse_unicode e.ey)
      | KeyLeft -> te#parse (parse_key e.ebut) (parse_unicode e.ey)
      | KeyRight -> te#parse (parse_key e.ebut) (parse_unicode e.ey)
      | _ -> 
	  if UTF8.length te#get_text< lines*text#get_max_size then
	te#parse (parse_key e.ebut) (parse_unicode e.ey));

      self#set_data_text (te#get_text); 

      text#set_text (data_text);




    initializer
      text#set_max_size (bw/8);
      rect<-new rectangle 0 0 (bw+12) (fnt#get_height+12)

    method move x y=
  
      rect#set_position (x-6) (y-6)


    method on_click x y=
      let rx=x-self#get_rect#get_x  in
	click()



    val mutable cur_refresh=30
    val mutable cur_c=0

    method get_data_text=te#get_text;

    method private auto_lines()=
      let l=List.length text#get_text in
	if l<>0 then
	  lines<-l;

    method put()=

      if showing==true then (	  
	rect#set_size (bw+12) ((fnt#get_height*lines)+12);
	  let t=tile_rect (bw+12) ((fnt#get_height*lines)+12) (0,0,0) in
	    tile_put t (rect#get_x) (rect#get_y);
	    tile_free t;
	  let bg=tile_box (bw+10) ((fnt#get_height*lines)+10) (200,200,200) in
	    tile_put bg (rect#get_x+1) (rect#get_y+1);
	    tile_free bg;
	if te#get_text<>"" then(

	  text#move (rect#get_x+6) (rect#get_y+6);
	  text#set_id self#get_id;

	  text#put()
      );	    
	if focused  then (
	    if cur_c>cur_refresh/2 then (
	      let cu=tile_rect 1 (fnt#get_height + 4) (0,0,0) in
	      let cline=te#get_cur_pos/text#get_max_size in
	      let mline=(te#get_cur_pos - (cline*text#get_max_size)) in
	      let tt=
		let r=try List.nth text#get_text (cline) with Failure x->""  in
		  if r<>"" then
		    try 
		      let u=(UTF8.nth r mline) in
			if u>0 then
			  String.sub r 0 u
			else ""
		    with Invalid_argument x -> (raise (Text_error ("put:"^string_of_int mline)));
		  else "" in
	      let (cw,ch)=fnt#sizeof_text tt in
		tile_put cu (rect#get_x + cw +6) (rect#get_y-2+6 + ch*(cline));
		tile_free cu;
	    );
	    if cur_c=cur_refresh then cur_c<-0
	    else cur_c<-cur_c+1

	)
      )

  end;;

(** text edit widget 1 line *)
class iface_text_edit fnt color bw=
object
  inherit iface_text_edit_box fnt color bw 1 as super
end

(** password edit widget *)
class iface_password_edit fnt color bw=
  object (self)
    inherit iface_text_edit fnt color bw as super
      
    method set_data_text t=
      let tmp=ref "" in
      for i=0 to String.length t - 1 do
	tmp:=String.concat "" [!tmp;"*"];
      done;
      data_text<- !tmp;

  end;;

class iface_dialog w h bg fnt text (bl:(string*iface_object) list)  (el:(string*iface_object*iface_object) list)=
object(self)
  inherit iface_graphic_file_object bg w h as super

  initializer
    List.iter (
      fun (n,o)->
	self#add_button n o;
    ) bl;

    List.iter (
      fun (n,o,e)->
	self#add_entry n o e;
    ) el;


  val mutable lab=new iface_label_static fnt (0,0,0) text
  val mutable buttons=Hashtbl.create 2
  val mutable entries=Hashtbl.create 2
  
  method add_button (n:string) (b:iface_object)=
    Hashtbl.add buttons n b
  
  method add_entry (n:string) (lb:iface_object) (ent:iface_object)=
    Hashtbl.add entries n (lb,ent)

  method put()=
    super#put();
    lab#put();
(*    Hashtbl.iter (
      fun k o->
	o#put();
    ) buttons;
*)  
    Hashtbl.iter (
      fun k (l,e)->
	l#put();
	e#put();
    ) entries;    

  method show()=
    super#show();
    lab#show();
    
    Hashtbl.iter (
      fun k o->
	o#show();
    ) buttons;

    Hashtbl.iter (
      fun k (l,e)->
	l#show();
	e#show();
    ) entries;

  method hide()=
    super#hide();
    lab#hide();
    
    Hashtbl.iter (
      fun k o->
	o#hide();
    ) buttons;

    Hashtbl.iter (
      fun k (l,e)->
	l#hide();
	e#hide();
    ) entries;


  method move x y=
    super#move x y;
    lab#move (x+16) (y+16);

    let n=ref 0 in  
      Hashtbl.iter (
	fun k o->
	  o#move (x+ (!n * o#get_rect#get_w) + 16) (y+(h -o#get_rect#get_h-16)) ;
	  n:= !n + 1;
      ) buttons;

      let n=ref 0 in  
	Hashtbl.iter (
	  fun k (l,e)->
	    l#move (x + 16) (y+(!n * e#get_rect#get_h)+lab#get_rect#get_h + 32) ; 
	    e#move (x + l#get_rect#get_w + 16 + 64) (y+(!n * e#get_rect#get_h) + lab#get_rect#get_h + 32) ;
	    n:= !n + 1;
	) entries;



end;;


(** special graphic resize with 9 tiles *)
class iface_rgraphic_object file iw ih=
object(self)
  inherit iface_object iw ih as super

  val mutable gen=new graphic_simple_from_func (file^"/gen") (fun()->tile_load file)
  val mutable gr=new graphic_generic_object file

  val mutable crect=new rectangle 0 0 0 0

  val mutable w=iw
  val mutable h=ih

  method resize nw nh=
    w<-nw;h<-nh;
    self#init();

  method private init()=
    crect#set_size (gen#get_rect#get_w/3) (gen#get_rect#get_h/3);
    gr<-new graphic_object (crect#get_w) (crect#get_h) file false false;
    
  initializer
    self#init()
(*

036
147
258

*)
  method move x y=
    super#move x y;
    gr#move x y;

  method put()=
    if self#is_showing then (
      let cw=w/crect#get_w and
	  ch=h/crect#get_h in    
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


exception Iface_object_not_found of string;;

(** main iface class *)
class interface bgfile w h=
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
	 ignore (interp#parse (o#get_id^".on_click("^string_of_int x^","^string_of_int y^")")) ;
	o#on_click x y;
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
	 ignore (interp#parse (o#get_id^".on_release("^string_of_int x^","^string_of_int y^")")) ;
      o#on_release x y;
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

class iface_object_types=
object
  inherit [iface_object] obj_types (new iface_object 0 0)
end;;


class xml_font_parser=
object
  inherit xml_parser

  val mutable file="none"
  val mutable size=0

  method get_val=new font_object file size

  method tag=""
  method parse_attr k v=
    match k with
      | "path" -> file<-v
      | "size" -> size<-int_of_string v
      | _ -> ()
  method parse_child k v=()


end;;

class iface_objects_container_parser parser=
object
 inherit [((string * string * iface_object) array)] xml_list_parser "iface_object" (parser) 
end;;

class iface_object_parser=
object(self)
  inherit xml_parser
  val mutable nm=""
  val mutable id=""
  val mutable file=""
  val mutable w=0
  val mutable h=0

  val mutable iw=0
  val mutable ih=0

  val mutable x=0
  val mutable y=0

  val mutable r=0
  val mutable g=0
  val mutable b=0

  val mutable fnt=new font_object "none" 0
  val mutable txt=""
  val mutable show=false

  val mutable container=false
  val mutable oarr=Array.create 100 ("none","",new iface_object 0 0)

  method get_oarr=oarr
  method is_container=container

(*  method oarr_to_arr oa=
    let a=DynArray.create() in
    Array.iter (fun (id,lua,o)->
		  if id<>"none" then
		    DynArray.add a o
	       ) oa;
      DynArray.to_array a
*)

  val mutable lua=""
  method tag=""



  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | "id" ->id<-v
      | _ -> ()
   
  method parse_child k v=
    match k with
      | "file" -> let p=(new xml_string_parser "path") in p#parse v;file<-p#get_val    
      | "size" -> let p=(new xml_size_parser ) in p#parse v;w<-p#get_w;h<-p#get_h;
      | "isize" -> let p=(new xml_size_parser ) in p#parse v;iw<-p#get_w;ih<-p#get_h;
      | "position" -> let p=(new xml_point_parser ) in p#parse v;x<-p#get_x;y<-p#get_y;
      | "color" -> let p=(new xml_color_parser ) in p#parse v;r<-p#get_r;g<-p#get_g;b<-p#get_b;
      | "font" -> let p=(new xml_font_parser ) in p#parse v;fnt<-p#get_val
      | "text" -> let p=(new xml_string_parser "str") in p#parse v;txt<-p#get_val
      | "lua" -> lua<-v#get_pcdata;
      | "show" -> show<-true
(*      | "container" -> let p=(new iface_objects_container_parser (fun()->new iface_object_parser)) in p#parse v;oarr<-p#get_array.(0);container<-true
*)
      | _ -> ()

  method get_val=
    let o=(
    match nm with
      | "iface_button" -> new iface_button file (video#f_size_w w) (video#f_size_h h)
      | "iface_button_with_label" -> new iface_button_with_label fnt txt file (video#f_size_w w) (video#f_size_h h)
      | "iface_button_icon" -> new iface_button_icon file (video#f_size_w w) (video#f_size_h h) iw ih
      | "iface_label_static" -> new iface_label_static fnt (r,g,b) txt
      | "iface_label_dynamic" -> new iface_label_dynamic fnt (r,g,b)
      | "iface_text_edit" -> new iface_text_edit fnt (r,g,b) (video#f_size_w w)
      | "iface_password_edit" -> new iface_password_edit fnt (r,g,b) (video#f_size_w w)
      | "iface_graphic_object" -> new iface_graphic_file_object file (video#f_size_w w) (video#f_size_h h)
(*      | "iface_container_object" -> new iface_container (self#oarr_to_arr oarr)*)
      | _ -> new iface_object (video#f_size_w w) (video#f_size_h h)
    ) in
      o#move x y;
      if show then
	o#show();
      o#move (video#f_size_w x) (video#f_size_h y);
      if container=true then (
	(oarr)
      )
      else (	
	[|(id,lua,o)|]
      )
end;;


class iface_objects_parser name=
object(self)
  inherit [((string * string * iface_object) array)] xml_list_parser name (fun()->new iface_object_parser)
  
  val mutable bg="none"
  val mutable w=0
  val mutable h=0


  method parse_attr k v=
    match k with
      | "background" ->bg<-v
      | "w" ->w<-(int_of_string v)
      | "h" ->h<-(int_of_string v)
      | _ -> ()

  method get_val=
    let iface=new interface bg w h in
    let l=self#get_list in
      List.iter (fun ol->
		   Array.iter (fun (n,l,o)->
				 iface#add_object_n n o;
				 
				 let l2=(n^"={};\n")^l in
				   print_string l2;
				   iface#get_interp#parse l2;()
			      ) ol
		) l;
      iface
      
end;;


(*
<iface background="none" w="1024" h="768">
<iface_object type="iface_button_with_label" id="button1">
  <file path="medias/iface/button.png"/>
  <size w="100" h="40"/>
  <position x="10" y="10"/>
  <color r="0" g="0" b="0"/>
  <font path="medias/fonts/Vera.ttf" size="8"/>
  <text str="Ok"/>
  <lua>
   function button1.on_click (x,y)
    print("ok")
   end
  </lua>
</iface_object>
</iface>
*)

let iface_from_xml f=
    let iface_xml=new xml_node (Xml.parse_file f) in
    let p=new iface_objects_parser "iface" in
      p#parse iface_xml;
      
      p#get_val;;


(* some functions *)
let iface_add_object iface obj=
  iface#add_object (obj);
  let nbut=iface#get_cur_obj - 1 in
  let o=(iface#get_object_num nbut) in
  o;;

