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

open Str;;

open Low;;

open Rect;;
open Video;;
open Medias;;
open Event_manager;;

open Iface_object;;



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
class text nid fnt (col:color)=
object(self)
  inherit graphic_generic_object nid
  val mutable graphic=new graphic_generic_object nid


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
      rect#set_size (!cw) (!ch);
      
  val mutable color=col
  method get_color=color
  method set_color c=color<-c

(*    graphic#move x y; *)
(*
    for i=0 to (List.length self#get_text)-1 do
      let ty=(graphic#get_rect#get_y) in
	graphic#move (graphic#get_rect#get_x) (ty+(i*fnt#get_height));
	graphic#move (graphic#get_rect#get_x) (ty);
    done;
*)
(*  method get_rect=graphic#get_rect *)

  method put()=
    for i=0 to (List.length self#get_text)-1 do
      graphic#move (rect#get_x) (rect#get_y+(i*fnt#get_height));
      graphic#set_cur_tile i;
      graphic#put();	

    done;

end;;


(** text edit widget multiline *)
class iface_text_edit_box rid bptile fnt color bw il=
  object (self)
    inherit iface_object bw (fnt#get_height) as super

    val mutable bg=new iface_rgraphic_object rid bptile

    val mutable lines=il
    method private set_lines l=lines<-l
    method private get_lines=lines

    val mutable text=new text (rid^"/text_edit") fnt color
    val mutable te=new text_edit
   
    method private get_textedit=te

    method get_data_text=te#get_text;

    method set_data_text t=
      if t="" then
	te#set_text ""; 
      super#set_data_text (t); 
      text#set_text (data_text);

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



    initializer
      text#set_max_size (bw/8);
      let (brw,brh)=bg#border_size in
      rect<-new rectangle 0 0 (bw+(brw*2)) (fnt#get_height+(brh*2));
      bg#resize rect#get_w rect#get_h;

    method move x y=
      let (brw,brh)=bg#border_size in
      text#move (x+brw) (y+brh);
      bg#move (x) (y);
      rect#set_position (x) (y)


    method on_click x y=
      let rx=x-self#get_rect#get_x  in
	click()

    val mutable cur_refresh=30
    val mutable cur_c=0

    method private auto_lines()=
      let l=List.length text#get_text in
	if l<>0 then
	  lines<-l;

    method hide()=
      super#hide();
      bg#hide();

    method show()=
      super#show();
      bg#show();

    method put()=

      if showing==true then (	  
	let (brw,brh)=bg#border_size in
	rect#set_size (bw+(brw*2)) ((fnt#get_height*lines)+(brh*2));
	bg#put();
	if te#get_text<>"" then(
	  text#set_id self#get_id;
	  text#put()
      );	    
	if focused  then (
	    if cur_c>cur_refresh/2 then (
	      let cu=tile_rect 1 (fnt#get_height + 4) color in
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
	      let (brw,brh)=bg#border_size in
		tile_put cu (rect#get_x + cw +(brw)) (rect#get_y-2+(brh) + ch*(cline));
		tile_free cu;
	    );
	    if cur_c=cur_refresh then cur_c<-0
	    else cur_c<-cur_c+1

	)
      )

  end;;

(** text edit widget 1 line *)
class iface_text_edit rid bptile fnt color bw=
object
  inherit iface_text_edit_box rid bptile fnt color bw 1 as super
end

(** password edit widget *)
class iface_password_edit rid bptile fnt color bw=
  object (self)
    inherit iface_text_edit rid bptile fnt color bw as super
      
    method set_data_text t=
      let tmp=ref "" in
      for i=0 to String.length t - 1 do
	tmp:=String.concat "" [!tmp;"*"];
      done;
      data_text<- t;
      text#set_text (!tmp);


  end;;
