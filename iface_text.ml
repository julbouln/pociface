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



open Str;;

open Value_val;;

open Core_rect;;
open Core_video;;
open Core_drawing;;
open Core_medias;;
open Core_graphic;;
open Core_event;;

open Binding;;

open Iface_object;;


(** Interface text objects *)

let string_eol=
  (String.make 1 '\n')

let text_split s=
  split_delim (regexp "[\n\t]+") s;;


(** label_static object *)
class iface_label_static fnt_t color txt=
  object
    inherit iface_graphic_object  
    (
      new graphic_object_text fnt_t [txt] color
(*      new graphic_real_object 

       ("label/static/"^txt^":"^(string_of_int fnt#get_size)^":"
	^string_of_int(match color with (r,v,b) -> r)
	^string_of_int(match color with (r,v,b) -> v)
	^string_of_int(match color with (r,v,b) -> b)
	)

	(fnt#create_text txt color)
*)	  
	)
	0 0 as super


  end;;

(** label_dynamic object *)
(*class iface_label_dynamic fnt color=
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
*)

(** text edit (no graphic) *)
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
  method set_text t=
    cur_pos<-UTF8.length t;
(*    cur_pos<-0; *)
    text<-t

  method parse c u=
    match c with
      | KeySpace ->self#add_char " ";cur_pos<-cur_pos + 1;
(*      | KeyChar ch->self#add_char ch *)
      | KeyBackspace ->if cur_pos>0 then (self#del_cur_char();cur_pos<-cur_pos - 1);
      | KeyReturn -> self#add_char (String.make 1 '\n');cur_pos<-cur_pos + 1;
      | KeyShift -> ()
      | KeyUp -> ()
      | KeyDown -> ()
      | KeyLeft -> if cur_pos>0 then cur_pos<-cur_pos - 1
      | KeyRight -> if cur_pos<self#utf_length then cur_pos<-cur_pos + 1
      | KeyEchap -> ()
      | KeyCtrl -> ()
      | KeyAltR -> ()
      | KeyAltL -> ()
      | KeyAltGr -> () 
      | KeyNumLock -> () 
      | KeyCapsLock -> () 
      | _ ->
	  match u with
	    | KeyUnicode ch->let c=(UTF8.init 1 (fun i->ch)) in
		self#insert_char c;cur_pos<-cur_pos + 1;
	    | _ ->()


end;;



(** text box *)
class iface_text_box rid bpgraph fnt_t color bw il=
object(self)
    inherit iface_object bw 0 as super

    val mutable bg=new iface_pgraphic_object bpgraph
    val mutable text=new graphic_text (rid^"/text_box") fnt_t color
    val mutable fnt=(font_vault#get_cache_simple ((get_font_id fnt_t)^string_of_int (get_font_size fnt_t)))

    initializer
      text#set_lines il;

      let (brw,brh)=bg#border_size in
	text#set_max_size (bw-(brw*3));
	bg#resize (bw+(brw*2)) (il*fnt#get_height+(brh*2));
	rect#set_size bg#get_rect#get_w bg#get_rect#get_h;

    method resize w h=
      let (brw,brh)=bg#border_size in
	rect#set_size w h;
	text#set_max_size (w);
	bg#resize (w) (h);
	text#set_lines (h/fnt#get_height);

    method move x y=
      super#move (x) (y);
      bg#move (x) (y);
      let (brw,brh)=bg#border_size in
(*      let ah=(text#get_rect#get_h mod brh)/2 and
	  aw=(text#get_rect#get_w mod brw)/2 in*)
      text#move (x+brw) (y+brh);

    method private auto_lines()=
      let l=List.length text#get_text in
	if l<>0 then
	  text#set_lines l;

    method hide()=
      super#hide();
      bg#hide();

    method show()=
      super#show();
      bg#show();

    method set_data_text t=
      super#set_data_text (t); 
      text#set_text (data_text);
(*      if t<>"" then (
      )
*)
    method put()=
      if showing==true then (	  
	let (brw,brh)=bg#border_size in

	bg#put();
	if data_text<>"" then (
	  text#set_id self#get_id;
	  text#put()
	);

      )

end;;

(** text edit box *)
class iface_text_edit_box rid bptile fnt_t color bw il=
  object (self)
    inherit iface_text_box rid bptile fnt_t color bw il as super
    val mutable te=new text_edit
    val mutable cursor=new graphic_object

    initializer
      cursor<-new graphic_from_drawing "cursor" (
	fun()->
	  let dr=drawing_vault#new_drawing() in
	    dr#exec_op_create_from_list "rect" 
	      [
		`Size(1,(fnt#get_height));
		`Color color
	      ];
	    [|dr|]
      )   

    method grab_focus=true	

    method private get_textedit=te

    method get_data_text=te#get_text;

    method set_data_text t=
      super#set_data_text (t);       
(*      if t="" then *)
      te#set_text t;

    method on_keypress (k,utfk)=
      te#parse k utfk;
(*
      (match (parse_key e.ebut) with
      | KeyShift -> ()
      | _ -> 
	  (*	  if UTF8.length te#get_text< lines*text#get_max_size then *)
	  (*	  text#set_text te#get_text;
		  if UTF8.length te#get_text<= text#lines_size text#get_lines then *)
	  te#parse (parse_key e.ebut) (parse_unicode e.ey);
	  (*	  text#set_text ""; *)
      );
  
*)  
      self#set_data_text (te#get_text); 


    val mutable cur_refresh=30
    val mutable cur_c=0

    method put()=
      super#put();
      if showing=true then
	if focused then (
	  if cur_c>cur_refresh/2 then (
(*	    let cu=tile_rect 1 (fnt#get_height) color in *)
	    let cline=text#line_of_pos te#get_cur_pos in
	    let mline=(te#get_cur_pos - (text#lines_size cline)) in
	    let tt=
(*	      print_string ("curs:"
			    ^string_of_int te#get_cur_pos^","
			    ^string_of_int (text#lines_size (text#get_lines)));
	      print_newline();
*)
	      if te#get_cur_pos<=(text#lines_size (text#get_lines)) then
	      (try
	      let utf=new utf8 in
		utf#set (List.nth text#get_text cline);
		utf#sub 0 mline 
	       with Failure x->"")
	      else ""
	       in

	    let (cw,ch)=fnt#sizeof_text tt in
	    let (brw,brh)=bg#border_size in 
	      cursor#move (rect#get_x + cw +(brw)) (rect#get_y-2+(brh) + ch*(cline));
	      cursor#put();
(*	      tile_put cu (rect#get_x + cw +(brw)) (rect#get_y-2+(brh) + ch*(cline));
	      tile_free cu;*)
	  );
	  if cur_c=cur_refresh then cur_c<-0
	  else cur_c<-cur_c+1
	)

  end;;

(** text edit object 1 line *)
class iface_text_edit rid bptile fnt_t color bw=
object
  inherit iface_text_edit_box rid bptile fnt_t color bw 1 as super
end

(** password edit object *)
class iface_password_edit rid bptile fnt_t color bw=
  object (self)
    inherit iface_text_edit rid bptile fnt_t color bw as super
      
    method set_data_text t=
      let tmp=ref "" in
      for i=0 to String.length t - 1 do
	tmp:=String.concat "" [!tmp;"*"];
      done;
      data_text<- t;
      text#set_text (!tmp);


  end;;



