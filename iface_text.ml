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
open Str;;

open Generic;;
open Rect;;
open Video;;
open Medias;;
open Event;;
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

(** utf8 *)
class utf8=
object(self)
  val mutable str=""

  method set v=str<-v
  method get=str

  method length=UTF8.length str
  method sub p l=
    String.sub str (self#byte_get p) ((self#byte_get (p+l))-(self#byte_get p))

  method byte_get n=UTF8.nth str n
  method byte_length=String.length str

end;;

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
  method set_text t=cur_pos<-0;text<-t

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
      | KeyAlt -> ()
      | _ ->
	  match u with
	    | KeyUnicode ch->let c=(UTF8.init 1 (fun i->ch)) in
		self#insert_char c;cur_pos<-cur_pos + 1;
	    | _ ->()


end;;




(* FIXME : go in poccore *)
(** graphic text object *)
class graphic_text nid fnt_t (col:color)=
object(self)
  inherit graphic_cached_object nid
(*  val mutable graphic=new graphic_cached_object nid *)
  val mutable graphic=new graphic_object_text fnt_t (["text:empty"]) col;
  val mutable fnt=(font_vault#get_cache_simple (get_font_id fnt_t))
 
  val mutable color=col
  method get_color=color
  method set_color c=color<-c

  val mutable lines=1
  method set_lines l=lines<-l
  method get_lines=lines


  val mutable max_size=100
  method set_max_size s=max_size<-s
  method get_max_size=max_size

  val mutable text=[""]
  method get_text=text


  method lines_size (l:int)=
    let si=ref 0 in
    let i=ref 0 in
    List.iter (
      fun s->
	let utf=new utf8 in
	  utf#set s;
	if !i<l then
	  si:= !si+utf#length;
	i:= !i+1;
    ) text;
      !si

  method line_of_pos (p:int)=
    let si=ref 0 in
    let l=ref 0 in
    let i=ref 0 in
    List.iter (
      fun s->
	let utf=new utf8 in
	  utf#set s;
	  if p> !si && p<= !si+utf#length then
	    l:= !i;
	  
	  si:= !si+utf#length;
	  i:= !i+1;
    ) text;
      !l



  (* NEW cut_string *)
  method private cut_string2 is=
    let nlist=(split_delim (regexp "[\n]+") is) in

    let a=DynArray.create() in      
    let cl=ref 0 in
      List.iter (
	fun s->
	  let utf=new utf8 in
	    utf#set s;
	    let cp=ref 0 and
		lp=ref 0 and
		cs=ref 0 and

		str=ref "" in
	      
	      
	      while !cl<lines && !cp<utf#length do
		while !cs<max_size && !cp<utf#length do
		  (
		    str:= (utf#sub !lp (!cp- !lp));
		    let (cw,ch)=fnt#sizeof_text (!str) in 
		      cs:=cw;
		      cp:= !cp+1;
		  )
		done;
		str:= (utf#sub !lp (!cp- !lp));
		DynArray.add a !str;
		cl:= !cl+1;
		lp:= !cp;
		cs:=0;
	      done;

      ) nlist;

      DynArray.to_list a 
	
  (* OLD cut_string *)	  
  method private cut_string s=
    (*    text<-split_delim (regexp "[\n]+") s; *)
    let a=DynArray.create() in
    let ss=UTF8.length s/max_size in
      for i=0 to ss do

	let md=UTF8.length s - (i*max_size) in
	let l=if md>=max_size then max_size else md in
	let cs=UTF8.nth s (i*max_size) and
	    ce=UTF8.nth s ((i*max_size) + l) in
	    let ns=String.sub s cs (ce-cs) in
	      if String.length ns>0 then
		DynArray.add a ns
      done;
      DynArray.to_list a
	

  method set_text t=

    if t="" then (
      text<-[""];
      graphic<-new graphic_cached_object "text:empty";
    )
    else (
      text<-self#cut_string2 t;
      graphic<-new graphic_object_text fnt_t (self#get_text) self#get_color;
    );

    print_string "taille: ";
    print_int (List.length (self#get_text));
    print_newline();
    

(*      new graphic_dyn_object (id^"/text") (List.length self#get_text)
      (function k-> (
	 fnt#create_text (List.nth self#get_text k) self#get_color
       ));
*)
    let cw=ref 0 and
      ch=ref 0 in
      for i=0 to (List.length (self#get_text))-1 do
	let pos=fnt#sizeof_text (List.nth (self#get_text) i) in
	  if !cw<(fst pos) then
	    cw:=(fst pos);
	  ch:=!ch + (snd pos);
      done;
      rect#set_size (!cw) (!ch);
      

  method put()=
    for i=0 to (List.length self#get_text)-1 do
      graphic#move (rect#get_x) (rect#get_y+(i*fnt#get_height));
      graphic#set_cur_drawing i;
      graphic#put();	
      
    done;

end;;

(** text box *)
class iface_text_box rid bpgraph fnt_t color bw il=
object(self)
    inherit iface_object bw 0 as super

    val mutable bg=new iface_pgraphic_object bpgraph
    val mutable text=new graphic_text (rid^"/text_box") fnt_t color
    val mutable fnt=(font_vault#get_cache_simple (get_font_id fnt_t))

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
    val mutable cursor=new graphic_cached_object "cursor"

    initializer
      cursor<-new graphic_from_drawing "cursor" (
	fun()->
	  let dr=drawing_vault#new_drawing() in
	    dr#exec_op_create "rect" 
	      [
		DrawValSize(1,(fnt#get_height));
		DrawValColor color
	    ];
	    [|dr|]
      )   
	

    method private get_textedit=te

    method get_data_text=te#get_text;

    method set_data_text t=
      super#set_data_text (t);       
      if t="" then
	te#set_text t;

    method on_keypress (k,utfk)=
      te#parse k utfk;

(*      (match (parse_key e.ebut) with
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



