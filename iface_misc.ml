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


open Core_rect;;
open Core_medias;;

open Core_event;;

open Iface_object;;
open Iface_text;;
open Iface_button;;

(** Interface misc objects (some experiment)*)

(*
(** dialog object *)
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
*)
