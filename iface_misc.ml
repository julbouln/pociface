open Low;;

open Video;;
open Rect;;
open Medias;;

open Event_manager;;

open Iface_object;;
open Iface_text;;
open Iface_button;;


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

