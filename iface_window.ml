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

open Rect;;

open Iface_object;;
open Iface_button;;
open Iface_text;;
open Iface_container;;

(** Interface windows *)

(** generic window object *)
class iface_window (rid:string) (tipa) (tipa_min) (bgpa) fnt tcol (clgr) (mingr) (maxgr) ttxt (c:iface_object)=
object(self)
  inherit iface_object 0 0 as super
  val mutable content=c

  val mutable tilab=new iface_label_static fnt tcol ttxt 
  val mutable ticl=new iface_graphic_button clgr clgr#get_rect#get_w clgr#get_rect#get_h
  val mutable timin=new iface_graphic_button mingr mingr#get_rect#get_w mingr#get_rect#get_h
  val mutable timax=new iface_graphic_button maxgr maxgr#get_rect#get_w maxgr#get_rect#get_h

  val mutable tigr=new iface_pgraphic_object tipa
  val mutable tigr_min=new iface_pgraphic_object tipa_min

  val mutable bggr=new iface_pgraphic_object bgpa

  val mutable minimized=false

  initializer
    self#init_size();

  method set_layer l=
    layer<-l;
    content#set_layer (layer+1);

(*
  method resize w h=
*)  

  method private init_size()=    


    let (cbw,cbh)=bggr#border_size in 
    let (tbw,tbh)=tigr#border_size in
    let titot_w=(tilab#get_rect#get_w+ticl#get_rect#get_w+timin#get_rect#get_w+timax#get_rect#get_w) in
      if content#get_rect#get_w>titot_w then (
	bggr#resize (content#get_rect#get_w+(cbw*2)) (content#get_rect#get_h+(cbh*2));
	tigr#resize (content#get_rect#get_w+(tbw*2)) (tilab#get_rect#get_h+(tbh*2));
	tigr_min#resize (content#get_rect#get_w+(tbw*2)) (tilab#get_rect#get_h+(tbh*2));
      )
      else
	(
	  bggr#resize (titot_w+(cbw*2)) (content#get_rect#get_h+(cbh*2));
	  tigr#resize (titot_w+(tbw*2)) (tilab#get_rect#get_h+(tbh*2));
	  tigr_min#resize (titot_w+(tbw*2)) (tilab#get_rect#get_h+(tbh*2));
	  content#resize (titot_w) (content#get_rect#get_h);
	);

      rect#set_size (bggr#get_rect#get_w) (bggr#get_rect#get_h+tigr#get_rect#get_h)


  method move x y=
    super#move x y;
    
    (* title part *)
    tigr#move x y;
    tigr_min#move x y;

    let (tbw,tbh)=tigr#border_size in      
      tilab#move (x+tbw) (y+tbh);
      ticl#move (x+tigr#get_rect#get_w-ticl#get_rect#get_w-tbw) (y+tbh);
      timax#move (x+tigr#get_rect#get_w-timax#get_rect#get_w-tbw-ticl#get_rect#get_w) (y+tbh);
      timin#move (x+tigr#get_rect#get_w-timin#get_rect#get_w-tbw-ticl#get_rect#get_w-timax#get_rect#get_w) (y+tbh);


    (* content part *)
    let (cbw,cbh)=bggr#border_size in      
      bggr#move x (y+tigr#get_rect#get_h-cbh);
      content#move (x+cbw) (y+tigr#get_rect#get_h);



  method put()=
    if minimized=false then
      (    
	bggr#put();
        tigr#put();
      )
    else
      tigr_min#put();

    tilab#put();
    ticl#put();
    timin#put();
    timax#put();

  method show()=
    super#show();

    tigr#show();
    tigr_min#show();

    tilab#show();
    ticl#show();
    timin#show();
    timax#show();    

    bggr#show();
    content#show();
    
  method hide()=
    super#hide();

    tigr#hide();
    tigr_min#hide();

    tilab#hide();
    ticl#hide();
    timax#hide();
    timin#hide();

    bggr#hide();
    content#hide();

(* drag *)
  val mutable clicked=false
  val mutable crect=new rectangle 0 0 0 0
   
  method on_click x y=
    if tigr#get_vrect#is_position x y then (
      crect#set_position (x-rect#get_x) (y-rect#get_y);
      self#set_layer (layer+2);
      clicked<-true;
    );

    if ticl#get_vrect#is_position x y then (
      ticl#on_click x y;
      clicked<-false;
    );

    if timin#get_vrect#is_position x y then (
      timin#on_click x y;
      minimized<-true;
      content#hide();
      clicked<-false;
    );

    if timax#get_vrect#is_position x y then (
      timax#on_click x y;
      minimized<-false;
      content#show();
      clicked<-false;
    );


  method on_release x y=
    if ticl#get_vrect#is_position x y then (
      self#hide();
      ticl#on_release x y
    );

    if timin#get_vrect#is_position x y then (
      timin#on_release x y
    );

    if timax#get_vrect#is_position x y then (
      timax#on_release x y
    );
    
    if clicked then
      (
	self#set_layer (layer-2); 
	clicked<-false;
      )
  method private drag x y=
    if clicked then (	
	self#move (x-crect#get_x) (y-crect#get_y);
    );

  method on_mouseover x y=
    self#drag x y;

  method on_mouseout x y=
    self#drag x y;

end;;

