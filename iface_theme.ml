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

open Value_xml;;
open Value_xmlparser;;

open Core_medias;;
open Core_graphic;;
open Core_font;;
open Core_drawing;;

open Binding;;

open Iface_object;;
open Iface_properties;;

(** Interface theming *)

(** xml iface style parser *)
class xml_iface_style_parser=
object(self)
  inherit xml_parser
  val mutable nm=""
  val mutable props=new iface_properties

  method get_val=(nm,props)

  method parse_attr k v=
    match k with
      | "type" ->nm<-v
      | _ -> ()
   
  method parse_child k v=
    match k with
      | "properties" -> let p=(new xml_iface_props_parser) in p#parse v;props<-p#get_val
      | _ -> ()

end;;

(** default pattern *)
let default_pattern bgcol=
  let dr=drawing_vault#new_drawing() in
    dr#create 24 24 bgcol;
  let (r,g,b)=bgcol in
  let lcol=(r+16,g+16,b+16) and
      dcol=(r-16,g-16,b-16) in
    dr#exec_op_write_from_list "line" [
	`Position (0,0);
	`Position (23,0);
	`Color lcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (1,1);
	`Position (22,1);
	`Color lcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (0,0);
	`Position (0,23);
	`Color lcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (1,1);
	`Position (1,22);
	`Color lcol;
      ];

    dr#exec_op_write_from_list "line" [
	`Position (23,0);
	`Position (23,23);
	`Color dcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (22,1);
	`Position (22,22);
	`Color dcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (0,23);
	`Position (23,23);
	`Color dcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (1,22);
	`Position (22,22);
	`Color dcol;
      ];
    dr;;
(*  let bg=(tile_box 24 24 bgcol) in
  let (r,g,b)=bgcol in
  let lcol=(r+16,g+16,b+16) and
      dcol=(r-16,g-16,b-16) in

    tile_line bg (0,0) (23,0) lcol;
    tile_line bg (1,1) (22,1) lcol;
    tile_line bg (0,0) (0,23) lcol;
    tile_line bg (1,1) (1,22) lcol;
    tile_line bg (23,0) (23,23) dcol;
    tile_line bg (22,1) (22,22) dcol;
    tile_line bg (0,23) (23,23) dcol;
    tile_line bg (1,22) (22,22) dcol;
    bg;;
*)

let default_pattern_clicked bgcol=
  let dr=new default_drawing_object in
    dr#create 24 24 bgcol;
  let (r,g,b)=bgcol in
  let lcol=(r+16,g+16,b+16) and
      dcol=(r-16,g-16,b-16) in
    dr#exec_op_write_from_list "line" [
	`Position (0,0);
	`Position (23,0);
	`Color dcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (1,1);
	`Position (22,1);
	`Color dcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (0,0);
	`Position (0,23);
	`Color dcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (1,1);
	`Position (1,22);
	`Color dcol;
      ];

    dr#exec_op_write_from_list "line" [
	`Position (23,0);
	`Position (23,23);
	`Color lcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (22,1);
	`Position (22,22);
	`Color lcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (0,23);
	`Position (23,23);
	`Color lcol;
      ];
    dr#exec_op_write_from_list "line" [
	`Position (1,22);
	`Position (22,22);
	`Color lcol;
      ];
    dr;;
(*
  let bg=(tile_box 24 24 bgcol) in
  let (r,g,b)=bgcol in
  let lcol=(r+16,g+16,b+16) and
      dcol=(r-16,g-16,b-16) in
    tile_line bg (0,0) (23,0) dcol;
    tile_line bg (1,1) (22,1) dcol;
    tile_line bg (0,0) (0,23) dcol;
    tile_line bg (1,1) (1,22) dcol;
    tile_line bg (23,0) (23,23) lcol;
    tile_line bg (22,1) (22,22) lcol;
    tile_line bg (0,23) (23,23) lcol;
    tile_line bg (1,22) (22,22) lcol;
    bg;;
*)
(*
let default_graph w h bgcol bordcol=
  let bg=(tile_box w h bgcol) in
  let fg=(tile_rect w h bordcol) in
    tile_set_alpha fg 255 255 255;
    tile_put_to fg bg 0 0;
    tile_free fg;
    bg;;
*)
let default_graph w h color=
  new graphic_from_drawing "default_rect" (
    fun()->
      let dr=drawing_vault#new_drawing() in
	dr#exec_op_create_from_list "rect" 
	  [
	    `Size(w,h);
	    `Color color
	  ];
	[|dr|]
  )   


(** default style *)
let get_default_style n=

  let props=new iface_properties in
    (match n with
       | "iface_text_edit" ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->new graphic_pattern "default_pattern_text:simple"));
	     ("font",IPropFont (FontEmbed));
	     ("foreground_color",IPropColor (0,0,0))
	   ]
       | "iface_text_edit_box" ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->new graphic_pattern "default_pattern_text:simple"));
	     ("font",IPropFont (FontEmbed));
	     ("foreground_color",IPropColor (0,0,0))
	   ]
       | "iface_hcontainer" ->props#from_list
	   [
	     ("valign",IPropAlign IAlignMiddle);
	     ("halign",IPropAlign IAlignMiddle);
	     ("fixed_size",IPropBool false);
	     ("symmetric_size",IPropBool false) 
	   ]
       | "iface_vcontainer" ->props#from_list
	   [
	     ("valign",IPropAlign IAlignMiddle);
	     ("halign",IPropAlign IAlignMiddle);
	     ("fixed_size",IPropBool false); 
	     ("symmetric_size",IPropBool false) 
	   ]
     
       | "iface_menu" ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->(new graphic_pattern "default_pattern:simple")));
	   ]
       | "iface_menubar" ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->(new graphic_pattern "default_pattern:simple")));
	   ]
       | "iface_window" ->props#from_list
	   [
	     ("pattern_background",IPropPattern  (fun()->new graphic_pattern "default_pattern:simple"));
	     ("pattern_title",IPropPattern  (fun()->new graphic_pattern "default_pattern_clicked:simple"));
	     ("pattern_title_min",IPropPattern  (fun()->new graphic_pattern "default_pattern_clicked:simple"));
	     ("font",IPropFont (FontEmbed));
	     ("foreground_color",IPropColor (0,0,0));
	     ("close_button",IPropGraphic  (fun()->default_graph 16 16 (0,0,0)));
	     ("minimize_button",IPropGraphic  (fun()->default_graph 16 16 (0,0,0)));
	     ("maximize_button",IPropGraphic  (fun()->default_graph 16 16 (0,0,0)));

	   ];
       | _ ->props#from_list
	   [
	     ("pattern",IPropPattern  (fun()->new graphic_pattern "default_pattern:simple"));
	     ("pattern_normal",IPropPattern  (fun()->new graphic_pattern "default_pattern:simple"));
	     ("pattern_clicked",IPropPattern  (fun()->new graphic_pattern "default_pattern_clicked:simple"));
	     ("font",IPropFont (FontEmbed));
	     ("foreground_color",IPropColor (0,0,0))
	   ];
    );
    props;;
      

exception Iface_style_not_found of string;;

(** iface theme *)
class iface_theme (hs:(string,iface_properties) Hashtbl.t)=
object
  val mutable styles=hs

  method get_style n=
    (try
       Hashtbl.find styles n 
     with Not_found -> get_default_style n)
(*raise (Iface_style_not_found n))*)


end;;


