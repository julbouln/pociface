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


open Core_video;;
open Core_event;;
open Core_medias;;

open Interface;;

(** Interface event parser *)

let ev_iface_parser a iface curs=
  (match a with
     | EventMouse em ->
	 (match em with
	    | MouseMotion(x,y) -> 
		iface#mouseover x y;
		curs#move x y;
	    | MouseRelease(x,y,but) -> 
		iface#release x y; 
		curs#set_state "normal";
	    | MouseClick(x,y,but) -> 
		iface#click x y; 
		curs#set_state "clicked";
	    | _ -> ()
	 )
     | EventKeyboard ek->
	 (match ek with
	    | KeyboardPress (k,uk)-> 
		iface#keypress (k,uk)
	    | _ -> ()
	 )
     | _ -> ()
  )



