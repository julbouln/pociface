open Core_event;;

open Core_video;;
open Core_medias;;
open Core_cursor;;

open Core_main;;
open Core_stage;;

open Interface;;
open Iface_event;;
open Iface_xml;;

open Iface_object;;
open Iface_button;;


(*
<main>
 <info>
  <cmd str="iface_parser"/>
  <name str="Interface parser"/>
  <version str="0.1"/>  
 <info>
 <screen_size w="800" h="600"/>
 <screen_default_size w="1024" h="768"/>
<!-- <cursor path="medias/misc/cursor.png"/>  -->

</main>

*)

let rec usleep sec = ignore (Unix.select [] [] [] sec);;

main#info#set_cmd "iface_parser";
main#info#set_name "Interface Parser";
main#info#set_version "0.1";

main#set_scr_w 800;
main#set_scr_h 600;
main#parse_args();
main#medias_init();;

main#set_def_size 800 600;;

let curs=new cursors 30 30 None;;

let stages=new stages curs;;

class iface_parser=
object (self)
  inherit iface_stage curs Sys.argv.(1) as super

  method on_loop()=
    video#blank();
    super#on_loop();

(*    usleep(1./.30.); *)

  method ev_parser e=
    super#ev_parser e;
    (match e with
       | EventKeyboard ek ->
	   (match ek with
	      | KeyboardPress (k,uk)-> 
		  (match k with 
		     | KeyEchap -> 
			 exit 2;
		     | KeyReturn ->
			 ()
		     | _ ->()
		  )
	      | _ -> ()
	   )
       | _ -> ()
    )

end;;


stages#stage_add "parser" (new iface_parser:>stage);
stages#stage_load "parser"; 
