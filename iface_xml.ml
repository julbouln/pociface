open Video;;

open Oxml;;
open Medias;;

open Iface_object;;
open Iface_text;;
open Iface_button;;

open Interface;;



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

