open Low;;

open Rect;;
open Medias;;

open Event_manager;;

(** parent widget *)
class iface_object w h=
object(self)
    val mutable id="none"
    method set_id i=id<-i
    method get_id=id

(*
    val mutable parent=new iface_object w h
    method set_parent p=parent<-p
    method get_parent=parent
*)

    val mutable data=0
    val mutable data1=0
    val mutable data_text=""
    val mutable showing=false

    val mutable rect=new rectangle 0 0 w h
      

    val mutable click=(function()->())
    val mutable release=(function()->())
    val mutable mouseover=(function()->())
    val mutable mouseout=(function()->())

    val mutable focused=false

    method set_focused f=focused<-f


    method on_keypress (e:event)=()
    method on_keyrelease (e:event)=()
	
    method on_click (x : int) (y : int)=click()
    method on_release (x : int) (y : int)=release()
    method on_mouseover (x : int) (y : int)=mouseover()
    method on_mouseout (x : int) (y : int)=mouseout()


    method append_click c=
      let oclick=click in
      let nclick()=oclick();c() in
	click<- nclick;

    method prepend_click (c:unit->unit)=
      let oclick=click in
      let nclick()=c();oclick() in
	click<- nclick;

    method set_click c=click<-c
    method set_release r=release<-r

    method get_click=click
    method get_release=release

    method set_mouseover c=mouseover<-c
    method set_mouseout c=mouseout<-c
      
    method is_showing=showing
    method show()=showing<-true
    method hide()=showing<-false
      
    method move x y=rect#set_position x y

    method get_rect=rect
    method get_vrect=self#get_rect

    method put()=()

    method set_data d=data<-d
    method get_data=data
    method set_data1 d=data1<-d
    method get_data1=data1
    method set_data_text d=data_text<-d
    method get_data_text=data_text
	  
  end;;

(** graphic object widget *)
class iface_graphic_object gr w h=
  object (self)
    inherit iface_object (gr#get_rect#get_w) (gr#get_rect#get_h) as super
    val mutable graphic=gr
    method move x y=
      super#move x y;
      graphic#move x y
    method put()=
      if showing==true then
	graphic#put()
    method get_rect=graphic#get_rect
  end;;



(** graphic object from file widget *)
class iface_graphic_file_object file w h=
  object (self)
    inherit iface_graphic_object (new graphic_scr_resized_object w h file false false) w h as super

  end;;

(** graphic object from file widget with var color *)
class iface_graphic_colored_object file w h un uc=
  object (self)
    inherit iface_graphic_object (new graphic_object_colored w h file false false un uc) w h as super

  end;;


(** special graphic resize with 9 tiles *)
class iface_rgraphic_object file iw ih=
object(self)
  inherit iface_object iw ih as super

  val mutable gen=new graphic_simple_from_func (file^"/gen") (fun()->tile_load file)
  val mutable gr=new graphic_generic_object file

  val mutable crect=new rectangle 0 0 0 0

  val mutable w=iw
  val mutable h=ih

  method resize nw nh=
    w<-nw;h<-nh;
    self#init();

  method private init()=
    crect#set_size (gen#get_rect#get_w/3) (gen#get_rect#get_h/3);
    gr<-new graphic_object (crect#get_w) (crect#get_h) file false false;
    
  initializer
    self#init()
(*

036
147
258

*)
  method move x y=
    super#move x y;
    gr#move x y;

  method put()=
    if self#is_showing then (
      let cw=w/crect#get_w and
	  ch=h/crect#get_h in    
	for i=0 to cw do
	  for j=0 to ch do
	    (match (i,j) with
	       | (0,0) -> gr#set_cur_tile 0
	       | (0,ih) when ih=ch ->gr#set_cur_tile 2
	       | (0,_) ->gr#set_cur_tile 1
	       | (iw,0) when iw=cw -> gr#set_cur_tile 6
	       | (_,0) ->gr#set_cur_tile 3
	       | (iw,ih) when iw=cw && ih=ch -> gr#set_cur_tile 8
	       | (_,ih) when ih=ch ->gr#set_cur_tile 5
	       | (iw,_) when iw=cw ->gr#set_cur_tile 7
	       | (_,_) ->gr#set_cur_tile 4
	    );
	    gr#move (rect#get_x + (i*crect#get_w)) (rect#get_y + (j*crect#get_h));
	    gr#put();
	  done
	done
    )
end;; 


