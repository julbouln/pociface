open Rect;;

open Iface_object;;

(** container widget *)
class iface_container c=
  object (self)
    inherit iface_object 0 0 as super
    val mutable content=c

(*
    method init_content()=
      self#foreach (fun obj->
		      obj#set_parent (self:>iface_object)
		   )

    initializer
      self#init_content();
*)
    method private foreach f=
      Array.iter f content;
      
    method private reset_size()=()

    method get_rect=
      self#reset_size();
      rect
      
    method private foreachi f=
      Array.iteri f content;

    method show()=
      super#show();
      self#foreach (let f obj=obj#show() in f)

    method hide()=
      super#hide();
      self#foreach (let f obj=obj#hide() in f)

    method put()=
      super#put();
      self#foreach (let f obj=obj#put() in f)

  end;;


(** vertical container widget *)
class iface_vcontainer c=
  object (self)
    inherit iface_container c as super

    val mutable vrect=new rectangle 0 0 0 0
    method get_vrect=vrect

    method private reset_size()=
      let w=ref 0 in
      let h=ref 0 in
      self#foreach (
      let f obj=
	h:=!h+obj#get_rect#get_h;
	if obj#get_rect#get_w> !w then
	  w:=obj#get_rect#get_w
      in f
      );
	rect#set_size !w !h;

      let vw=ref 0 in
      let vh=ref 0 in	
      self#foreach (
      let f obj=
	vh:=!vh+obj#get_vrect#get_h;
	if obj#get_vrect#get_w> !vw then
	  vw:=obj#get_vrect#get_w
      in f
     );
	vrect#set_size !vw !vh;
	
    method move x y=
      super#move x y;
      self#foreachi (
	fun i obj->
	  obj#move x (y+ (obj#get_rect#get_h*i))
      )
  end;;

(** vertical container widget *)
class iface_hcontainer c=
  object (self)
    inherit iface_container c as super


    val mutable vrect=new rectangle 0 0 0 0
    method get_vrect=vrect

    method private reset_size()=
      let w=ref 0 in
      let h=ref 0 in
      self#foreach (
      let f obj=
	w:=!w+obj#get_rect#get_w;
	if obj#get_rect#get_h> !h then
	  h:=obj#get_rect#get_h
      in f
     );
      rect#set_size !w !h;
	

      let vw=ref 0 in
      let vh=ref 0 in	
      self#foreach (
      let f obj=
	vw:=!vw+obj#get_vrect#get_w;
	if obj#get_vrect#get_h> !vh then
	  vh:=obj#get_vrect#get_h
      in f
     );
	vrect#set_size !vw !vh;

    method move x y=
      super#move x y;
      self#foreachi (
      fun i obj->
	obj#move (x+(obj#get_rect#get_w*i)) y
     )
  end;;

