

open Iface_object;;
open Iface_button;;
open Iface_properties;;
open Iface_container;;


class iface_vscrollbar_container c bfg bbg topgr bottomgr=
object(self)
  inherit iface_object_vcontainer c as super

  val mutable fggr=new iface_pgraphic_object bfg
  val mutable bggr=new iface_pgraphic_object bbg
  val mutable topbut=new iface_graphic_button topgr topgr#get_rect#get_w topgr#get_rect#get_h
  val mutable bottombut=new iface_graphic_button bottomgr bottomgr#get_rect#get_w bottomgr#get_rect#get_h

  val mutable vy=0
  val mutable max_obj=5
  method set_max_obj m=max_obj<-m

    method hide()=
      super#hide();
      fggr#hide();
      bggr#hide();
      topbut#hide();
      bottombut#hide();      

    method show()=
      self#hide();
      showing<-true;
      let cobj=ref 0 in
	self#foreach (let f obj=
			if !cobj>=vy && !cobj<vy+max_obj then (
			  obj#show() 
			);
			cobj:= !cobj+1;
		      in f);

	if max_obj<self#objs_count then (
	  fggr#show();
	  bggr#show();
	  topbut#show();
	  bottombut#show();
	)
    method put()=
(*      super#put(); *)
      let cobj=ref 0 in
	self#foreach (fun obj->
			if !cobj>=vy && !cobj<vy+max_obj then (
			  if obj#get_embed then
			    obj#put()
			);
			cobj:= !cobj+1;
		     );

	bggr#put();
	topbut#put();
	bottombut#put();
	fggr#put();

    method reset_size()=
      let w=ref 0 in
      let h=ref 0 in
      let (mw,mh)=self#max_size() in	
      let cobj=ref 0 in
      self#foreach (
	let f obj=
	  if !cobj<max_obj then (
	    
	    cobj:=!cobj+1;
	    h:=!h+(if symmetric_size then mh else obj#get_rect#get_h);
	    if obj#get_rect#get_w> !w then
	      w:=obj#get_rect#get_w

	  )
	in f
      );
	rect#set_size !w !h;
	
	let vw=ref 0 in
	let vh=ref 0 in	
	let (vmw,vmh)=self#vmax_size() in	
	let cobj=ref 0 in
	self#foreach (
	  let f obj=
	    if !cobj<max_obj then (

	      cobj:=!cobj+1;
	      vh:=!vh+(if symmetric_size then vmh else obj#get_vrect#get_h);
	      if obj#get_vrect#get_w> !vw then
		vw:=obj#get_vrect#get_w
	    )
	  in f
	);
	vrect#set_size !vw !vh;		   

	let (cbw,cbh)=bggr#border_size in 
	  bggr#resize (16) (rect#get_h);

	  if self#objs_count<>0 then
	    fggr#resize (16) ((max_obj*(rect#get_h-26))/self#objs_count)
          else 
  	    fggr#resize (16) (16);

	  rect#set_size (rect#get_w+bggr#get_rect#get_w+16) (rect#get_h);
	  vrect#set_size (vrect#get_w+bggr#get_rect#get_w+16) (vrect#get_h)


  method move x y=
    super#move x y;
    let (mw,mh)=self#max_size() in
    let h=ref 0 in
      
      self#foreachi (
	fun i obj->	  
	  if i>=vy && i<vy+max_obj then (
	  let (ax,ay)=self#pos_from_align obj#get_rect in
	    obj#move 
	      (x+ax) 
	      (y+( (if symmetric_size then (mh*(i-vy))+ay else (!h))));
	    h:= !h+obj#get_rect#get_h;
	  );
      );

      if self#objs_count<>0 then
	fggr#move (x+(rect#get_w-bggr#get_rect#get_w)) (y+13+(vy*(rect#get_h-26)/self#objs_count))
      else
	fggr#move (x+(rect#get_w-bggr#get_rect#get_w)) (y+13);

    bggr#move (x+(rect#get_w-bggr#get_rect#get_w)) y;
    topbut#move (x+(rect#get_w-bggr#get_rect#get_w)+2) y;
    bottombut#move (x+(rect#get_w-bggr#get_rect#get_w)+2) (y+rect#get_h - bottombut#get_rect#get_h);


    method on_click x y=
      
      let cobj=ref 0 in
      self#foreach (
	fun obj->
	  if !cobj>=vy && !cobj<vy+max_obj then (
	    if obj#get_embed then (
	      if obj#get_vrect#is_position x y then (
		
		obj#on_click x y;
	      ) 
	    )
	  );
	  cobj:= !cobj+1;
      );

	if topbut#get_vrect#is_position x y then (
	  topbut#on_click x y;
	  if vy>0 then vy<-vy-1;
	);
	
	if bottombut#get_vrect#is_position x y then (
	  bottombut#on_click x y;
	  if vy<(self#objs_count-max_obj) then vy<-vy+1;
	);

(* reinit *)
	self#move rect#get_x rect#get_y;
	self#show();

	super#on_click x y;    

    method on_release x y=
	if topbut#get_vrect#is_position x y then (
	  topbut#on_release x y;
	);
	
	if bottombut#get_vrect#is_position x y then (
	  bottombut#on_release x y;
	);

	super#on_release x y;    


end;;
