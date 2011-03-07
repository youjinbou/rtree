(*
  a rtree library

  Copyright (C) 2010  Didier Cassirame

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

*)
open Sdl
open Sdlvideo
open Sdlevent
open Sdlkey
open Sdlmouse
open Sdlgl
open Sdlwm
open GlPix

open Common

(* always useful *)
let pi = acos (-.1.)

class box3d b t =
object(self)

  inherit Rb.c b t as super

  method draw () =
    let xb, yb, zb = FVec3.to_tuple self#bottom
    and xt, yt, zt = FVec3.to_tuple self#top
    in
      GlDraw.begins `quad_strip;
      GlDraw.vertex ~x:xt ~y:yt ~z:zb ();
      GlDraw.vertex ~x:xt ~y:yb ~z:zb ();
      GlDraw.vertex ~x:xb ~y:yt ~z:zb ();
      GlDraw.vertex ~x:xb ~y:yb ~z:zb ();
      GlDraw.vertex ~x:xb ~y:yt ~z:zt ();
      GlDraw.vertex ~x:xb ~y:yb ~z:zt ();
      GlDraw.vertex ~x:xt ~y:yt ~z:zt ();
      GlDraw.vertex ~x:xt ~y:yb ~z:zt ();
      GlDraw.ends ();
      GlDraw.begins `quad_strip;
      GlDraw.vertex ~x:xb ~y:yt ~z:zb ();
      GlDraw.vertex ~x:xb ~y:yt ~z:zt ();
      GlDraw.vertex ~x:xt ~y:yt ~z:zb ();
      GlDraw.vertex ~x:xt ~y:yt ~z:zt ();
      GlDraw.vertex ~x:xt ~y:yb ~z:zb ();
      GlDraw.vertex ~x:xt ~y:yb ~z:zt ();
      GlDraw.vertex ~x:xb ~y:yb ~z:zb ();
      GlDraw.vertex ~x:xb ~y:yb ~z:zt ();
      GlDraw.ends ()

end  

class box3d_gllist b t =
object(self)

  inherit box3d b t as super

  val mutable gl_id = Obj.magic 0

  method private init_id =
    let l = GlList.create `compile 
    in
      GlList.begins l ~mode:`compile;
      super#draw ();
      GlList.ends ();
      gl_id <- l

  method draw () =
    GlList.call gl_id
      
  initializer (
    self#init_id
  )

end

module Rtree3Def : Rtree.Rtreedef.T with type t = box3d =
struct 
  let minimum = 8
  let maximum = 16
  type t = box3d
end

module Rtree3 = Rtree.Make(Rtree.Lsplit.Make)(Rbox3d.Make)(FVec3)(Rtree3Def)

module Glv =
struct

  module Mat =
  struct
    let translate v =
      GlMat.translate ~x:(FVec3.get v 0) ~y:(FVec3.get v 1) ~z:(FVec3.get v 2) ()

    let rotate a v =
      GlMat.rotate ~angle:a ~x:(FVec3.get v 0) ~y:(FVec3.get v 1) ~z:(FVec3.get v 2) ()
  end

  module Draw =
  struct
    let vertex v =
      GlDraw.vertex ~x:(FVec3.get v 0) ~y:(FVec3.get v 1) ~z:(FVec3.get v 2) ()
  end

end


(* ---------------------------------------------------------------------------------*)

class view screen = 
object (self)

  val width  = 800
  val height = 600

  val rtree = 
    let fname = "rtree" 
    and fdir  = "dots" in 
    let range = 100.0 in
    let maxv = [| range ; range ; range |] in
    let origb = [| -1.0; -1.0 ; -1.0 |]
    and origt = [| 1.0 ; 1.0 ; 1.0 |] in 
    let r = Rtree3.make (new Rb.c origb origt) (new box3d_gllist origb origt) 
(*
    let startb = [| -100.0; -10.0; 35.0 |]
    and startt = [| -98.0; -12.0; 37.0 |] in
    let r = Rtree3.make (new Rtree3.Rbox.t startb startt) (new box3d_gllist startb startt)
*)
    in
    let sfact = 3.0 in
    let rec addup i m =
(*      Rtree3.dump r (fname^"_"^(Printf.sprintf "%03d" i)) fdir;  *)
      if i < m 
      then 
	(*
	  let b = (FVec3.random maxv)
	  and t = (FVec3.random maxv)
	*)
	let b = FVec3.of_tuple ((float)(i / 100) *. sfact, (float)((i / 10) mod 10) *. sfact, (float)(i mod 10) *. sfact)
	in
	let t = FVec3.map (fun x -> x +. 1.0) b 
	in
	  Rtree3.insert r (new Rb.c b t) (new box3d_gllist b t);
	  addup (succ i) m
      else ()
    in
      addup 0 2000; 
      Rtree3.dump r fname fdir;
      r

  val mutable frustum_min = [| 0. ; 0. ; 0. |]
  val mutable frustum_max = [| 0. ; 0. ; 0. |]
  val mutable view_rot   =  [| 0.0 ; 0.0 ; 0.0 |]
  val mutable view_scale  = 1.
  val mutable old_xmcoord = 0
  val mutable old_ymcoord = 0
  val mutable pos         = [| 0. ; -5. ; -15. |]
  val mutable speed       = [| 0. ;  0. ; 0. |]

  val mutable texpos      = [| 0. ; 0.  ; 0. |]

  val mutable focale_     = 1.5
  val mutable depth_      = 60.


  val front = FVec3.of_tuple (0.0,0.0,1.0)
  val up    = FVec3.of_tuple (0.0,1.0,0.0)
  val right = FVec3.of_tuple (1.0,0.0,0.0)

  val mutable vdir = Array.make 3 (FVec3.null ())

  method position () = pos

  method vfront () = vdir.(2)
  method vup () = vdir.(1)
  method vright () = vdir.(0)

  val mutable keyboard_handler = fun a b -> ()
  val mutable mouse_handler    = fun a b c d -> ()
  val mutable motion_handler   = fun a b -> ()

  val modangle = fun x -> if x > 180. then (x -. 360.) else if (x < -180.) then (x +. 360.) else x

  method rotx  a = view_rot.(0) <- modangle (view_rot.(0) +. a)
  method roty  a = view_rot.(1) <- modangle (view_rot.(1) +. a)
  method rotz  a = view_rot.(2) <- modangle (view_rot.(2) +. a)

  val mutable lists = []

  val grid = 
    let grid_step = 10 in
    let l = GlList.create `compile 
    in
      GlList.begins l ~mode:`compile;
      List.iter Gl.enable [
	`blend; 
	`alpha_test; 
      ];
      List.iter Gl.disable [
	`depth_test;
	`texture_2d;
      ];
      GlDraw.polygon_mode `both `line;
      GlDraw.color (0.3,0.0,0.0);
      for j=(0-20) to 20 do 
	GlDraw.begins `quad_strip;
	for i=(0-20) to 20 do
	  let fi = float_of_int (i*grid_step) in
	    GlDraw.vertex ~x:(fi) ~z:(float_of_int (j*grid_step))      ~y:(0.) ();
	    GlDraw.vertex ~x:(fi) ~z:(float_of_int ((j+1)*grid_step))  ~y:(0.) ()
	done;
	GlDraw.ends ()
      done;
      GlDraw.color (1.,1.,1.);
      List.iter Gl.enable [
	`depth_test;
	`texture_2d;
      ];
      List.iter Gl.disable [
	`blend; 
	`alpha_test; 
      ];
      GlList.ends ();
      l
	
  method draw_grid () =
    GlList.call grid

  method draw_compass () =
    (new Compass.t (FVec3.opp pos) right up front vdir.(0) vdir.(1) vdir.(2))#draw ()

  method draw_node k =
    GlDraw.color (0.0,1.0,0.0);
    Gl.disable (`cull_face);
    (new box3d k#bottom k#top)#draw ();
    Gl.enable (`cull_face);

  method draw_box k b =
    GlDraw.color (0.0,0.0,1.0);
    b#draw ()

  val mutable visible = []


  (* moving bitfield *)
  val move_rotate    = 0x01
  val move_z         = 0x02
  val move_x         = 0x04
  val move_y         = 0x08
  val mutable moving = 0

  val mutable full_draw = false


  method init_visible () = 
    (* let vpos = FVec3.map2 (fun x y -> -. (x -. (3. *. y))) pos vdir.(2) in *)
    let vv = new View_volume.t vdir.(0) vdir.(1) vdir.(2) depth_ (FVec3.opp pos)
    in
      (*      vv#draw (); *)
      visible <- Rtree3.filter rtree vv#overlaps;
      Debug.int "visibles : " (List.length visible);
(*      List.iter (fun x -> Debug.int "id : " (Oo.id x)) visible *)

  method draw_tree () =
    if full_draw 
    then (
      Rtree3.iter rtree self#draw_node self#draw_box
    )
    else (
      if moving <> 0
      then ( 
	self#init_visible ()
      );
      let dummy = new Rb.c (FVec3.null ()) (FVec3.null ()) in
      List.iter (fun (k,v) -> self#draw_box dummy v) visible
    )

  method update_vdir () =
    let mat = GlMat.to_array (GlMat.get_matrix `modelview_matrix)
    in
      vdir.(0) <- FVec3.of_tuple (mat.(0).(0), mat.(1).(0), mat.(2).(0));
      vdir.(1) <- FVec3.of_tuple (mat.(0).(1), mat.(1).(1), mat.(2).(1));
      vdir.(2) <- FVec3.of_tuple (mat.(0).(2), mat.(1).(2), mat.(2).(2))

  method projection_setup () =
    (* projection setup *)
    GlMat.mode `projection;
    GlMat.load_identity();
    GlMat.frustum 
      ~x:( frustum_min.(0), frustum_max.(0) ) 
      ~y:( frustum_min.(1), frustum_max.(1) ) 
      ~z:( frustum_min.(2), frustum_max.(2) )

  method view_setup () =
    GlMat.mode `modelview;
    GlMat.load_identity();
    GlMat.rotate ~angle:view_rot.(0) ~x:1.0 ();
    GlMat.rotate ~angle:view_rot.(1) ~y:1.0 ();
    GlMat.rotate ~angle:view_rot.(2) ~z:1.0 ();
    self#update_vdir ();
    GlMat.load_identity();    
    GlMat.rotate ~angle:view_rot.(0) ~x:1.0 ();
    GlMat.rotate ~angle:view_rot.(1) ~y:1.0 ();
    GlMat.rotate ~angle:view_rot.(2) ~z:1.0 ();
    GlMat.translate ~x:pos.(0) ~y:pos.(1) ~z:pos.(2) ();
    GlMat.scale ~x:view_scale ~y:view_scale ~z:view_scale ();

  (* draw the whole thing *)
  method draw () =
    (* modelview setup *)
    GlClear.clear [`color;`depth];
    self#projection_setup ();
    self#view_setup ();
    self#draw_grid ();
    self#draw_compass ();
    self#draw_tree ()

  method display () =
    self#draw ();
    flush stderr;
    Sdlgl.swap_buffers ()
      
  method reshape () =
    GlDraw.viewport ~x:0 ~y:0 ~w:width ~h:height;
    GlMat.mode `projection;
    GlMat.load_identity ();
    let r = (float width) /. (float height) 
    and f = focale_
    in
    frustum_min.(0) <-   -.r ;        (* left view frustum    *)
    frustum_max.(0) <-     r ;        (* right view frustum   *)
    frustum_min.(1) <-   -.1.;        (* bottom view frustum  *)
    frustum_max.(1) <-     1.;        (* top view frustum     *)
    frustum_min.(2) <-     f ;        (* view depth           *)
    frustum_max.(2) <- depth_;        (* view depth           *)
    GlMat.frustum 
      ~x:( frustum_min.(0), frustum_max.(0) ) 
      ~y:( frustum_min.(1), frustum_max.(1) ) 
      ~z:( frustum_min.(2), frustum_max.(2) );
    GlMat.mode `modelview;
    GlMat.load_identity();
    Gl.enable `blend; 
    Gl.enable `texture_2d;
    Gl.enable `alpha_test;
    GlTex.env (`mode `decal);
    List.iter (GlTex.parameter ~target:`texture_2d)
      [ `wrap_s `repeat;
      	`wrap_t `repeat;
      	`mag_filter `nearest;
      	`min_filter `nearest ]; 
    GlClear.clear [`color;`depth]

  method motion_rot ~xmcoord ~ymcoord =
    self#rotx ((float ymcoord) -. (float old_ymcoord)); old_ymcoord <- ymcoord;
    self#roty ((float xmcoord) -. (float old_xmcoord)); old_xmcoord <- xmcoord

  method motion_scale ~xmcoord =
    let limit = 0.01 in
    view_scale <- (let x = view_scale +. (float (xmcoord - old_xmcoord)) /. 100.0 in if x < limit then limit else x);
    old_xmcoord <- xmcoord

  method motion_focale ~xmcoord =
    let limit = 1.0 in
    focale_ <- (let x = focale_ +. (float (xmcoord - old_xmcoord)) /. 1000.0 in if x < limit then limit else x);
    self#reshape ();
    old_xmcoord <- xmcoord

  method add_depth a =
    depth_ <- depth_ +. a;
    self#reshape ()

  method motion_off = ()
    
  method mouse ~btn ~state ~xmcoord ~ymcoord =
    match state with 
	RELEASED -> (
	  moving <- moving land (lnot move_rotate);
	  match btn with
	      BUTTON_LEFT  -> 
		self#set_motion (fun a b  -> self#motion_off );
	    | _            ->
		self#set_motion (fun a b  -> self#motion_off )
	)
      | PRESSED  -> (
	  moving <- moving lor move_rotate;
	  match btn with
	      BUTTON_LEFT  -> (
		old_xmcoord <- xmcoord;
		old_ymcoord <- ymcoord;
		self#set_motion (fun a b  -> self#motion_rot a b)
	      )
	    | BUTTON_RIGHT -> (
		old_xmcoord <- xmcoord;
		self#set_motion (fun a b  -> self#motion_scale a )
	      )
	    | BUTTON_MIDDLE -> (
		old_xmcoord <- xmcoord;
		self#set_motion (fun a b  -> self#motion_focale a )
	      )
	    | BUTTON_WHEELUP   -> self#add_depth 10.
	    | BUTTON_WHEELDOWN -> self#add_depth (-10.)
 	)

  method keyboard ~key ~state =
    let speed_fact = 0.1 in
    let move k fact flag =
      moving <- moving lor flag;
      speed.(k) <- speed.(k) +. fact
    and stop k fact flag =
      moving <- moving land (lnot flag);
      speed.(k) <- speed.(k) -. fact
    in
    match state with 
	RELEASED -> (
	  match key with
	    | KEY_RIGHT    | KEY_f       -> stop 0 (-.speed_fact) move_x
	    | KEY_LEFT     | KEY_s       -> stop 0 speed_fact move_x
	    | KEY_UP       | KEY_e       -> stop 2 speed_fact move_y
	    | KEY_DOWN     | KEY_d       -> stop 2 (-.speed_fact) move_y
	    | KEY_PAGEUP   | KEY_t       -> stop 1 (-.speed_fact) move_z
	    | KEY_PAGEDOWN | KEY_g       -> stop 1 speed_fact move_z
	    | KEY_q       -> full_draw <- false
	    | _           -> ()
	)
      | PRESSED ->
	  match key with
	      KEY_UNKNOWN -> ()
	    | KEY_ESCAPE  -> Sdl.quit ();exit 0
	    | KEY_r       -> self#reshape ();
	    | KEY_RIGHT    | KEY_f       -> move 0 (-.speed_fact) move_x
	    | KEY_LEFT     | KEY_s       -> move 0 speed_fact move_x
	    | KEY_UP       | KEY_e       -> move 2 speed_fact move_y
	    | KEY_DOWN     | KEY_d       -> move 2 (-.speed_fact) move_y
	    | KEY_PAGEUP   | KEY_t       -> move 1 (-.speed_fact) move_z
	    | KEY_PAGEDOWN | KEY_g       -> move 1 speed_fact move_z
	    | KEY_q       -> full_draw <- true
	    | KEY_SPACE   -> Debug.fvec "pos: " pos; Debug.fvec "rot: " view_rot;  Debug.fvec "frustum_min : " frustum_min; Debug.fvec "frustum_max : " frustum_max ; Debug.float "scale :" view_scale
	    | _           -> ()

  method motion x y = motion_handler x y 

  method check_events () =
    let rec check_events () =
    match poll () with
	None   -> ()
      | Some e -> ( 
	  match e with 
	      KEYDOWN           k 
	    | KEYUP             k -> self#keyboard ~state:k.ke_state ~key:k.keysym
	    | MOUSEBUTTONDOWN   m
	    | MOUSEBUTTONUP     m -> self#mouse ~btn:m.mbe_button ~state:m.mbe_state ~xmcoord:m.mbe_x ~ymcoord:m.mbe_y
	    | MOUSEMOTION       m -> self#motion m.mme_x m.mme_y
	    | _                   -> prerr_string "unhandled event";prerr_newline (); flush stderr
	); check_events ()
    in
      check_events ()

  method event_loop () =
    let timeframe = 0.01 in
    let usleep t = ignore (Unix.select [] [] [] t )
    in

    let t = Unix.gettimeofday () in
      self#display ();
      pos <- FVec3.add pos (FVec3.scale vdir.(0) speed.(0));
      pos <- FVec3.add pos (FVec3.scale vdir.(1) speed.(1));
      pos <- FVec3.add pos (FVec3.scale vdir.(2) speed.(2));
      self#check_events ();
      let t = timeframe -. (Unix.gettimeofday ()) -. t
      in if t > 0.0 then usleep t else ()

  method set_keyboard f = keyboard_handler <- f
  method set_mouse    f = mouse_handler    <- f
  method set_motion   f = motion_handler   <- f

  initializer (
    self#set_keyboard (fun a b     -> self#keyboard a b);
    self#set_mouse    (fun a b c d -> self#mouse a b c d);
    self#set_motion   (fun a b     -> self#motion_off );
    self#projection_setup ();
    self#view_setup ();
    self#init_visible ();
  )

end

let init_draw () =
  GlDraw.cull_face `back;
  GlDraw.front_face `ccw;
  GlTex.env (`mode `decal);
  List.iter Gl.enable 
    [
    `cull_face;
    ];
  GlDraw.shade_model `flat

let print_attr a = 
  let s, v = 
  match a with 
      RED_SIZE i         -> "RED_SIZE", string_of_int i
    | GREEN_SIZE i       -> "GREEN_SIZE", string_of_int i 
    | BLUE_SIZE i        -> "BLUE_SIZE", string_of_int i
    | ALPHA_SIZE i       -> "ALPHA_SIZE", string_of_int i
    | BUFFER_SIZE i      -> "BUFFER_SIZE", string_of_int i
    | DOUBLEBUFFER b     -> "DOUBLEBUFFER", string_of_bool b
    | DEPTH_SIZE i       -> "DEPTH_SIZE", string_of_int i
    | STENCIL_SIZE i     -> "STENCIL_SIZE", string_of_int i
    | ACCUM_RED_SIZE i   -> "ACCUM_RED_SIZE", string_of_int i
    | ACCUM_GREEN_SIZE i -> "ACCUM_GREEN_SIZE", string_of_int i
    | ACCUM_BLUE_SIZE i  -> "ACCUM_BLUE_SIZE", string_of_int i
    | ACCUM_ALPHA_SIZE i -> "ACCUM_ALPHA_SIZE", string_of_int i
    | STEREO i           -> "STEREO", string_of_int i
  in
    print_string s; print_string " "; print_string v; print_newline ()

let main () =
  begin
    Sdl.init [`EVERYTHING];
    let screen = set_video_mode ~w:800 ~h:600 ~bpp:32 [`HWSURFACE ; `OPENGL] 
    in
    let s = "TEST RTREE"
    in
      Sdlgl.set_attr [
	RED_SIZE 5;
	GREEN_SIZE 5;
	BLUE_SIZE 5;
	DEPTH_SIZE 16;
	DOUBLEBUFFER true
      ];
      List.iter print_attr (Sdlgl.get_attr ());
      Sdlwm.set_caption s s;
      init_draw ();
      let view = new view screen
      in
	view#reshape ();
	while true do
	  view#event_loop ()
	done
  end


let _ =
  begin 
    main ();
    if not !Sys.interactive then begin
      exit 0;
    end;
  end;;

(* ---------------------------------------------------------------------------- *)

