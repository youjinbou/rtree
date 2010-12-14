open Sdl;;
open Sdlvideo;;
open Sdlevent;;
open Sdlkey;;
open Sdlmouse;;
open Sdlgl;;
open Sdlwm;;
open GlPix;;


open Primitives
open Vec3

module FVec3  = Vec3.Make(Float)

open Debug

class box3d =
  let i = ref 0 in
    fun b t ->
object(self)

  val id = let id = !i in incr i; id

  val gl_id = 
    let xb, yb, zb = FVec3.to_tuple b
    and xt, yt, zt = FVec3.to_tuple t
    and l       = GlList.create `compile 
    in
      GlList.begins l ~mode:`compile;
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
      GlDraw.ends ();
      GlList.ends ();
      l

  method draw () =
    GlList.call gl_id
      
end

open Rtree 

module Rtree3Def =
struct 
  let minimum = 2
  let maximum = 4
  type t = box3d
end


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

module Rtree3 = Rtree.Make(FVec3)(Rtree3Def)

let usleep () = ignore (Unix.select [] [] [] 0.01 )
    
class view screen = 
object (self)

  val width  = 800
  val height = 600

  val rtree = 
    let range = 100.0 in
    let maxv = [| range ; range ; range |] in
    let origb = [| -1.0; -1.0 ; -1.0 |]
    and origt = [| 1.0 ; 1.0 ; 1.0 |] in
    let r = Rtree3.make (new Rtree3.Rbox.t origb origt) (new box3d origb origt)
    in
    let rec addup i =
      if i = 0 
      then r
      else 
	let b = (FVec3.random maxv)
	and t = (FVec3.random maxv)
	in
	  Rtree3.insert r (new Rtree3.Rbox.t b t) (new box3d b t);
	  addup (pred i)
    in
      addup 100

  val mutable frustum_min = [| 0. ; 0. ; 0. |]
  val mutable frustum_max = [| 0. ; 0. ; 0. |]
  val mutable view_rot   =  [| 0.0 ; 0.0 ; 0.0 |]
  val mutable view_scale  = 1.
  val mutable old_xmcoord = 0
  val mutable old_ymcoord = 0
  val mutable pos         = [| 0. ; -5. ; -15. |]
  val mutable speed       = [| 0. ;  0. ; 0. |]

  val mutable texpos      = [| 0. ; 0.  ; 0. |]

  val mutable keyboard_handler = fun a b -> ()
  val mutable mouse_handler    = fun a b c d -> ()
  val mutable motion_handler   = fun a b -> ()

  val modangle = fun x -> if x > 180. then (x -. 360.) else if (x < -180.) then (x +. 360.) else x

  method rotx  a = view_rot.(0) <- modangle (view_rot.(0) +. a)
  method roty  a = view_rot.(1) <- modangle (view_rot.(1) +. a)
  method rotz  a = view_rot.(2) <- modangle (view_rot.(2) +. a)

  val mutable lists = []

  val grid = 
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
	  let fi = float_of_int (i*10) in
	    GlDraw.vertex ~x:(fi) ~z:(float_of_int (j*10))      ~y:(0.) ();
	    GlDraw.vertex ~x:(fi) ~z:(float_of_int ((j+1)*10))  ~y:(0.) ()
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
	
  method draw_grid =
    GlList.call grid;

  val pi = acos (-.1.)

  method draw_box b =
    b#draw ()

  method draw_tree =
    GlDraw.color (0.0,0.0,1.0);
    Rtree3.iter rtree self#draw_box 

  method view_setup =
    GlMat.scale ~x:view_scale ~y:view_scale ~z:view_scale ();
    GlMat.rotate ~angle:view_rot.(0) ~x:1.0 ();
    GlMat.rotate ~angle:view_rot.(1) ~y:1.0 ();
    GlMat.rotate ~angle:view_rot.(2) ~z:1.0 ();
    GlMat.translate ~x:pos.(0) ~y:pos.(1) ~z:pos.(2) ();

  (* draw the whole thing *)
  method draw =
    GlClear.clear [`color;`depth];
    GlMat.mode `modelview;
    GlMat.load_identity();
    self#view_setup;
    self#draw_grid; 
    self#draw_tree;

  method display =
    GlMat.mode `projection;
    GlMat.load_identity();
    GlMat.frustum 
      ~x:( frustum_min.(0), frustum_max.(0) ) 
      ~y:( frustum_min.(1), frustum_max.(1) ) 
      ~z:( frustum_min.(2), frustum_max.(2) );
    self#draw;
    Sdlgl.swap_buffers ()
      
  method reshape =
    GlDraw.viewport ~x:0 ~y:0 ~w:width ~h:height;
    GlMat.mode `projection;
    GlMat.load_identity ();
    let r = (float width) /. (float height) 
    and f = 1.5
    in
    frustum_min.(0) <- -.r ;        (* left view frustum    *)
    frustum_max.(0) <-   r ;        (* right view frustum   *)
    frustum_min.(1) <- -.1.;        (* bottom view frustum  *)
    frustum_max.(1) <-   1.;        (* top view frustum     *)
    frustum_min.(2) <-   f ;        (* view depth           *)
    frustum_max.(2) <-  60.;        (* view depth           *)
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

  method depth a =
    frustum_max.(2) <- frustum_max.(2) +. a 

  method motion_off =
    ()
    
  method mouse ~btn ~state ~xmcoord ~ymcoord =
    match state with 
	RELEASED -> self#set_motion (fun a b  -> self#motion_off )
      | PRESSED  -> (
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
	    | BUTTON_WHEELUP   -> self#depth 10.
	    | BUTTON_WHEELDOWN -> self#depth (-10.)
	    | _            -> ()
 	)

  method keyboard ~key ~state =
    let speed_fact = 0.1 in
    match state with 
	RELEASED -> (
	  match key with
	    | KEY_f       -> speed.(0) <-  speed.(0) +. speed_fact 
	    | KEY_s       -> speed.(0) <-  speed.(0) -. speed_fact 
	    | KEY_e       -> speed.(2) <-  speed.(2) -. speed_fact 
	    | KEY_d       -> speed.(2) <-  speed.(2) +. speed_fact 
	    | KEY_t       -> speed.(1) <-  speed.(1) +. speed_fact 
	    | KEY_g       -> speed.(1) <-  speed.(1) -. speed_fact 
	    | _           -> ()
	)
      | PRESSED ->
	  match key with
	      KEY_UNKNOWN -> ()
	    | KEY_ESCAPE  -> Sdl.quit ();exit 0
	    | KEY_r       -> self#reshape;
	    | KEY_f       -> speed.(0) <-  speed.(0) -. speed_fact 
	    | KEY_s       -> speed.(0) <-  speed.(0) +. speed_fact 
	    | KEY_e       -> speed.(2) <-  speed.(2) +. speed_fact 
	    | KEY_d       -> speed.(2) <-  speed.(2) -. speed_fact 
	    | KEY_t       -> speed.(1) <-  speed.(1) -. speed_fact 
	    | KEY_g       -> speed.(1) <-  speed.(1) +. speed_fact 
	    | KEY_SPACE   -> Debug.fvec "pos: " pos; Debug.fvec "rot: " view_rot;  Debug.fvec "frustum_min : " frustum_min; Debug.fvec "frustum_max : " frustum_max ; Debug.float "scale :" view_scale
	    | _           -> ()

  method motion x y = motion_handler x y 

  method event_loop =
    self#display;
    pos.(0) <- speed.(0) +. pos.(0);
    pos.(1) <- speed.(1) +. pos.(1);
    pos.(2) <- speed.(2) +. pos.(2);
    match poll () with
	None   -> usleep ()
      | Some e -> match e with 
	    KEYDOWN           k 
	  | KEYUP             k -> self#keyboard ~state:k.ke_state ~key:k.keysym
	  | MOUSEBUTTONDOWN   m
	  | MOUSEBUTTONUP     m -> self#mouse ~btn:m.mbe_button ~state:m.mbe_state ~xmcoord:m.mbe_x ~ymcoord:m.mbe_y
	  | MOUSEMOTION       m -> self#motion m.mme_x m.mme_y
	  | _                   -> prerr_string "unhandled event";prerr_newline ()


  method set_keyboard f = keyboard_handler <- f
  method set_mouse    f = mouse_handler    <- f
  method set_motion   f = motion_handler   <- f

  initializer (
    self#set_keyboard (fun a b     -> self#keyboard a b);
    self#set_mouse    (fun a b c d -> self#mouse a b c d);
    self#set_motion   (fun a b     -> self#motion_off );
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
	view#reshape;
	while true do
	  view#event_loop 
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

