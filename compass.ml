open Sdl
open Sdlvideo
open Sdlevent
open Sdlkey
open Sdlmouse
open Sdlgl
open Sdlwm
open GlPix

open Common

class t position right up front vright vup vfront =
  let scaling = 0.2 in
object(self)

  val front_  = FVec3.add position (FVec3.scale front scaling)
  val right_  = FVec3.add position (FVec3.scale right scaling)
  val up_     = FVec3.add position (FVec3.scale up scaling)
  val vfront_ = FVec3.add position (FVec3.scale vfront scaling)
  val vright_ = FVec3.add position (FVec3.scale vright scaling)
  val vup_    = FVec3.add position (FVec3.scale vup scaling)

  (* compass "on screen" position *)
  val spos_   = FVec3.add4 position (FVec3.scale vfront (-.10.0)) (FVec3.scale vright (-.5.0)) (FVec3.scale vup (-.5.0))

  method draw () =
    let draw_vertex v =
      let (x,y,z) = FVec3.to_tuple v in
      GlDraw.vertex ~x ~y ~z ()
    in
    let draw_axis p v color =
      GlDraw.color color;
      GlDraw.begins `lines;
      draw_vertex p;
      draw_vertex v;
      GlDraw.ends ()

    in
(*      GlMat.scale3 (20.0,20.0,20.0); *)
      List.iter Gl.disable [`texture_2d ];

      draw_axis spos_ front_ (1.0, 0.0, 0.0);
      draw_axis spos_ vfront_ (0.5, 0.0, 0.0);

      draw_axis spos_ right_ (0.0, 1.0, 0.0);
      draw_axis spos_ vright_ (0.0, 0.5, 0.0);

      draw_axis spos_ up_ (0.0, 0.0, 1.0);
      draw_axis spos_ vup_ (0.0, 0.0, 0.5);

      List.iter Gl.enable [`texture_2d ];
end
