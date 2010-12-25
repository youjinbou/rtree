open Sdl
open Sdlvideo
open Sdlevent
open Sdlkey
open Sdlmouse
open Sdlgl
open Sdlwm
open GlPix

open Common

module FVec4 = Vec4.Make(Primitives.Float)

module FMatrix4 = Matrix.Make(FVec4)
  
type rbox_t = Rbox3d.t

(** view_volume :
    determine if a region box is within or crosses the viewing volume 
*)
class t vright vup vfront position =
object(self)
  
  val planes = 
    (* 4 planes corresponding to the 4 front faces of a tetrahedron with top vertex
       being the view position.
       
       normals:
       front + up
       front - up
       front + right
       front - right
    *)
    let n1 = FVec3.opp (FVec3.add vfront vright)
    and n2 = FVec3.sub vright vfront
    and n3 = FVec3.opp (FVec3.add vfront vup)
    and n4 = FVec3.sub vup vfront
    in
      [| Plane.make n1 position ; 
	 Plane.make n2 position ; 
	 Plane.make n3 position ; 
	 Plane.make n4 position |]
    (*       
       -----------------------------------------------------------------------

       viewing volume clipping:
       normal (N)    position (P)                        plane eq.
        i  <1;0;0>   < -width/2 ;         0 ;     0 > -> x + (-width/2)  = 0
        j  <0;1;0>   <        0 ; -height/2 ;     0 > -> y + (-height/2) = 0
        k  <0;0;1>   <        0 ;         0 ;     0 > -> z               = 0
       -i <-1;0;0>   <  width/2 ;         0 ;     0 > -> x + width/2     = 0
       -j <0;-1;0>   <        0 ;  height/2 ;     0 > -> y + height/2    = 0
       -k <0;0;-1>   <        0 ;         0 ; depth > -> z + depth       = 0

       in camera space => need to transform the coords from absolute space 
                          to camera space

       what are the width, height and depth values?
       Couldn't we extract this information from the the frustum data?

       plane eq : 
       N(xn,yn,zn) is the plane Normal
       P(xo,yo,zo) is a point in the plane
       V(x,y,z)    is the point to be tested
       
             PV . N >= 0
       => V.N - P.N >= 0
       x.xn + y.yn + z.zn - (xo.xn + yo.yn + zo.zn) >= 0
       (1)  x - xo <= 0  -> x <=  xo
       (2)  y - yo <= 0  -> y <=  yo
       (3)  z - zo <= 0  -> z <=  zo
       (1)  x + xo >= 0  -> x >= -xo
       (2)  y + yo >= 0  -> y >= -yo
       (3)  z + zo >= 0  -> z >= -zo

       -xo < x < xo
       -yo < y < yo
         0 < z < zo

       < -xo ; yo ; 0 > 
    *) 

  method private inside p =
    let rec inside i v =
      if i < 0 
      then true
      else
        (Plane.in_or_above planes.(i) v) && inside (pred i) v
    in
      inside 3 p 
	
  method overlaps (rb : rbox_t) = 
    let rec check_plane bbox = 
         ((Plane.check_bbox planes.(0) bbox) <> 2) 
      && ((Plane.check_bbox planes.(1) bbox) <> 2)
      && ((Plane.check_bbox planes.(2) bbox) <> 2)
      && ((Plane.check_bbox planes.(3) bbox) <> 2)

    and b = rb#bottom
    and t = rb#top
    in
      check_plane (b,t)

  method includes (rb : rbox_t) = 
    (self#inside rb#bottom) && (self#inside rb#top)

  method draw () = 
    let draw_plane (n,k,s) =
      let (x,y,z)    = FVec3.to_tuple (FVec3.sub position (FVec3.scale vfront 4.))
      and (xn,yn,zn) = FVec3.to_tuple n
      in
	GlDraw.color (0.3,0.6,0.9);
	GlDraw.begins `lines;
	GlDraw.vertex ~x ~y ~z ();
	GlDraw.vertex ~x:(x+.xn) ~y:(y+.yn) ~z:(z+.zn) ();
	GlDraw.ends ()
    in
    Array.iter (fun x -> draw_plane x) planes
  
  initializer (
    prerr_string "new view_volume"; prerr_newline (); flush stderr
  )
      
end

(* ---------------------------------------------------------------------------------*)
