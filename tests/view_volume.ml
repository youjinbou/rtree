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

module FVec4 = Vec4.Make(Primitives.Float)

module FMatrix4 = Matrix.Make(FVec4)
  
type rbox_t = Rb.t

(** view_volume :
    determine if a region box is within or crosses the viewing volume 
*)
class t vright vup vfront depth position =
object(self)

  val planes = 
    (* 4 planes corresponding to the 4 front faces of a tetrahedron with top vertex
       being the view position.
       normals:
       front + up
       front - up
       front + right
       front - right

       1 plane corresponding to the far view clipping:
       normal : - vfront
       
    *)
    let n1 = FVec3.opp (FVec3.add vfront vright)
    and n2 = FVec3.sub vright vfront
    and n3 = FVec3.opp (FVec3.add vfront vup)
    and n4 = FVec3.sub vup vfront
    and n5 = FVec3.opp vfront
    and p  = FVec3.add position (FVec3.scale vfront depth)
    in
      [| Plane.make n1 position ; 
	 Plane.make n2 position ; 
	 Plane.make n3 position ; 
	 Plane.make n4 position ;
	 Plane.make n5 p |]
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
      && ((Plane.check_bbox planes.(4) bbox) <> 2)

    and b = rb#bottom
    and t = rb#top
    in
      check_plane (b,t)

  method contains (rb : rbox_t) = 
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


  (* all these are defined to provide a sig compatibility with Rb.t 
     required by the module type used in the library
  *)
  method top = vright
  method bottom = vright
  method area = 0.0
  method area_with (x : Rb.t) = 0.0
  method expand (x : Rb.t) = (self :> Rb.t)
  method to_string = "view volume"
  
  initializer (
    (*
    prerr_endline "new view_volume"; flush stderr
    *)
  )
      
end

(* ---------------------------------------------------------------------------------*)
