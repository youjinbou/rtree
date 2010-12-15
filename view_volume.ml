open Common
  
type rbox_t = Rbox3d.t


(** view_volume :
    uses opengl transform matrices to determine if a region box is within or
    crosses the viewing volume 
*)
class view_volume () =
object(self)
  
  val planes =
    (* 6 planes corresponding to the 6 faces of a cube for 
       viewing volume clipping:
       normal     position (relative to camera position)
       <1;0;0>    < -width/2 ;         0 ;     0 > => x + (-width/2)  = 0
       <0;1;0>    <        0 ; -height/2 ;     0 > => y + (-height/2) = 0
       <0;0;1>    <        0 ;         0 ;     0 > => z               = 0
       <1;0;0>    <  width/2 ;         0 ;     0 > => x + width/2     = 0
       <0;1;0>    <        0 ;  height/2 ;     0 > => y + height/2    = 0
       <0;0;1>    <        0 ;         0 ; depth > => z + depth       = 0

       what are the width, height and depth values?
       Couldn't we extract this information from the the frustum data?

    *) 
    let mv_mat = GlMat.to_array (GlMat.get_matrix `modelview_matrix)
    and pr_mat = GlMat.to_array (GlMat.get_matrix `projection_matrix)
    and cc_mat = (Array.make 4 (Array.make 4 0.0))
    in 
    let planes = Array.init 6 (fun i -> Array.make 4 0.0)
    in
      Matrix4.concat mv_mat pr_mat cc_mat;
      (* left *)
      planes.(0).(0) <- cc_mat.(0).(3) -. cc_mat.(0).(0); 
      planes.(0).(1) <- cc_mat.(1).(3) -. cc_mat.(1).(0); 
      planes.(0).(2) <- cc_mat.(2).(3) -. cc_mat.(2).(0); 
      planes.(0).(3) <- cc_mat.(3).(3) -. cc_mat.(3).(0); 
      
      (* right *)
      planes.(1).(0) <- cc_mat.(0).(3) +. cc_mat.(0).(0); 
      planes.(1).(1) <- cc_mat.(1).(3) +. cc_mat.(1).(0); 
      planes.(1).(2) <- cc_mat.(2).(3) +. cc_mat.(2).(0); 
      planes.(1).(3) <- cc_mat.(3).(3) +. cc_mat.(3).(0); 
      
      (* down *)
      planes.(2).(0) <- cc_mat.(0).(3) -. cc_mat.(0).(1); 
      planes.(2).(1) <- cc_mat.(1).(3) -. cc_mat.(1).(1); 
      planes.(2).(2) <- cc_mat.(2).(3) -. cc_mat.(2).(1); 
      planes.(2).(3) <- cc_mat.(3).(3) -. cc_mat.(3).(1); 
      
      (* up *)
      planes.(3).(0) <- cc_mat.(0).(3) +. cc_mat.(0).(1); 
      planes.(3).(1) <- cc_mat.(1).(3) +. cc_mat.(1).(1); 
      planes.(3).(2) <- cc_mat.(2).(3) +. cc_mat.(2).(1); 
      planes.(3).(3) <- cc_mat.(3).(3) +. cc_mat.(3).(1); 
            
      (* front *)
      planes.(4).(0) <- cc_mat.(0).(3) -. cc_mat.(0).(2); 
      planes.(4).(1) <- cc_mat.(1).(3) -. cc_mat.(1).(2); 
      planes.(4).(2) <- cc_mat.(2).(3) -. cc_mat.(2).(2); 
      planes.(4).(3) <- cc_mat.(3).(3) -. cc_mat.(3).(2);

      (* back *)
      planes.(5).(0) <- cc_mat.(0).(3) +. cc_mat.(0).(2); 
      planes.(5).(1) <- cc_mat.(1).(3) +. cc_mat.(1).(2); 
      planes.(5).(2) <- cc_mat.(2).(3) +. cc_mat.(2).(2); 
      planes.(5).(3) <- cc_mat.(3).(3) +. cc_mat.(3).(2); 
      planes

  method private inside p =
    (* distance of vertex v to plane *)
    let plane_distance plane v =
      let p = plane in
	p.(0) *. v.(0) +. p.(1) *. v.(1) +. p.(2) *. v.(2) +. p.(3)
    in
    let rec inside i v =
      if i < 0 
      then true
      else
        (plane_distance planes.(i) v >= 0.) && inside (pred i) v
    in
      inside 5 p
	
  method overlaps (rb : rbox_t) = 
    if (self#inside rb#bottom) || (self#inside rb#top) 
    then (prerr_string ((string_of_int (Oo.id rb))^" inside"); prerr_newline ();true)
    else (prerr_string ((string_of_int (Oo.id rb))^" outside"); prerr_newline ();false)
      
  method includes (rb : rbox_t) = 
    (self#inside rb#bottom) && (self#inside rb#top)
      
end

(* ---------------------------------------------------------------------------------*)
