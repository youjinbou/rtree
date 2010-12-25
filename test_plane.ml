open OUnit
open Random

open Primitives

module FVec3 = Vec3.Make(Float)

let _ = Random.init (int_of_float ((Sys.time ()) *. 1000.))

let p = FVec3.of_tuple (Random.float 10.0, Random.float 10.0, Random.float 10.)
let n = FVec3.normalize (FVec3.of_tuple (Random.float 10.0, Random.float 10.0, Random.float 10.))

let pl = Plane.make n p

exception Assert_error of string


let test_plane () = 
  assert_equal ~msg:"inside" (Plane.inside pl p) true;
  assert_equal ~msg:"above" (Plane.above pl (FVec3.add p n)) true;
  assert_equal ~msg:"below" (Plane.below pl (FVec3.sub p n)) true


let plane_test_list =
  TestLabel ("[ plane test ]", TestCase (fun _ -> test_plane ()))

let test_suite =
  TestLabel("[ Test Suite ]",
	    TestList
	      [
		plane_test_list
	      ]
	   )

let _ =
  run_test_tt test_suite
