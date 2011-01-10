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
