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
open Ocamlbuild_plugin;;
open Command;;


dispatch begin function
| After_rules ->

  let lablgl      = "+lablGL"
  and lablgl_libs =  [
    "lablgl";
    "lablglut";
    "gl";
    "glArray";
    "glClear";
    "glDraw";
    "glFunc";
    "glLight";
    "glList";
    "glMap";
    "glMat";
    "glMisc";
    "glPix";
    "glTex";
    "gluMat";
    "gluMisc";
    "gluNurbs";
    "gluQuadric";
    "glut";
    "gluTess";
    "raw";
    "togl";
  ]
  and labltk      = "+labltk"
  and labltk_libs = [ "tk" ]
  and sdl         = "+sdl"
  and sdl_libs    = [
    "sdl";
    "sdlcdrom";
(*  "sdlevent"; *)
    "sdlgl";
    "sdljoystick";
    "sdlkey";
    "sdlloader"; 
    "sdlmixer";
    "sdlmouse";
    "sdltimer";
    "sdlttf";
(*  "sdlvideo"; *)
    "sdlwm";
  ]
  and ounit       = "+oUnit"
  and ounit_libs  =  [ "oUnit" ]
  in

    flag ["ocaml";"compile";"native";"inline"] (S [A "-inline"; A "50"]);
    flag ["ocaml";"compile";"native";"compact"] (S [A "-compact"]);
    flag ["ocaml";"compile";"native";"unsafe"] (S [A "-unsafe"]);
    flag ["ocaml";"compile";"native";"asm"] (S [A "-S"]);

    List.iter (fun (dir, l) -> List.iter (fun x -> ocaml_lib ~extern:true ~dir x) l) [
      labltk, labltk_libs;
      lablgl, lablgl_libs;
      sdl   , sdl_libs   ;
      ounit , ounit_libs
    ]

| _ -> ()
end;;
