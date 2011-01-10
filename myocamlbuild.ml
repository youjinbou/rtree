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

  let lablgl = "+lablGL" 
  and labltk = "+labltk"
  and sdl    = "+sdl"
  and ounit  = "+oUnit"

  in

    (* add ulex preprocessor for use_ulex tag 
    flag ["ocaml";"pp";"use_ulex"] (S[A "camlp4o";A "-I";A "/usr/lib/ocaml/ulex";A "pa_ulex.cma"]);
    flag ["ocaml";"use_ulex"] (S[A "-I";A "/usr/lib/ocaml/ulex"]);
    *)
    flag ["ocaml";"compile";"native";"inline"] (S [A "-inline"; A "50"]);
    flag ["ocaml";"compile";"native";"compact"] (S [A "-compact"]);
    flag ["ocaml";"compile";"native";"unsafe"] (S [A "-unsafe"]);
    flag ["ocaml";"compile";"native";"asm"] (S [A "-S"]);


    ocaml_lib ~extern:true ~dir:labltk "tk";

    ocaml_lib ~extern:true ~dir:lablgl "lablgl";
    ocaml_lib ~extern:true ~dir:lablgl "lablglut";
    ocaml_lib ~extern:true ~dir:lablgl "gl";
    ocaml_lib ~extern:true ~dir:lablgl "glArray";
    ocaml_lib ~extern:true ~dir:lablgl "glClear";
    ocaml_lib ~extern:true ~dir:lablgl "glDraw";
    ocaml_lib ~extern:true ~dir:lablgl "glFunc";
    ocaml_lib ~extern:true ~dir:lablgl "glLight";
    ocaml_lib ~extern:true ~dir:lablgl "glList";
    ocaml_lib ~extern:true ~dir:lablgl "glMap";
    ocaml_lib ~extern:true ~dir:lablgl "glMat";
    ocaml_lib ~extern:true ~dir:lablgl "glMisc";
    ocaml_lib ~extern:true ~dir:lablgl "glPix";
    ocaml_lib ~extern:true ~dir:lablgl "glTex";
    ocaml_lib ~extern:true ~dir:lablgl "gluMat";
    ocaml_lib ~extern:true ~dir:lablgl "gluMisc";
    ocaml_lib ~extern:true ~dir:lablgl "gluNurbs";
    ocaml_lib ~extern:true ~dir:lablgl "gluQuadric";
    ocaml_lib ~extern:true ~dir:lablgl "glut";
    ocaml_lib ~extern:true ~dir:lablgl "gluTess";
    ocaml_lib ~extern:true ~dir:lablgl "raw";
    ocaml_lib ~extern:true ~dir:lablgl "togl";

    ocaml_lib ~extern:true ~dir:sdl    "sdl";
    ocaml_lib ~extern:true ~dir:sdl    "sdlcdrom";
(*    ocaml_lib ~extern:true ~dir:sdl    "sdlevent"; *)
    ocaml_lib ~extern:true ~dir:sdl    "sdlgl";
    ocaml_lib ~extern:true ~dir:sdl    "sdljoystick";
    ocaml_lib ~extern:true ~dir:sdl    "sdlkey";
    ocaml_lib ~extern:true ~dir:sdl    "sdlloader"; 
    ocaml_lib ~extern:true ~dir:sdl    "sdlmixer";
    ocaml_lib ~extern:true ~dir:sdl    "sdlmouse";
    ocaml_lib ~extern:true ~dir:sdl    "sdltimer";
    ocaml_lib ~extern:true ~dir:sdl    "sdlttf";
(*    ocaml_lib ~extern:true ~dir:sdl    "sdlvideo"; *)
    ocaml_lib ~extern:true ~dir:sdl    "sdlwm";

    ocaml_lib ~extern:true ~dir:ounit  "oUnit";

| _ -> ()
end;;
