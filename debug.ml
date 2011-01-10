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
(** debug module: a set of debugging functions *)
let flusherr () = (* flush stderr *) ()


(** output the content of the integer vector v, prefixed by the string s *)
let ivec s v =
  prerr_string s; Printf.fprintf stderr "[| %d ; %d ; %d |]" v.(0) v.(1) v.(2); prerr_newline (); flusherr ()

(** output the content of the float vector v, prefixed by the string s *)
let fvec s v =
  prerr_string s; Printf.fprintf stderr "[| %f ; %f ; %f |]\n" v.(0) v.(1) v.(2); prerr_newline (); flusherr ()

(** output the content of the float v, prefixed by the string s *)
let float s v = 
  prerr_string s; prerr_float v; prerr_newline (); flusherr ()

(** output the content of the integer v, prefixed by the string s *)
let int s v = 
  prerr_string s; prerr_int v; prerr_newline (); flusherr ()

(** output the content of the float tuple (x,y,z), prefixed by the string s *)
let rgb s (x,y,z) = 
  prerr_string s; Printf.fprintf stderr "( %f , %f , %f )\n" x y z; flusherr ()

(** output the content of the float array a, prefixed by the string s *)
let farray s a = 
  prerr_string s;
  prerr_string "[| ";
  Array.iter (fun x -> Printf.fprintf stderr "%f ;" x) a; 
  prerr_string " |]\n"; 
  flusherr ()

(** output the content of the string a, prefixed by the string s, in a per character fashion *)
let string s a = 
  prerr_string s;
  if String.length a <> 0
  then (
    prerr_string "[| ";
    String.iter (fun x -> Printf.fprintf stderr "%d ;" (Char.code x)) a; 
    prerr_string " |]"; 
  );
  prerr_newline ();
  flusherr ()

let msg s =
  prerr_string s; prerr_newline (); flusherr ()
