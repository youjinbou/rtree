(** debug module: a set of debugging functions *)

(** output the content of the integer vector v, prefixed by the string s *)
let ivec s v =
  prerr_string s; Printf.fprintf stderr "[| %d ; %d ; %d |]\n" v.(0) v.(1) v.(2); flush stderr

(** output the content of the float vector v, prefixed by the string s *)
let fvec s v =
  prerr_string s; Printf.fprintf stderr "[| %f ; %f ; %f |]\n" v.(0) v.(1) v.(2); flush stderr

(** output the content of the float v, prefixed by the string s *)
let float s v = 
  prerr_string s; prerr_float v; prerr_newline (); flush stderr

(** output the content of the integer v, prefixed by the string s *)
let int s v = 
  prerr_string s; prerr_int v; prerr_newline (); flush stderr

(** output the content of the float tuple (x,y,z), prefixed by the string s *)
let rgb s (x,y,z) = 
  prerr_string s; Printf.fprintf stderr "( %f , %f , %f )\n" x y z; flush stderr

(** output the content of the float array a, prefixed by the string s *)
let farray s a = 
  prerr_string s;
  prerr_string "[| ";
  Array.iter (fun x -> Printf.fprintf stderr "%f ;" x) a; 
  prerr_string " |]\n"; 
  flush stderr

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
  flush stderr

