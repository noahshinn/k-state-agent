(*let string = Csv.load "data.csv"*)
(*let () = Csv.print_readable string*)

open Printf
(*open Float*)

let eucl_dist t1 t2 = Float.sqrt (float_of_int (List.fold_left (fun s -> fun x -> s + x * x) 0 (List.map2 (-) t1 t2)))
let () = printf "%f" (eucl_dist [ 2;2;1 ] [ 5;6;0 ])

  (*List.fold_left*)

    (*(fun s x -> s + (x * x))*)
    (*0 (List.map2 ( - ) t1 t2) printf eucl_dist [ 0; 0; 0 ] [ 2; 2; 2 ]*)

(** k-means classification + model-based RL agent *)
