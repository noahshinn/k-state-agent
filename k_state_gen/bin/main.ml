(*let string = Csv.load "data.csv"*)
(*let () = Csv.print_readable string*)

open Printf
open Float

(*computer.ml*)
module Computer = struct
  let normalize t1 =
    (*let div n k = (n / k) + (n mod k) in*)
    let max_ = List.fold_left max (List.hd t1) (List.tl t1) in
    let min_ = List.fold_left min (List.hd t1) (List.tl t1) in
    List.map (fun a -> div a max_) (List.map (fun b -> sub b min_) t1)
  let rand_lst dim0 dim1 =
    let rec rand_int_lst acc count =
      match count with
      | 0 -> acc
      | _ -> rand_int_lst (List.cons Random.int acc) (count - 1) in
    let rec rand_lst_lst acc count =
      match count with
      | 0 -> acc
      | _ -> rand_lst_lst (List.cons (rand_int_lst [] dim1) acc) (count - 1) in
    rand_lst_lst [] dim0
  end;;

(*loaders.ml*)
(*module Loader = struct*)
  (*let load_csv path = Csv.load*)
  (*end;;*)

(*let () =*)
  (*List.iter (printf "%f ") (Computer.normalize (List.map float_of_int [ 0; 1; 2 ]))*)

(** k-means classification + model-based RL agent *)
