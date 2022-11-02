(*let string = Csv.load "data.csv"*)
(*let () = Csv.print_readable string*)

open Printf
open Float

(*computer.ml*)
module Computer = struct
  let normalize t1 =
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
  let state_eucl_dist s1 s2 =
    let eucl_dist t1 t2 =
      sqrt
      (List.fold_left
        add
        0.0
        (List.map2
          (fun a b -> pow (sub a b) 2.)
          t1
          t2)) in
    List.map2 (fun a b -> eucl_dist a b) s1 s2
  end;;

(*loaders.ml*)
(*module Loader = struct*)
  (*let load_csv path = Csv.load*)
  (*end;;*)

let () =
  List.iter (printf "%f ") (Computer.state_eucl_dist [[0.; 0.; 0.]; [0.; 0.; 0.]; [0.; 0.; 0.]] [[1.; 1.; 1.;]; [1.; 1.; 1.;]; [1.; 1.; 1.;]])

(** k-means classification + model-based RL agent *)
