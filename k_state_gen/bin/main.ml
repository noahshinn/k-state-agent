open Printf

module Computer = struct
  let rec transpose lst =
    match lst with
    | [] -> []
    | [] :: xss -> transpose xss
    | (x::xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss)) in
  let mean_state (state_lst : float list list list) : float list list =
    let mean (t1 : float list) : float = 
      let n = List.length t1 in
      Float.div (List.fold_left Float.add 0.0 t1) (float_of_int n) in
    let mean_pt (pt_lst : float list list) : float list =
      List.map mean (transpose pt_lst) in
    List.map mean_pt (transpose state_lst) in
    
  let normalize t1 =
    let max_ = List.fold_left max (List.hd t1) (List.tl t1) in
    let min_ = List.fold_left min (List.hd t1) (List.tl t1) in
    List.map (fun a -> Float.div a max_) (List.map (fun b -> Float.sub b min_) t1) in
  let rand_lst (dim0 : int) (dim1 : int) (dim2 : int) : float list list list =
    let rec rand_float_lst (acc : float list) (count : int) : float list =
      match count with
      | c when c == 0 -> acc
      | _ -> rand_float_lst (List.cons (Random.float 1.0) acc) (count - 1) in
    let rec rand_float_lst_lst (acc : float list list) (count : int) : float list list =
      match count with
      | c when c == 0 -> acc
      | _ -> rand_float_lst_lst (List.cons (rand_float_lst [] dim2) acc) (count - 1) in
    let rec rand_float_lst_lst_lst (acc : float list list list) (count : int) : float list list list =
      match count with
      | c when c == 0 -> acc
      | _ -> rand_float_lst_lst_lst (List.cons (rand_float_lst_lst [] dim1) acc) (count - 1) in
    rand_float_lst_lst_lst [] dim0 in
  let state_eucl_dist (s1 : float list list) (s2 : float list list) : float =
    let eucl_dist t1 t2 =
      Float.sqrt
      (List.fold_left
        Float.add
        0.0
        (List.map2
          (fun a b -> Float.pow (Float.sub a b) 2.)
          t1
          t2)) in
    List.fold_left Float.add 0.0 (List.map2 (fun (a : float list) (b : float list) -> eucl_dist a b) s1 s2) in
  let k_means (k : int) (structures : float list list list) (niter : int) : float list list list =
    let dim0 : int = List.length structures in
    let dim1 : int = List.length (List.hd structures) in
    let dim2 : int = List.length (List.hd (List.hd structures)) in
    let normalized_structures : float list list list =
      List.map
        (fun (a : float list list) : float list list ->
          List.map normalize a)
        structures in

    let rec get_closest_centr
      (s1 : float list list)
      (centr_lst : float list list list)
      (closest_centr : float list list)
      (closest_dist : float) : float list list =
      match centr_lst with
      | [] -> closest_centr
      | _ -> (let cur_dist : float = state_eucl_dist s1 (List.hd centr_lst) in
             if cur_dist < closest_dist
               then get_closest_centr s1 (List.tl centr_lst) (List.hd centr_lst) cur_dist
             else get_closest_centr s1 (List.tl centr_lst) closest_centr closest_dist) in

    let get_centr centr =
      let hash = List.fold_left
                   (fun (a : float list list) hash_acc -> 
                     let s_centr = get_closest_centr a centr (List.hd centr) Float.infinity in
                     if Hashtbl.mem hash_acc s_centr
                       then Hashtbl.replace hash_acc s_centr (List.cons a (Hashtbl.find hash_acc s_centr))
                     else
                       Hashtbl.add hash_acc s_centr (List.cons a []))
                   (Hashtbl.create 123456)
                   normalized_structures in
      let rec hash_to_centr hash_keys (lst : float list list list) : float list list list =
        match hash_keys with
        | [] -> lst
        | _ -> (let hash_key : float list list = List.hd hash_keys in
               let hash_val : float list list list = Hashtbl.find hash hash_key in
               hash_to_centr
                 (List.tl hash_keys)
                 (List.cons
                   (if (List.length hash_val < 0)
                     then hash_key
                   else mean_state hash_val)
                   lst)) in

      hash_to_centr (Hashtbl.to_seq_keys hash) [] in

    let rec k_means_iter (centr : float list list list) (iter : int) : float list list list =
      match iter with
      | n when n == niter -> centr
      | _ -> k_means_iter
               (get_centr centr)
               (iter - 1) in
    let init_centr : float list list list = rand_lst dim0 dim1 dim2 in
    k_means_iter init_centr niter

  end;;

(*loaders.ml*)
(*module Loader = struct*)
  (*let load_csv path = Csv.load*)
  (*end;;*)

let () =
  List.iter (printf "%f ") (Computer.k_means
                              3
                              [[1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.];
                               [1.; 1.; 1.]]
                              100)

(** state classification + RL agent via q-learning *)
