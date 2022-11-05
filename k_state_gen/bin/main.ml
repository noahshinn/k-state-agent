open Printf

exception MaxNstructuresExceeded

module DataUtils = struct
  (*json read*)
  let load_json (file : string) : Yojson.Basic.t = Yojson.Basic.from_file file

  (*load structures or states from json file
    the data to read must be formatted as a list of list of list of floats
    ex. 100 structures x 50 points per structure x 2 dimensions per point*)
  let load_data (file : string) : float list list list =
    let raw_data : Yojson.Basic.t = load_json file in
    List.map
      (fun a ->
        List.map
          (fun b ->
            List.map Yojson.Basic.Util.to_float (Yojson.Basic.Util.to_list b))
          (Yojson.Basic.Util.to_list a))
      (Yojson.Basic.Util.to_list raw_data)

  (*dump structures or states to json file
    the data to write must be formatted as a list of list of list of floats
    ex. 200 states x 50 points per state x 10 dimensions per point*)
  let dump_data (raw_data : float list list list) (file : string) : unit =
    let data : Yojson.Basic.t =
      `List
        (List.map
           (fun a ->
             `List
               (List.map (fun b -> `List (List.map (fun c -> `Float c) b)) a))
           raw_data)
    in

    Yojson.Basic.to_file file data
end

module StateGeneration = struct
  (*transposes a 2d list*)
  let rec transpose lst =
    match lst with
    | [] -> []
    | [] :: xss -> transpose xss
    | (x :: xs) :: xss ->
        List.((x :: map hd xss) :: transpose (xs :: map tl xss))

  (*computes the mean structure from a list of structures*)
  let mean_state (state_lst : float list list list) : float list list =
    let mean (t1 : float list) : float =
      let n = List.length t1 in
      Float.div (List.fold_left Float.add 0.0 t1) (float_of_int n)
    in
    let mean_pt (pt_lst : float list list) : float list =
      List.map mean (transpose pt_lst)
    in
    List.map mean_pt (transpose state_lst)

  (*normalizes the values of a structure to the range [0.0, 1.0]
    to establish translation and magntitude invariance*)
  let normalize t1 =
    let max_ = List.fold_left max (List.hd t1) (List.tl t1) in
    let min_ = List.fold_left min (List.hd t1) (List.tl t1) in
    List.map
      (fun a -> Float.div a max_)
      (List.map (fun b -> Float.sub b min_) t1)

  (*generates a random list of list of list of floats
    example usage: generate 200 random centroid starting structures with 50
      points per structure and 2 dimensions per point for k-means clustering
    >>> rand_lst 200 50 2
  *)
  let rand_lst (dim0 : int) (dim1 : int) (dim2 : int) : float list list list =
    let rec rand_float_lst (acc : float list) (count : int) : float list =
      match count with
      | c when c == 0 -> acc
      | _ -> rand_float_lst (Random.float 1.0 :: acc) (count - 1)
    in
    (*TODO: fix with abstraction*)
    let rec rand_float_lst_lst (acc : float list list) (count : int) :
        float list list =
      match count with
      | c when c == 0 -> acc
      | _ -> rand_float_lst_lst (rand_float_lst [] dim2 :: acc) (count - 1)
    in
    (*TODO: fix with abstraction*)
    let rec rand_float_lst_lst_lst (acc : float list list list) (count : int) :
        float list list list =
      match count with
      | c when c == 0 -> acc
      | _ ->
          rand_float_lst_lst_lst (rand_float_lst_lst [] dim1 :: acc) (count - 1)
    in
    rand_float_lst_lst_lst [] dim0

  (*computes a euclidean-based evaluation function on two states to determine similarity*)
  let state_eucl_dist (s1 : float list list) (s2 : float list list) : float =
    let eucl_dist t1 t2 =
      Float.sqrt
        (List.fold_left Float.add 0.0
           (List.map2 (fun a b -> Float.pow (Float.sub a b) 2.) t1 t2))
    in
    List.fold_left Float.add 0.0
      (List.map2 (fun (a : float list) (b : float list) -> eucl_dist a b) s1 s2)

  (*generates K states where K is the number of centroids*)
  let k_means (k : int) (structures : float list list list) (niter : int) :
      float list list list =
    let () =
      let nstructures = List.length structures in
      if nstructures > 100_000 then (
        printf "%d" nstructures;
        print_string "max number of structures is 100_000 ";
        raise MaxNstructuresExceeded)
    in
    let dim1 : int = List.length (List.hd structures) in
    let dim2 : int = List.length (List.hd (List.hd structures)) in
    let normalized_structures : float list list list =
      List.map
        (fun (a : float list list) : float list list -> List.map normalize a)
        structures
    in

    let rec get_closest_centr (s1 : float list list)
        (centr_lst : float list list list) (closest_centr : float list list)
        (closest_dist : float) : float list list =
      match centr_lst with
      | [] -> closest_centr
      | _ ->
          let cur_dist : float = state_eucl_dist s1 (List.hd centr_lst) in
          if cur_dist < closest_dist then
            get_closest_centr s1 (List.tl centr_lst) (List.hd centr_lst)
              cur_dist
          else
            get_closest_centr s1 (List.tl centr_lst) closest_centr closest_dist
    in

    let get_centr (centr : float list list list) : float list list list =
      let hash = Hashtbl.create 123456 in
      let () =
        List.iter
          (fun (a : float list list) ->
            let s_centr =
              get_closest_centr a centr (List.hd centr) Float.infinity
            in
            if Hashtbl.mem hash s_centr then
              Hashtbl.replace hash s_centr (a :: Hashtbl.find hash s_centr)
            else Hashtbl.add hash s_centr (a :: []))
          normalized_structures
      in

      let hash_to_centr hashtbl : float list list list =
        Hashtbl.fold
          (fun (a : float list list) (b : float list list list)
               (acc : float list list list) ->
            match b with [] -> a :: acc | _ -> mean_state b :: acc)
          hashtbl []
      in
      hash_to_centr hash
    in

    let rec k_means_iter (centr : float list list list) (iter : int) :
        float list list list =
      match iter with
      | n when n == niter -> centr
      | _ -> k_means_iter (get_centr centr) (iter - 1)
    in

    let init_centr : float list list list = rand_lst k dim1 dim2 in
    k_means_iter init_centr niter
end

module QLearning = struct
  (*determines the next decision
    exploit: true
    explore: false
  *)
  let exec_eps_greedy (eps : float) : bool =
    let () = assert (eps <= 1.0 && eps <= 1.0) in
    Random.float 1.0 > eps

  (*classifies a structure to its nearest state*)
  let structure_to_state (structure : float list list)
      (states : float list list list)
      (similarity_func : float list list -> float list list -> float) :
      float list list =
    let () = assert (List.length states > 0) in
    let rec structure_to_state_iter (states_lst : float list list list)
        (cur_nearest_state : float list list) (cur_min_nearness : float) :
        float list list =
      match states_lst with
      | [] -> cur_nearest_state
      | _ ->
          let cur_state = List.hd states_lst in
          let nearness = similarity_func structure cur_state in
          if nearness < cur_min_nearness then
            structure_to_state_iter (List.tl states_lst) cur_state nearness
          else
            structure_to_state_iter (List.tl states_lst) cur_nearest_state
              cur_min_nearness
    in
    let first_state = List.hd states in
    structure_to_state_iter (List.tl states) first_state
      (similarity_func structure first_state)

  (* updates the current Q value given precomputed values
     Follows the formula Q(s, a) <-- Q(s, a) + \alpha * [R + \gamma * max_over_a\'[Q(s\', a\')]  - Q(s, a)]
     where
       R = reward
       s = state
       a = action
       s\' = next state
       a\' = action from next state
       \alpha = learning rate
       \gamma = discount factor
  *)
  let compute_q_update (cur_q : float) (opt_next_q : float) (reward : float)
      (alpha : float) (gamma : float) : float =
    Float.add cur_q
      (Float.mul alpha
         (Float.sub (Float.add reward (Float.mul gamma opt_next_q)) cur_q))
end

let () =
  let nstates : int = 200 in
  let structures : float list list list = StateGeneration.rand_lst 1000 50 2 in
  let niter : int = 1000 in
  let states : float list list list =
    StateGeneration.k_means nstates structures niter
  in
  DataUtils.dump_data states "example_out.json"
