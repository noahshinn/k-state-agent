module QLearningUtils = struct
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
