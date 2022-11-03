(*open Printf*)
(*open Stategen*)

(*test run*)
(*let () =*)
(*List.iter*)
(*(fun a -> List.iter (fun b -> List.iter (printf "%f ") b) a)*)
(*(Stategen.k_means 200 (Stategen.rand_lst 100_001 50 2) 100)*)

(** state classification + RL agent via q-learning *)
