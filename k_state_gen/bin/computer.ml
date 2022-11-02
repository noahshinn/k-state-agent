(*let rec sqr a = function*)
(*| 0 -> 1*)
(*| 1 -> a*)
(*| n ->*)
(*let b = sqr a (n / 2) in*)
(*b * b * (if n mod 2 = 0 then 1 else a)*)

(*let rec eucl_dist l r =*)
(*match l r with*)
(*| a_val_l :: rest_l, a_val_r :: rest_r -> sqr (a_val_l - a_val_r) 2 + eucl_dist rest_l rest_r*)
(*| _ -> 0*)

(*module computer = struct*)
(*let eucl_dist t1 t2 = List.fold_left (fun s -> fun x -> s + x * x) 0 (List.map2 (-) t1 t2);;*)
