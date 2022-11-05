open Yojson.Basic.Util

module DataUtils = struct
  let load_json (file : string) : Yojson.Basic.t = Yojson.Basic.from_file file

  (*load structures or states from json file
    the data to read must be formatted as a list of list of list of floats
    ex. 100 structures x 50 points per structure x 2 dimensions per point*)
  let load_data (file : string) : float list list list =
    let raw_data = load_json file in
    List.map
      (fun a -> List.map (fun b -> List.map to_float (to_list b)) (to_list a))
      (to_list raw_data)

  (*dump structures or states to json file
    the data to write must be formatted as a list of list of list of floats
    ex. 200 states x 50 points per state x 10 dimensions per point*)
  let dump_data (raw_data : float list list list) (file : string) : unit =
    let data : Yojson.Basic.t =
      (*`List (List.map (fun a -> `Float a) raw_data)*)
      `List
        (List.map
           (fun a ->
             `List
               (List.map (fun b -> `List (List.map (fun c -> `Float c) b)) a))
           raw_data)
    in
    Yojson.Basic.to_file file data
end
