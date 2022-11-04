open Printf
open Yojson.Basic.Util

module DataUtils = struct
  let load_json (file : string) : Yojson.Basic.t = Yojson.Basic.from_file file

  (*load structures from json file
    must be formatted as a list of list of list of floats
    ex. 100 structures x 50 points per structure x 2 dimensions per point*)
  let load_data (file : string) : float list list list =
    let raw_data = load_json file in
    List.map
      (fun a -> List.map (fun b -> List.map to_float (to_list b)) (to_list a))
      (to_list raw_data)

  (*FIX: breaking*)
  (*let dump_data (raw_data : float list list list) (file : string) : unit =*)
  (*let data : Yojson.Basic.t = Yojson.Safe.to_basic raw_data in*)
  (*Yojson.Basic.to_file file data*)
end

let () =
  List.iter
    (fun a -> List.iter (fun b -> List.iter (printf "%f") b) a)
    (DataUtils.load_data "./example.json")
