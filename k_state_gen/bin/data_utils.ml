module DataUtils = struct
  let load_json (file : string) = Yojson.Basic.from_file file
end
