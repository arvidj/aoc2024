open Aoc2024

let sum input =
  let matches = Rex.(matches input (rex "(mul\\(\\d+,\\d+\\))")) in
  List.map
    (fun m ->
      match Rex.(m =~** rex "mul\\((\\d+),(\\d+)\\)") with
      | Some (s1, s2) -> int_of_string s1 * int_of_string s2
      | None -> failwith "[sum] can't happen")
    matches
  |> sum

let check ~tag ~to_string ~value ~expected =
  if value <> expected then
    fail "%s: expected %s, got %s" tag (to_string expected) (to_string value)
  else pfn "%s: %s" tag (to_string expected)

let () =
  let sample = Base.read_file "bin/ex3-sample.txt" in
  check ~to_string:string_of_int ~tag:"sample" ~value:(sum sample) ~expected:161;
  let input = Base.read_file "bin/ex3-input.txt" in
  check ~to_string:string_of_int ~tag:"input" ~value:(sum input)
    ~expected:174960292
