open Aoc2024
open Ex3

let sum1 input =
  let tokens = parse_re input in
  let _enabled, sum =
    List.fold_left
      (fun (enabled, sum) tok ->
        (enabled, match tok with Mul (a, b) -> sum + (a * b) | _ -> sum))
      (true, 0) tokens
  in
  sum

let sum2 ?(debug = false) input =
  let tokens = parse_re input in
  if debug then List.iter (fun tok -> print_endline (show_token tok)) tokens;
  List.fold_left
    (fun (enabled, sum) tok ->
      match tok with
      | Mul (a, b) when enabled -> (enabled, sum + (a * b))
      | Mul _ -> (enabled, sum)
      | Do -> (true, sum)
      | Dont -> (false, sum))
    (true, 0) tokens
  |> snd

let check ~tag ~to_string ~value ~expected =
  if value <> expected then
    fail "%s: expected %s, got %s" tag (to_string expected) (to_string value)
  else pfn "%s: %s" tag (to_string expected)

let () =
  let to_string = string_of_int in
  let sample1 = Base.read_file "bin/ex3-sample1.txt" in
  let sample2 = Base.read_file "bin/ex3-sample2.txt" in
  let input = Base.read_file "bin/ex3-input.txt" in
  check ~to_string ~tag:"sample pt1:" ~value:(sum1 sample1) ~expected:161;
  check ~to_string ~tag:"sample pt2:" ~value:(sum2 sample2) ~expected:48;
  check ~to_string ~tag:"input pt1:" ~value:(sum1 input) ~expected:174960292;
  check ~to_string ~tag:"input pt2:" ~value:(sum2 input) ~expected:56275602
