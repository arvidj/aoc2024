open Base

type token = Mul of int * int | Do | Dont
[@@deriving show { with_path = false }]

let parse_re input =
  let re =
    "("
    ^ String.concat "|" [ "mul\\(\\d+,\\d+\\)"; "do\\(\\)"; "don't\\(\\)" ]
    ^ ")"
  in
  let matches = Rex.(matches input (rex re)) in
  Fun.flip List.map matches @@ fun s ->
  match s with
  | "do()" -> Do
  | "don't()" -> Dont
  | s -> (
      match Rex.(s =~** rex "mul\\((\\d+),(\\d+)\\)") with
      | Some (s1, s2) -> Mul (int_of_string s1, int_of_string s2)
      | None -> fail "[parse] can't parse" s)
