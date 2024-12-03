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

module Parse_tyre = struct
  open Tyre

  let conv' f = conv f (fun _ -> failwith "[conv']")
  let ( <.> ) a b = a <|> b |> conv' (function `Left a | `Right a -> a)

  let mul =
    str "mul(" *> int
    <&> str "," *> int
    <* str ")"
    |> conv' (fun (d, d') -> Mul (d, d'))

  let do_ = str "do()" |> conv' (fun _ -> Do)
  let dont = str "don't()" |> conv' (fun _ -> Dont)
  let tok = mul <.> do_ <.> dont

  let parse input =
    match all (compile tok) input with
    | Ok tokens -> tokens
    | Error _ -> failwith "[parse_tyre]"
end
