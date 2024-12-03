type rex = string * Re.re

let rex ?opts r = (r, Re.compile (Re.Perl.re ?opts r))
let rexf ?opts fmt = Printf.ksprintf (rex ?opts) fmt
let show = fst
let ( =~ ) s (_, r) = Re.execp r s
let ( =~! ) s (_, r) = not (Re.execp r s)

let get_group group index =
  match Re.Group.get group index with
  | exception Not_found ->
      invalid_arg
        "regular expression has not enough capture groups for its usage, did \
         you forget parentheses?"
  | value -> value

let ( =~* ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group -> Some (get_group group 1)

let ( =~** ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group -> Some (get_group group 1, get_group group 2)

let ( =~*** ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group -> Some (get_group group 1, get_group group 2, get_group group 3)

let ( =~**** ) s (_, r) =
  match Re.exec_opt r s with
  | None -> None
  | Some group ->
      Some
        ( get_group group 1,
          get_group group 2,
          get_group group 3,
          get_group group 4 )

let matches s (_, r) = Re.all r s |> List.map (fun g -> get_group g 1)

let replace_string ?pos ?len ?all (_, r) ~by s =
  Re.replace_string ?pos ?len ?all r ~by s
