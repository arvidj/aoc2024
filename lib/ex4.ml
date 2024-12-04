open Base

type rect = { chars : char array array; dimx : int; dimy : int }

let pp_rect fmt rect =
  for x = 0 to rect.dimx - 1 do
    for y = 0 to rect.dimy - 1 do
      Format.pp_print_char fmt rect.chars.(x).(y);
      Format.pp_print_char fmt ' '
    done;
    Format.pp_print_newline fmt ()
  done

let create ~dimx ~dimy : rect =
  if dimx <= 0 || dimy <= 0 then raise (Invalid_argument "[create]");
  { chars = Array.make_matrix dimx dimy ' '; dimx; dimy }

let init_matrix dimx dimy f =
  Array.init dimx (fun x -> Array.init dimy (fun y -> f x y))

let init ~dimx ~dimy f : rect = { chars = init_matrix dimx dimy f; dimx; dimy }

let of_strings ss : rect =
  let dimx = List.length ss in
  let dimy =
    match List.map String.length ss |> List.sort_uniq Int.compare with
    | [ dimy ] -> dimy
    | _ -> failwith "[occ]: input is not rectangular"
  in
  let chars : char array array =
    List.map (fun s -> s |> String.to_seq |> Array.of_seq) ss |> Array.of_list
  in
  { dimx; dimy; chars }

let to_strings : rect -> string list =
 fun rect ->
  List.map
    (fun char_array -> String.of_seq (Array.to_seq char_array))
    (Array.to_list rect.chars)

(* let str_occ (needle : string) (hay : string) : int = *)

let%expect_test "test diags" =
  pp_rect Format.std_formatter (of_strings [ "abc"; "cde" ]);
  [%expect {|
    a b c
    c d e |}]

type transform = rect -> rect

let transpose : transform =
 fun rect ->
  init ~dimx:rect.dimy ~dimy:rect.dimx (fun x y -> rect.chars.(y).(x))

let reverseh : transform =
 fun { chars; dimx; dimy } ->
  init ~dimx ~dimy (fun x y -> chars.(x).(dimy - y - 1))

let diagonals : rect -> string list =
 fun { chars; dimx; dimy } ->
  let walk_diag x y =
    String.init
      (min (dimx - x) (dimy - y))
      (fun idx -> chars.(x + idx).(y + idx))
  in
  let diags =
    (* From lower-left up to origo *)
    List.init (dimx - 1) (fun idx -> walk_diag (idx + 1) 0)
    (* origo *)
    @ [ walk_diag 0 0 ]
    @ (* From origo to upper-right *)
    List.init (dimy - 1) (fun idx -> walk_diag 0 (idx + 1))
  in
  diags

let reverse_strings ss = List.map string_rev ss
let diagonals_aux : rect -> string list = fun r -> diagonals (reverseh r)

let%expect_test "test diags" =
  let rect =
    let dimx, dimy = (3, 2) in
    init ~dimx ~dimy (fun x y -> Char.(code '0' + (1 + (x * dimy) + y) |> chr))
  in
  let pp_diags diags =
    Fun.flip List.iter diags @@ fun row ->
    String.iter (fun c -> Printf.printf "%c " c) row;
    Printf.printf "\n"
  in
  pp_diags (diagonals rect);
  [%expect {|
    3 6
    5
    1 4
    2 |}];
  pp_diags (diagonals_aux rect);
  [%expect {|
    4 5
    6
    2 3
    1 |}]

let%expect_test "test transpose" =
  let rect =
    let dimx, dimy = (3, 2) in
    init ~dimx ~dimy (fun x y -> Char.(code '0' + (1 + (x * dimy) + y) |> chr))
  in
  let pp_rect r = pp_rect Format.std_formatter r in
  pp_rect rect;
  [%expect {|
    1 2
    3 4
    5 6 |}];
  pp_rect (transpose rect);
  [%expect {|
    1 3 5
    2 4 6 |}];
  pfn "Identity: %b" (rect = transpose (transpose rect));
  [%expect {|
    Identity: true |}];
  pp_rect (reverseh rect);
  [%expect {|
    2 1
    4 3
    6 5 |}]
