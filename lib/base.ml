(* We let some exceptions propagate. They are caught when running tests and
   logged as errors. We want to print a human-readable version of those errors.
   Exceptions that should be caught, such as [Not_found] or [End_of_file],
   do not need a human-readable version. *)
let () =
  Printexc.register_printer @@ function
  | Failure error -> Some error
  | Sys_error error -> Some error
  | _ -> None

let fail fmt = Format.ksprintf failwith fmt
let sf = Printf.sprintf
let pfn fmt = Printf.ksprintf print_endline fmt
let efn fmt = Printf.ksprintf prerr_endline fmt
let do_debug = Sys.getenv_opt "DEBUG" |> Option.is_some

let dbg fmt =
  Printf.ksprintf (fun s -> if do_debug then prerr_endline s else ()) fmt

(* range a b is [a ... b - 1] *)
let range (a : int) (b : int) : int list =
  if b - a < 0 then [] else List.init (b - a) (fun i -> a + i)

let ( -- ) = range

let with_file_in path f =
  let channel = open_in path in
  try f channel
  with x ->
    close_in channel;
    raise x

let read_file filename =
  with_file_in filename @@ fun ch ->
  let buffer = Buffer.create 512 in
  let bytes = Bytes.create 512 in
  let rec loop () =
    let len = input ch bytes 0 512 in
    if len > 0 then (
      Buffer.add_subbytes buffer bytes 0 len;
      loop ())
  in
  loop ();
  Buffer.contents buffer

let iter_lines path f =
  with_file_in path @@ fun channel ->
  try
    while true do
      f (input_line channel)
    done
  with End_of_file ->
    close_in channel;
    ()

let lines_seq (path : string) (f : string Seq.t -> unit) : unit =
  with_file_in path @@ fun channel ->
  let rec aux () : string Seq.t =
    try
      let line = input_line channel in
      Seq.cons line (aux ())
    with End_of_file ->
      close_in channel;
      Seq.empty
  in
  f (aux ())

let rec repeat (n : int) (f : unit -> unit) : unit =
  if n < 0 then raise (Invalid_argument "[repeat]");
  if n = 0 then ()
  else (
    f ();
    repeat (n - 1) f)

let fold_lines ~(path : string) (acc : 'a) (f : 'a -> string -> 'a) : 'a =
  with_file_in path @@ fun channel ->
  let rec aux acc =
    try
      let acc' = f acc (input_line channel) in
      aux acc'
    with End_of_file ->
      close_in channel;
      acc
  in
  aux acc

let get_lines ~(path : string) : string list =
  fold_lines ~path [] (fun acc line -> line :: acc) |> List.rev

let iter_lines3 path f =
  with_file_in path @@ fun channel ->
  try
    while true do
      let a = input_line channel in
      try
        let b = input_line channel in
        let c = input_line channel in
        f (a, b, c)
      with End_of_file ->
        close_in channel;
        raise
          (Invalid_argument
             (sf "[iter_lines3] lines in file %s is not tripled" path))
    done
  with End_of_file ->
    close_in channel;
    ()

let sum lst = List.fold_left ( + ) 0 lst
let min a b = if a < b then a else b
let max a b = if a > b then a else b

let list_take n xs =
  let len = List.length xs in
  if n < 0 || n > len then
    raise (Invalid_argument "[list_take] n < 0 || n > len")
  else
    let rec aux acc n xs =
      if n = 0 then List.rev acc
      else
        match xs with
        | x :: xs -> aux (x :: acc) (n - 1) xs
        | [] -> failwith "list_take.aux: impossible"
    in
    aux [] n xs

let%expect_test "test list_take" =
  let pp_int_list ls =
    Printf.printf "[%s]\n" (String.concat ", " (List.map string_of_int ls))
  in
  let test n ls =
    try pp_int_list (list_take n ls) with
    | Invalid_argument e -> Printf.printf "invalid_arg: %s" e
    | Failure e -> Printf.printf "error: %s" e
  in
  test 1 [];
  [%expect {| invalid_arg: [list_take] n < 0 || n > len |}];
  test 1 [ 1; 2; 3 ];
  [%expect {| [1] |}];
  test 4 [ 1; 2; 3 ];
  [%expect {| invalid_arg: [list_take] n < 0 || n > len |}];
  test 0 [ 1; 2; 3 ];
  [%expect {| [] |}];
  test 2 [ 1; 2; 3 ];
  [%expect {| [1, 2] |}];
  test 3 [ 1; 2; 3 ];
  [%expect {| [1, 2, 3] |}]

let list_remove idx xs =
  if idx < 0 || idx > List.length xs - 1 then
    raise (Invalid_argument "[list_remove]: idx < 0 || idx > len - 1");
  let rec aux idx acc xs =
    if idx = 0 then List.rev acc @ List.tl xs
    else
      match xs with
      | x :: xs -> aux (idx - 1) (x :: acc) xs
      | [] -> failwith "list_remove: unexpected exhaust"
  in
  aux idx [] xs

let%expect_test "test list_remove" =
  let pp_int_list ls =
    Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_int ls))
  in
  let test n ls =
    try pp_int_list (list_remove n ls) with
    | Invalid_argument e -> Printf.printf "invalid_arg: %s" e
    | Failure e -> Printf.printf "error: %s" e
  in
  test 1 [];
  [%expect {| invalid_arg: [list_remove]: idx < 0 || idx > len - 1 |}];
  test 1 [ 1; 2; 3 ];
  [%expect {| [1; 3] |}];
  test 4 [ 1; 2; 3 ];
  [%expect {| invalid_arg: [list_remove]: idx < 0 || idx > len - 1 |}];
  test 0 [ 1; 2; 3 ];
  [%expect {| [2; 3] |}];
  test 2 [ 1; 2; 3 ];
  [%expect {| [1; 2] |}];
  test 3 [ 1; 2; 3 ];
  [%expect {| invalid_arg: [list_remove]: idx < 0 || idx > len - 1 |}]

let insert_sorted (n : int) (lst : int list) : int list =
  let rec loop acc lst =
    match lst with
    | [] -> List.rev (n :: acc)
    | x :: xs ->
        if n < x then List.rev acc @ (n :: x :: xs) else loop (x :: acc) xs
  in
  loop [] lst

let%expect_test "test insert_sorted" =
  let pp_int_list ls =
    Printf.printf "[%s]\n" (String.concat ", " (List.map string_of_int ls))
  in
  let test n ls = pp_int_list (insert_sorted n ls) in
  test 1 [];
  [%expect {| [1] |}];
  test 1 [ 1; 2; 3 ];
  [%expect {| [1, 1, 2, 3] |}];
  test 4 [ 1; 2; 3 ];
  [%expect {| [1, 2, 3, 4] |}];
  test 0 [ 1; 2; 3 ];
  [%expect {| [0, 1, 2, 3] |}];
  test 2 [ 1; 2; 3 ];
  [%expect {| [1, 2, 2, 3] |}]

let hashtbl_update ~default tbl key f =
  Hashtbl.replace tbl key
  @@ match Hashtbl.find_opt tbl key with None -> f default | Some v -> f v

let hashtbl_get ~default tbl key =
  Hashtbl.find_opt tbl key |> Option.value ~default

let string_rev s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = len - 1 downto 0 do
    Buffer.add_char b s.[i]
  done;
  Buffer.contents b

let%expect_test "test string_rev" =
  let test s = print_endline (string_rev s) in
  test "foo";
  [%expect {| oof |}];
  test "";
  [%expect {| |}]

(* --- *)

let check ~tag ~to_string ~value ~expected =
  if value <> expected then
    fail "%s: expected %s, got %s" tag (to_string expected) (to_string value)
  else pfn "%s: %s" tag (to_string expected)

module Int_set = Set.Make (Int)

let show_int_set set =
  sf "{%s}"
    (Int_set.elements set |> List.map string_of_int |> String.concat ", ")
