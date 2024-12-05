let parse (ss : string list) =
  (* Map pre-reqs to numbers that must appear after them *)
  let rules : (int, Int_set.t) Hashtbl.t = Hashtbl.create 5 in
  let re_rule =
    let open Tyre in
    compile (int <&> str "|" *> int)
  in
  let rec read_rules = function
    | [] -> failwith "[part1.read_rules] unexpected end of input"
    | "" :: ss' -> ss'
    | s :: ss' -> (
        match Tyre.exec re_rule s with
        | Ok (prereq, page) ->
            hashtbl_update ~default:Int_set.empty rules prereq
              (Int_set.add page);
            read_rules ss'
        | _ -> fail "[part1.read_rules] can't parse: '%s'" s)
  in
  let updates = read_rules ss in
  dbg "Pre-reqs (%d):" (Hashtbl.length rules);
  Hashtbl.iter
    (fun page prereqs ->
      dbg "Pre-reqs of page %d: %s" page (show_int_set prereqs))
    rules;
  dbg "\nUpdates:";
  let updates =
    Fun.flip List.map updates @@ fun update_s ->
    update_s |> String.split_on_char ',' |> List.map int_of_string
    |> Array.of_list
  in
  (rules, updates)

let is_correct rules (update : int array) =
  let len = Array.length update in
  let page_idx : (int, int) Hashtbl.t = Hashtbl.create 5 in
  let rec aux i printed =
    if i >= len then None
    else
      let page = update.(i) in
      Hashtbl.replace page_idx page i;
      (* At each page, check if the *)
      let must_follow = hashtbl_get ~default:Int_set.empty rules page in
      (* Check that none of the page number that must follow this page has been printed already *)
      if Int_set.disjoint must_follow printed then
        aux (succ i) (Int_set.add page printed)
      else
        let error = Int_set.inter must_follow printed in
        (*
           error contains the set of printed pages that should've appeared before [page]

           at which index were they written? what were the lowest?
          *)
        let swap =
          let mins = function
            | x :: xs -> List.fold_left min x xs
            | [] -> raise (Invalid_argument "[mins]")
          in
          Int_set.elements error |> List.map (Hashtbl.find page_idx) |> mins
        in
        dbg
          "page %d: the set of pages that must_follow %s is not disjoint with \
           the set of printed %s, intersect: %s. swap: %d"
          page (show_int_set must_follow) (show_int_set printed)
          (show_int_set error) swap;
        Some (i, swap)
  in
  aux 0 Int_set.empty

let middle (xs : int array) =
  let len = Array.length xs in
  assert (len mod 2 = 1);
  let middle = len / 2 in
  xs.(middle)

let part1 (ss : string list) : int =
  let rules, updates = parse ss in
  sum
  @@ List.map
       (fun update ->
         match is_correct rules update with
         | Some _idx -> 0
         | None -> middle update)
       updates

let part2 (ss : string list) : int =
  let rules, updates = parse ss in
  sum
  @@ List.map
       (fun update ->
         let rec aux (idx, swap) =
           let tmp = update.(idx) in
           update.(idx) <- update.(swap);
           update.(swap) <- tmp;
           match is_correct rules update with
           | Some (idx, swap) -> aux (idx, swap)
           | None -> middle update
         in
         match is_correct rules update with
         | Some (idx, swap) -> aux (idx, swap)
         | None -> 0)
       updates

let () =
  let to_string = string_of_int in
  let sample1 = Base.get_lines ~path:"bin/ex5-sample.txt" in
  let input = Base.get_lines ~path:"bin/ex5-input.txt" in
  check ~to_string ~tag:"sample pt1:" ~value:(part1 sample1) ~expected:143;
  check ~to_string ~tag:"sample pt2:" ~value:(part2 sample1) ~expected:123;
  check ~to_string ~tag:"input pt1:" ~value:(part1 input) ~expected:6260;
  check ~to_string ~tag:"input pt1:" ~value:(part2 input) ~expected:5346
