let part1 (ss : string list) : int =
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
  List.fold_left
    (fun middle_page_sum update_s ->
      let update =
        update_s |> String.split_on_char ',' |> List.map int_of_string
      in
      let is_correct =
        let rec aux printed = function
          | [] -> true
          | page :: pages ->
              (* At each page, check if the *)
              let must_follow = hashtbl_get ~default:Int_set.empty rules page in
              (* Check that none of the page number that must follow this page has been printed already *)
              if Int_set.disjoint must_follow printed then
                aux (Int_set.add page printed) pages
              else (
                dbg
                  "page %d: the set of pages that must_follow %s is not \
                   disjoint with the set of printed %s, intersect: %s"
                  page (show_int_set must_follow) (show_int_set printed)
                  (show_int_set (Int_set.inter must_follow printed));
                false)
        in
        aux Int_set.empty update
      in
      dbg "%s: is correct: %b" update_s is_correct;
      if is_correct then
        let middle =
          let len = List.length update in
          assert (len mod 2 = 1);
          let middle = List.length update / 2 in
          List.nth update middle
        in
        middle + middle_page_sum
      else middle_page_sum)
    0 updates

let _part2 (_ss : string list) : int = assert false

let () =
  let to_string = string_of_int in
  let sample1 = Base.get_lines ~path:"bin/ex5-sample.txt" in
  let input = Base.get_lines ~path:"bin/ex5-input.txt" in
  check ~to_string ~tag:"sample pt1:" ~value:(part1 sample1) ~expected:143;
  check ~to_string ~tag:"input pt1:" ~value:(part1 input) ~expected:6260
