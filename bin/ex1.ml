open Aoc2024

let read path =
  Base.fold_lines ~path ([], []) (fun (left, right) line ->
      Scanf.sscanf line "%d   %d" @@ fun l r -> (l :: left, r :: right))

let () =
  match Sys.argv with
  | [| _; path |] ->
      let left, right = read path in
      let left, right =
        (List.sort Int.compare left, List.sort Int.compare right)
      in
      let diff_sum =
        List.fold_left2
          (fun diff_sum l r -> diff_sum + abs (l - r))
          0 left right
      in
      pfn "Aoc2024 ex1.b) Diff sum: %d" diff_sum
  | [| _; "--part2"; path |] ->
      let left, right = read path in
      (* [occ v] Number of occurrences of [v] in right list. *)
      let occ : int -> int =
        let occ : (int, int) Hashtbl.t = Hashtbl.create (List.length right) in
        List.iter (fun d -> hashtbl_update ~default:0 occ d succ) right;
        fun d -> hashtbl_get ~default:0 occ d
      in
      let similarity_score =
        List.fold_left (fun acc l -> acc + (l * occ l)) 0 left
      in
      pfn "Aoc2024 ex1.a) Similarity score: %d" similarity_score
  | _ ->
      Printf.eprintf "Usage: %s [--part2] PATH_INPUT" Sys.argv.(0);
      exit 1
