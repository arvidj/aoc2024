open Aoc2024

let diff l l' =
  let diff = l - l' in
  if diff >= 1 && diff <= 3 then `Incr
  else if diff >= -3 && diff <= -1 then `Decr
  else `Unsafe

let safe levels =
  match levels with
  | l :: l' :: ls ->
      let direction = diff l l' in
      let rec is_safe ~idx last = function
        | [] -> true
        | l :: ls -> (
            match diff last l with
            | `Unsafe -> false
            | direction' ->
                if direction' = direction then is_safe ~idx:(idx + 1) l ls
                else false)
      in
      is_safe ~idx:1 l (l' :: ls)
  | [ _ ] -> true
  | [] -> true

let () =
  match Sys.argv with
  | [| _; path |] ->
      let n_safe_reports =
        Base.fold_lines ~path 0 @@ fun n_safe_reports line ->
        let levels =
          line |> String.split_on_char ' ' |> List.map int_of_string
        in
        n_safe_reports + if safe levels then 1 else 0
      in
      pfn "Aoc2024 ex2.a) Safe reports: %d" n_safe_reports
  | [| _; "--part2"; path |] ->
      let n_safe_reports =
        Base.fold_lines ~path 0 @@ fun n_safe_reports line ->
        let levels =
          line |> String.split_on_char ' ' |> List.map int_of_string
        in
        let is_safe =
          safe levels
          || List.exists
               (fun idx -> safe (list_remove idx levels))
               (0 -- List.length levels)
        in
        n_safe_reports + if is_safe then 1 else 0
      in
      pfn "Aoc2024 ex2.b) Safe reports with dampener: %d" n_safe_reports
  | _ ->
      Printf.eprintf "Usage: %s [--part2] PATH_INPUT" Sys.argv.(0);
      exit 1
