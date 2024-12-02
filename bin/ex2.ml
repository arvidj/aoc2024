open Aoc2024

let () =
  match Sys.argv with
  | [| _; path |] ->
      let n_safe_reports =
        Base.fold_lines ~path 0 @@ fun n_safe_reports line ->
        let levels =
          line |> String.split_on_char ' ' |> List.map int_of_string
        in
        let diff l l' =
          let diff = l - l' in
          if diff >= 1 && diff <= 3 then `Incr
          else if diff >= -3 && diff <= -1 then `Decr
          else `Unsafe
        in
        let safe =
          match levels with
          | l :: l' :: ls ->
              let direction = diff l l' in
              let rec is_safe last = function
                | [] -> true
                | l :: ls -> (
                    match diff last l with
                    | `Unsafe -> false
                    | direction' -> direction' = direction && is_safe l ls)
              in
              is_safe l (l' :: ls)
          | [ _ ] -> true
          | [] -> true
        in
        if safe then 1 + n_safe_reports else n_safe_reports
      in
      pfn "Aoc2024 ex2.a) Safe reports: %d" n_safe_reports
  | [| _; "--part2"; _path |] -> assert false
  | _ ->
      Printf.eprintf "Usage: %s [--part2] PATH_INPUT" Sys.argv.(0);
      exit 1
