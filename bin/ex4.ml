open Aoc2024

let count_occ (ss : string list) : int =
  let re = Rex.rex "(XMAS)" in
  sum (List.map (fun s -> Rex.(matches s re |> List.length)) ss)

let part1 (s : string list) : int =
  let rect = Ex4.of_strings s in
  (* Horizontal traversal *)
  let h = Ex4.(rect |> to_strings |> count_occ) in
  (* Reversed horizontal traversal *)
  let hr = Ex4.(rect |> reverseh |> to_strings |> count_occ) in
  (* Vertical traversal *)
  let v = Ex4.(rect |> transpose |> to_strings |> count_occ) in
  (* Reversed Vertical traversal *)
  let vr = Ex4.(rect |> transpose |> reverseh |> to_strings |> count_occ) in
  (* Top-left - bottom-right traversal *)
  let tlbr = Ex4.(rect |> diagonals |> count_occ) in
  (* Reversed Top-left - bottom-right traversal *)
  let tlbrr = Ex4.(rect |> diagonals |> reverse_strings |> count_occ) in
  (* Top-right - bottom-left traversal *)
  let trbl = Ex4.(rect |> diagonals_aux |> count_occ) in
  (* Reversed Top-right - bottom-left traversal *)
  let trblr = Ex4.(rect |> diagonals_aux |> reverse_strings |> count_occ) in
  let total = h + hr + v + vr + tlbr + tlbrr + trbl + trblr in
  let pp =
    [
      ("→", h);
      ("←", hr);
      ("↓", v);
      ("↑", vr);
      ("↘", tlbr);
      ("↖", tlbrr);
      ("↙", trbl);
      ("↗", trblr);
      ("total", total);
    ]
  in
  print_endline
    (String.concat "\t"
    @@ List.map (fun (dir, count) -> sf "%s: %d" dir count) pp);
  total

let part2 (s : string list) : int =
  let Ex4.{ chars; dimx; dimy } = Ex4.of_strings s in
  (* Search for each A. Then check we got MAS / SAM around it. *)
  let occ = ref 0 in
  for x = 1 to dimx - 2 do
    for y = 1 to dimy - 2 do
      if chars.(x).(y) = 'A' then
        let tl_br =
          [ chars.(x - 1).(y - 1); chars.(x).(y); chars.(x + 1).(y + 1) ]
        in
        let tr_bl =
          [ chars.(x + 1).(y - 1); chars.(x).(y); chars.(x - 1).(y + 1) ]
        in
        let is_mas cs = cs = [ 'M'; 'A'; 'S' ] || cs = [ 'S'; 'A'; 'M' ] in
        if is_mas tl_br && is_mas tr_bl then incr occ
    done
  done;
  !occ

let () =
  let to_string = string_of_int in
  let sample1 = Base.get_lines ~path:"bin/ex4-sample.txt" in
  let input = Base.get_lines ~path:"bin/ex4-input.txt" in
  check ~to_string ~tag:"sample pt1:" ~value:(part1 sample1) ~expected:18;
  check ~to_string ~tag:"input pt1:" ~value:(part1 input) ~expected:2493;
  check ~to_string ~tag:"sample pt2:" ~value:(part2 sample1) ~expected:9;
  check ~to_string ~tag:"input pt2:" ~value:(part2 input) ~expected:1890
