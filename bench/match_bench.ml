open Core_bench

let plus_one_match x =
  match x with
  | 0 ->
      1
  | 1 ->
      2
  | 2 ->
      3
  | 3 ->
      4
  | 4 ->
      5
  | 5 ->
      6
  | _ ->
      x + 1

let plus_one_if x =
  if x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else if x = 3 then 4
  else if x = 4 then 5
  else if x = 5 then 6
  else x + 1

let () =
  [ Bench.Test.create ~name:"plus_one_match" (fun () -> plus_one_match 10)
  ; Bench.Test.create ~name:"plus_one_if" (fun () -> plus_one_if 10) ]
  |> Bench.bench
