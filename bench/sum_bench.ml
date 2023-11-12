open Core
open Core_bench

let rec sum_if l =
  if List.is_empty l then 0 else List.hd_exn l + sum_if (List.tl_exn l)

let rec sum l = match l with [] -> 0 | hd :: tl -> hd + sum tl;;

let numbers = List.range 0 1000 in
[
  Bench.Test.create ~name:"sum_if" (fun () -> sum_if numbers);
  Bench.Test.create ~name:"sum" (fun () -> sum numbers);
]
|> Bench.bench
