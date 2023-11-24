open Base
open Stdio
(* open Soup *)

let%test "rev" = List.equal Int.equal (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]
(* let%test_unit "rev" = [%test_eq: int list] (List.rev [ 3; 2; 1 ]) [ 3; 2; 1 ] *)

let%expect_test "trivial" =
  print_endline "Hello World!";
  [%expect {| Hello World! |}]

let%expect_test "multi-block" =
  print_endline "Hello";
  [%expect {| Hello |}];
  print_endline "World!";
  [%expect {| World! |}]

let%expect_test _ =
  print_s [%sexp (List.rev [ 3; 2; 1 ] : int list)];
  [%expect {| (1 2 3) |}]

let get_href_hosts soup =
  Soup.select "a[href]" soup |> Soup.to_list
  |> List.map ~f:(Soup.R.attribute "href")
  |> Set.of_list (module String)

let%expect_test _ =
  let example_html =
    {|
      <html>
        Some random <b>text</b> with a
        <a href="http://ocaml.org/base">link</a>.
        And here's another
        <a href="http://github.com/ocaml/dune">link</a>.
        And here is <a>link</a> with no href.
      </html>|}
  in
  let soup = Soup.parse example_html in
  let hrefs = get_href_hosts soup in
  print_s [%sexp (hrefs : Set.M(String).t)]


  (* Quoted String *)
  let x = {|This is a literal quote: "|};;
  let y = {xxx|This is how you quote a {|quoted string|}|xxx};;
