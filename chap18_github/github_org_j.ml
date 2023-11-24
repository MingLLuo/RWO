(* Auto-generated from "github_org.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

type org = Github_org_t.org = {
  login : string;
  id : int;
  url : string;
  name : string option;
  blog : string option;
  email : string option;
  public_repos : int;
}

let write__string_option =
  Atdgen_runtime.Oj_run.write_option Yojson.Safe.write_string

let string_of__string_option ?(len = 1024) x =
  let ob = Buffer.create len in
  write__string_option ob x;
  Buffer.contents ob

let read__string_option p lb =
  Yojson.Safe.read_space p lb;
  match Yojson.Safe.start_any_variant p lb with
  | `Edgy_bracket -> (
      match Yojson.Safe.read_ident p lb with
      | "None" ->
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_gt p lb;
          (None : _ option)
      | "Some" ->
          Atdgen_runtime.Oj_run.read_until_field_value p lb;
          let x = Atdgen_runtime.Oj_run.read_string p lb in
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_gt p lb;
          (Some x : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)
  | `Double_quote -> (
      match Yojson.Safe.finish_string p lb with
      | "None" -> (None : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)
  | `Square_bracket -> (
      match Atdgen_runtime.Oj_run.read_string p lb with
      | "Some" ->
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_comma p lb;
          Yojson.Safe.read_space p lb;
          let x = Atdgen_runtime.Oj_run.read_string p lb in
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_rbr p lb;
          (Some x : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)

let _string_option_of_string s =
  read__string_option (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_org : _ -> org -> _ =
 fun ob (x : org) ->
  Buffer.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"login\":";
  Yojson.Safe.write_string ob x.login;
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"id\":";
  Yojson.Safe.write_int ob x.id;
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"url\":";
  Yojson.Safe.write_string ob x.url;
  (match x.name with
  | None -> ()
  | Some x ->
      if !is_first then is_first := false else Buffer.add_char ob ',';
      Buffer.add_string ob "\"name\":";
      Yojson.Safe.write_string ob x);
  (match x.blog with
  | None -> ()
  | Some x ->
      if !is_first then is_first := false else Buffer.add_char ob ',';
      Buffer.add_string ob "\"blog\":";
      Yojson.Safe.write_string ob x);
  (match x.email with
  | None -> ()
  | Some x ->
      if !is_first then is_first := false else Buffer.add_char ob ',';
      Buffer.add_string ob "\"email\":";
      Yojson.Safe.write_string ob x);
  if !is_first then is_first := false else Buffer.add_char ob ',';
  Buffer.add_string ob "\"public_repos\":";
  Yojson.Safe.write_int ob x.public_repos;
  Buffer.add_char ob '}'

let string_of_org ?(len = 1024) x =
  let ob = Buffer.create len in
  write_org ob x;
  Buffer.contents ob

let read_org p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_login = ref None in
  let field_id = ref None in
  let field_url = ref None in
  let field_name = ref None in
  let field_blog = ref None in
  let field_email = ref None in
  let field_public_repos = ref None in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s then
        invalid_arg
          (Printf.sprintf
             "out-of-bounds substring position or length: string = %S, \
              requested position = %i, requested length = %i"
             s pos len);
      match len with
      | 2 ->
          if
            String.unsafe_get s pos = 'i' && String.unsafe_get s (pos + 1) = 'd'
          then 1
          else -1
      | 3 ->
          if
            String.unsafe_get s pos = 'u'
            && String.unsafe_get s (pos + 1) = 'r'
            && String.unsafe_get s (pos + 2) = 'l'
          then 2
          else -1
      | 4 -> (
          match String.unsafe_get s pos with
          | 'b' ->
              if
                String.unsafe_get s (pos + 1) = 'l'
                && String.unsafe_get s (pos + 2) = 'o'
                && String.unsafe_get s (pos + 3) = 'g'
              then 4
              else -1
          | 'n' ->
              if
                String.unsafe_get s (pos + 1) = 'a'
                && String.unsafe_get s (pos + 2) = 'm'
                && String.unsafe_get s (pos + 3) = 'e'
              then 3
              else -1
          | _ -> -1)
      | 5 -> (
          match String.unsafe_get s pos with
          | 'e' ->
              if
                String.unsafe_get s (pos + 1) = 'm'
                && String.unsafe_get s (pos + 2) = 'a'
                && String.unsafe_get s (pos + 3) = 'i'
                && String.unsafe_get s (pos + 4) = 'l'
              then 5
              else -1
          | 'l' ->
              if
                String.unsafe_get s (pos + 1) = 'o'
                && String.unsafe_get s (pos + 2) = 'g'
                && String.unsafe_get s (pos + 3) = 'i'
                && String.unsafe_get s (pos + 4) = 'n'
              then 0
              else -1
          | _ -> -1)
      | 12 ->
          if
            String.unsafe_get s pos = 'p'
            && String.unsafe_get s (pos + 1) = 'u'
            && String.unsafe_get s (pos + 2) = 'b'
            && String.unsafe_get s (pos + 3) = 'l'
            && String.unsafe_get s (pos + 4) = 'i'
            && String.unsafe_get s (pos + 5) = 'c'
            && String.unsafe_get s (pos + 6) = '_'
            && String.unsafe_get s (pos + 7) = 'r'
            && String.unsafe_get s (pos + 8) = 'e'
            && String.unsafe_get s (pos + 9) = 'p'
            && String.unsafe_get s (pos + 10) = 'o'
            && String.unsafe_get s (pos + 11) = 's'
          then 6
          else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 -> field_login := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | 1 -> field_id := Some (Atdgen_runtime.Oj_run.read_int p lb)
    | 2 -> field_url := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | 3 ->
        if not (Yojson.Safe.read_null_if_possible p lb) then
          field_name := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | 4 ->
        if not (Yojson.Safe.read_null_if_possible p lb) then
          field_blog := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | 5 ->
        if not (Yojson.Safe.read_null_if_possible p lb) then
          field_email := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | 6 -> field_public_repos := Some (Atdgen_runtime.Oj_run.read_int p lb)
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s then
          invalid_arg
            (Printf.sprintf
               "out-of-bounds substring position or length: string = %S, \
                requested position = %i, requested length = %i"
               s pos len);
        match len with
        | 2 ->
            if
              String.unsafe_get s pos = 'i'
              && String.unsafe_get s (pos + 1) = 'd'
            then 1
            else -1
        | 3 ->
            if
              String.unsafe_get s pos = 'u'
              && String.unsafe_get s (pos + 1) = 'r'
              && String.unsafe_get s (pos + 2) = 'l'
            then 2
            else -1
        | 4 -> (
            match String.unsafe_get s pos with
            | 'b' ->
                if
                  String.unsafe_get s (pos + 1) = 'l'
                  && String.unsafe_get s (pos + 2) = 'o'
                  && String.unsafe_get s (pos + 3) = 'g'
                then 4
                else -1
            | 'n' ->
                if
                  String.unsafe_get s (pos + 1) = 'a'
                  && String.unsafe_get s (pos + 2) = 'm'
                  && String.unsafe_get s (pos + 3) = 'e'
                then 3
                else -1
            | _ -> -1)
        | 5 -> (
            match String.unsafe_get s pos with
            | 'e' ->
                if
                  String.unsafe_get s (pos + 1) = 'm'
                  && String.unsafe_get s (pos + 2) = 'a'
                  && String.unsafe_get s (pos + 3) = 'i'
                  && String.unsafe_get s (pos + 4) = 'l'
                then 5
                else -1
            | 'l' ->
                if
                  String.unsafe_get s (pos + 1) = 'o'
                  && String.unsafe_get s (pos + 2) = 'g'
                  && String.unsafe_get s (pos + 3) = 'i'
                  && String.unsafe_get s (pos + 4) = 'n'
                then 0
                else -1
            | _ -> -1)
        | 12 ->
            if
              String.unsafe_get s pos = 'p'
              && String.unsafe_get s (pos + 1) = 'u'
              && String.unsafe_get s (pos + 2) = 'b'
              && String.unsafe_get s (pos + 3) = 'l'
              && String.unsafe_get s (pos + 4) = 'i'
              && String.unsafe_get s (pos + 5) = 'c'
              && String.unsafe_get s (pos + 6) = '_'
              && String.unsafe_get s (pos + 7) = 'r'
              && String.unsafe_get s (pos + 8) = 'e'
              && String.unsafe_get s (pos + 9) = 'p'
              && String.unsafe_get s (pos + 10) = 'o'
              && String.unsafe_get s (pos + 11) = 's'
            then 6
            else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 -> field_login := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | 1 -> field_id := Some (Atdgen_runtime.Oj_run.read_int p lb)
      | 2 -> field_url := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | 3 ->
          if not (Yojson.Safe.read_null_if_possible p lb) then
            field_name := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | 4 ->
          if not (Yojson.Safe.read_null_if_possible p lb) then
            field_blog := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | 5 ->
          if not (Yojson.Safe.read_null_if_possible p lb) then
            field_email := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | 6 -> field_public_repos := Some (Atdgen_runtime.Oj_run.read_int p lb)
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    ({
       login =
         (match !field_login with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "login");
       id =
         (match !field_id with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "id");
       url =
         (match !field_url with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "url");
       name = !field_name;
       blog = !field_blog;
       email = !field_email;
       public_repos =
         (match !field_public_repos with
         | Some x -> x
         | None -> Atdgen_runtime.Oj_run.missing_field p "public_repos");
     }
      : org)

let org_of_string s =
  read_org (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
