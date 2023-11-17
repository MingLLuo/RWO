open Core

(* let () =
   Out_channel.output_string stdout "Pick a timezone: ";
   Out_channel.flush stdout;
   match In_channel.(input_line stdin) with
   | None -> failwith "No timezone provided"
   | Some zone_string ->
       let zone = Time_unix.Zone.find_exn zone_string in
       let time_string = Time.to_string_abs (Time.now ()) ~zone in
       Out_channel.output_string stdout
         (String.concat
            [
              "The time in ";
              Time_unix.Zone.to_string zone;
              " is ";
              time_string;
              ".\n";
            ]);
       Out_channel.flush stdout *)

(* %s, for including a string, and %! which causes printf to flush the channel. *)
let () =
  printf "Pick a timezone: %!";
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "No timezone provided"
  | Some zone_string ->
      let zone = Time_unix.Zone.find_exn zone_string in
      let time_string = Time.to_string_abs (Time.now ()) ~zone in
      printf "The time in %s is %s.\n%!"
        (Time_unix.Zone.to_string zone)
        time_string
