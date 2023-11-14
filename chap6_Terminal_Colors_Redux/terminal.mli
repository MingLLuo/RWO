type basic_color =
  [ `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]

type color =
  [ `Basic of basic_color * [ `Bold | `Regular ]
  | `Gray of int
  | `RGB of int * int * int ]

type extended_color =
  [ `Basic of basic_color * [ `Bold | `Regular ]
  | `Gray of int
  | `RGB of int * int * int
  | `RGBA of int * int * int * int ]

val basic_color_to_int :
  [< `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ] ->
  int

val color_by_number : int -> string -> string

val color_to_int :
  [< `Basic of
     [< `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]
     * [< `Bold | `Regular ]
  | `Gray of int
  | `RGB of int * int * int ] ->
  int

val color_print :
  [< `Basic of
     [< `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]
     * [< `Bold | `Regular ]
  | `Gray of int
  | `RGB of int * int * int ] ->
  string ->
  unit

val extended_color_to_int : extended_color -> int
val extended_color_to_int : extended_color -> int
