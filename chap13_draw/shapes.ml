open Core
open Async
open Async_graphics

type drawable = < draw : unit >

(* class square w x y =
     object (self)
       val mutable x : int = x
       method x = x
       val mutable y : int = y
       method y = y
       val mutable width = w
       method width = width
       method draw = fill_rect x y width width

       method private contains x' y' =
         x <= x' && x' <= x + width && y <= y' && y' <= y + width

       method on_click ?start ?stop f =
         on_click ?start ?stop (fun ev ->
             if self#contains ev.mouse_x ev.mouse_y then f ev.mouse_x ev.mouse_y)
     end

   class circle r x y =
     object (self)
       val mutable x : int = x
       method x = x
       val mutable y : int = y
       method y = y
       val mutable radius = r
       method radius = radius
       method draw = fill_circle x y radius

       method private contains x' y' =
         let dx = x' - x in
         let dy = y' - y in
         (dx * dx) + (dy * dy) <= radius * radius

       method on_click ?start ?stop f =
         on_click ?start ?stop (fun ev ->
             if self#contains ev.mouse_x ev.mouse_y then f ev.mouse_x ev.mouse_y)
     end *)

class virtual shape x y =
  object (self)
    method virtual private contains : int -> int -> bool
    val mutable x : int = x
    method x = x
    val mutable y : int = y
    method y = y

    method on_click ?start ?stop f =
      on_click ?start ?stop (fun ev ->
          if self#contains ev.mouse_x ev.mouse_y then f ev.mouse_x ev.mouse_y)

    method on_mousedown ?start ?stop f =
      on_mousedown ?start ?stop (fun ev ->
          if self#contains ev.mouse_x ev.mouse_y then f ev.mouse_x ev.mouse_y)
  end

class square w x y =
  object
    inherit shape x y
    val mutable width = w
    method width = width
    method draw = fill_rect x y width width

    method private contains x' y' =
      x <= x' && x' <= x + width && y <= y' && y' <= y + width
  end

class circle r x y =
  object
    inherit shape x y
    val mutable radius = r
    method radius = radius
    method draw = fill_circle x y radius

    method private contains x' y' =
      let dx = x' - x in
      let dy = y' - y in
      (dx * dx) + (dy * dy) <= radius * radius
  end

(* You can execute expressions during the instantiation of a class by placing them before the object expression or in the initial value of a field:   s *)
class obj x =
  let () = Stdio.printf "Creating obj %d\n" x in
  object
    val field =
      Stdio.printf "Initializing field\n";
      x
  end

let o = new obj 3

(* An initializer is an expression that will be executed during instantiation but after the object has been created. *)
class growing_circle r x y =
  object (self)
    inherit circle r x y
    initializer self#on_click (fun _x _y -> radius <- radius * 2)
  end

(* In any case, if you’re programming with objects, there’s one general pattern for multiple inheritance that is both useful and reasonably simple: the mixin pattern. Generically, a mixin is just a virtual class that implements a feature based on another one. If you have a class that implements methods A, and you have a mixin M that provides methods B from A, then you can inherit from M—“mixing” it in—to get features B. *)
class virtual draggable =
  object (self)
    method virtual on_mousedown
        : ?start:unit Deferred.t ->
          ?stop:unit Deferred.t ->
          (int -> int -> unit) ->
          unit

    val virtual mutable x : int
    val virtual mutable y : int
    val mutable dragging = false
    method dragging = dragging

    initializer
      self#on_mousedown (fun mouse_x mouse_y ->
          let offset_x = x - mouse_x in
          let offset_y = y - mouse_y in
          let mouse_up = Ivar.create () in
          let stop = Ivar.read mouse_up in
          dragging <- true;
          on_mouseup ~stop (fun _ ->
              Ivar.fill mouse_up ();
              dragging <- false);
          on_mousemove ~stop (fun ev ->
              x <- ev.mouse_x + offset_x;
              y <- ev.mouse_y + offset_y))
  end

class small_square =
  object
    inherit square 20 40 40
    inherit draggable
  end

class virtual animated span =
  object (self)
    method virtual on_click
        : ?start:unit Deferred.t ->
          ?stop:unit Deferred.t ->
          (int -> int -> unit) ->
          unit

    val mutable updates : (int -> unit) list = []
    val mutable step = 0
    val mutable running = false
    method running = running

    method animate =
      step <- 0;
      running <- true;
      let stop = Clock.after span >>| fun () -> running <- false in
      Clock.every ~stop
        (Time.Span.of_sec (1.0 /. 24.0))
        (fun () ->
          step <- step + 1;
          List.iter ~f:(fun f -> f step) updates)

    initializer
      self#on_click (fun _x _y -> if not self#running then self#animate)
  end

class my_circle =
  object
    inherit circle 20 50 50
    inherit animated Time.Span.second
    initializer updates <- [ (fun _ -> x <- x + 5) ]
  end

class virtual linear x' y' =
  object
    val virtual mutable updates : (int -> unit) list
    val virtual mutable x : int
    val virtual mutable y : int

    initializer
      let update _ =
        x <- x + x';
        y <- y + y'
      in
      updates <- update :: updates
  end

let pi = Float.atan 1.0 *. 4.0

class virtual harmonic offset x' y' =
  object
    val virtual mutable updates : (int -> unit) list
    val virtual mutable x : int
    val virtual mutable y : int

    initializer
      let update step =
        let m = Float.sin (offset +. (Float.of_int step *. (pi /. 64.))) in
        let x' = Float.to_int (m *. Float.of_int x') in
        let y' = Float.to_int (m *. Float.of_int y') in
        x <- x + x';
        y <- y + y'
      in
      updates <- update :: updates
  end

class my_square x y =
  object
    inherit square 40 x y
    inherit draggable
    inherit animated (Time.Span.of_int_sec 5)
    inherit linear 5 0
    inherit harmonic 0.0 7 ~-10
  end

let my_circle =
  object
    inherit circle 30 250 250
    inherit animated Time.Span.minute
    inherit harmonic 0.0 10 0
    inherit harmonic (pi /. 2.0) 0 10
  end

let main () =
  let shapes =
    [
      (my_circle :> drawable);
      (new my_square 50 350 :> drawable);
      (new my_square 50 200 :> drawable);
      (new growing_circle 20 70 70 :> drawable);
    ]
  in
  let repaint () =
    clear_graph ();
    List.iter ~f:(fun s -> s#draw) shapes;
    synchronize ()
  in
  open_graph "";
  auto_synchronize false;
  Clock.every (Time.Span.of_sec (1.0 /. 24.0)) repaint

let () = never_returns (Scheduler.go_main ~main ())
