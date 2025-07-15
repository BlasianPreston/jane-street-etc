open! Core
open Import

type t =
  { mutable positions : Position.t Symbol.Map.t
  ; mutable fair_values : Price.t Symbol.Map.t
  ; order_id_generator : Order_id_generator.t
  ; exchange_driver : (Exchange_driver.t [@sexp.opaque])
  }
[@@deriving sexp]

let create exchange_driver =
  { positions = Symbol.Map.empty
  ; fair_values =
      Symbol.Map.of_alist_exn [ Symbol.bond, Price.of_int_exn 1000 ]
  ; order_id_generator = Order_id_generator.create ()
  ; exchange_driver
  }
;;

let next_id t = Order_id_generator.next_id t.order_id_generator

let on_hello t hello_message =
  let positions =
    List.fold
      hello_message
      ~init:t.positions
      ~f:(fun positions (symbol, position) ->
        Map.add_exn positions ~key:symbol ~data:position)
  in
  t.positions <- positions
;;

let on_fill t (fill : Exchange_message.Fill.t) =
  t.positions
  <- Map.change t.positions fill.symbol ~f:(function
       | None -> failwith "No position exists"
       | Some position ->
         let position_of_int = Position.to_int position in
         let size_as_int = Size.to_int fill.size in
         (match fill.dir with
          | Buy -> Some (Position.of_int_exn (position_of_int + size_as_int))
          | Sell ->
            Some (Position.of_int_exn (position_of_int - size_as_int))))
;;
