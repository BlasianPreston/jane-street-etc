open! Core
open Import

type t =
  { mutable positions : Position.t Symbol.Map.t
  ; mutable highest_buy : Price.t Symbol.Map.t
  ; mutable lowest_sell : Price.t Symbol.Map.t
  ; mutable fair_value : Price.t Symbol.Map.t
  ; order_id_generator : Order_id_generator.t
  ; exchange_driver : (Exchange_driver.t[@sexp.opaque])
  ; mutable initialize_adr : bool
  }
[@@deriving sexp]

let create exchange_driver =
  { positions = Symbol.Map.empty
  ; highest_buy =
      Symbol.Map.of_alist_exn [ Symbol.bond, Price.of_int_exn 999 ]
  ; lowest_sell =
      Symbol.Map.of_alist_exn [ Symbol.bond, Price.of_int_exn 1001 ]
  ; fair_value =
      Symbol.Map.of_alist_exn [ Symbol.bond, Price.of_int_exn 1000 ]
  ; order_id_generator = Order_id_generator.create ()
  ; exchange_driver
  ; initialize_adr = false
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

let on_book t (book : Exchange_message.Book.t) =
  let symbol = book.symbol in
  let book = book.book in
  let buy_lst = book.buy in
  let sell_lst = book.sell in
  let best_buy_price = List.hd buy_lst in
  let best_sell_price = List.hd sell_lst in
  t.highest_buy
  <- Map.change t.highest_buy symbol ~f:(fun _ ->
       match best_buy_price with
       | None -> None
       | Some (price, _) -> Some price);
  t.lowest_sell
  <- Map.change t.lowest_sell symbol ~f:(fun _ ->
       match best_sell_price with
       | None -> None
       | Some (price, _) -> Some price);
  t.fair_value
  <- Map.change t.lowest_sell symbol ~f:(fun _ ->
       match best_buy_price, best_sell_price with
       | None, _ | _, None -> None
       | Some (buy_price, buy_size), Some (sell_price, sell_size) ->
         let buy_price_as_int = Price.to_int buy_price in
         let buy_size_as_int = Size.to_int buy_size in
         let sell_price_as_int = Price.to_int sell_price in
         let sell_size_as_int = Size.to_int sell_size in
         Some
           (Price.of_int_exn
              (((buy_price_as_int * buy_size_as_int)
                + (sell_price_as_int * sell_size_as_int))
               / (buy_size_as_int + sell_size_as_int))))
;;
