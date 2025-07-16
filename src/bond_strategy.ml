open! Core
open Async
open Import

let initialize_bond_orders (state : State.t) =
  let positions = Map.find_exn state.positions Symbol.bond in
  let position_as_int = Position.to_int positions in
  let amount_to_buy = 100 - position_as_int in
  let amount_to_sell = 200 - amount_to_buy in
  let order_id = Order_id_generator.next_id state.order_id_generator in
  Exchange_driver.add_order
    state.exchange_driver
    ~order_id
    ~symbol:Symbol.bond
    ~dir:Buy
    ~price:(Price.of_int_exn (Price.to_int (Map.find_exn state.highest_buy Symbol.bond)))
    ~size:(Size.of_int_exn amount_to_buy)
  |> don't_wait_for;
  let order_id = Order_id_generator.next_id state.order_id_generator in
  Exchange_driver.add_order
    state.exchange_driver
    ~order_id
    ~symbol:Symbol.bond
    ~dir:Sell
    ~price:(Price.of_int_exn (Price.to_int (Map.find_exn state.lowest_sell Symbol.bond)))
    ~size:(Size.of_int_exn amount_to_sell)
  |> don't_wait_for
;;

let adjust_bond_orders (state : State.t) (order : Exchange_message.Fill.t) =
  let symbol = order.symbol in
  if Symbol.equal symbol Symbol.bond then
  let size = Size.to_int order.size in
  match order.dir with
  | Buy ->
    let order_id = Order_id_generator.next_id state.order_id_generator in
    Exchange_driver.add_order
      state.exchange_driver
      ~order_id
      ~symbol:Symbol.bond
      ~dir:Sell
      ~price:(Price.of_int_exn (Price.to_int (Map.find_exn state.lowest_sell Symbol.bond)))
      ~size:(Size.of_int_exn size)
    |> don't_wait_for
  | Sell ->
    let order_id = Order_id_generator.next_id state.order_id_generator in
    Exchange_driver.add_order
      state.exchange_driver
      ~order_id
      ~symbol:Symbol.bond
      ~dir:Buy
      ~price:(Price.of_int_exn (Price.to_int (Map.find_exn state.highest_buy Symbol.bond)))
      ~size:(Size.of_int_exn size)
    |> don't_wait_for
;;
