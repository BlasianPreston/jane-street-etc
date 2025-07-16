open! Core
open Async
open Import

let initialize_adr_orders (state : State.t) =
  let positions = Map.find_exn state.positions Symbol.bond in
  let position_as_int = Position.to_int positions in
  let amount_to_buy = 10 - position_as_int in
  let amount_to_sell = 20 - amount_to_buy in
  let highest_buy = Map.find_exn state.highest_buy Symbol.vale in
  let lowest_sell = Map.find_exn state.lowest_sell Symbol.vale in
  let order_id = Order_id_generator.next_id state.order_id_generator in
  Exchange_driver.add_order
    state.exchange_driver
    ~order_id
    ~symbol:Symbol.vale
    ~dir:Buy
    ~price:(Price.of_int_exn ((Price.to_int highest_buy) + 1) )
    ~size:(Size.of_int_exn amount_to_buy)
  |> don't_wait_for;
  let order_id = Order_id_generator.next_id state.order_id_generator in
  Exchange_driver.add_order
    state.exchange_driver
    ~order_id
    ~symbol:Symbol.vale
    ~dir:Sell
    ~price:(Price.of_int_exn ((Price.to_int lowest_sell) - 1))
    ~size:(Size.of_int_exn amount_to_sell)
  |> don't_wait_for
;;

let adjust_adr_orders (state : State.t) (order : Exchange_message.Fill.t) =
  let symbol = order.symbol in
  if Symbol.equal symbol Symbol.vale
  then (
    let opp_symbol =
      if Symbol.equal symbol Symbol.valbz then Symbol.vale else Symbol.valbz
    in
    let size = Size.to_int order.size in
    let vale_highest_buy, vale_lowest_sell =
      match
        Map.find state.highest_buy symbol, Map.find state.lowest_sell symbol
      with
      | None, _ | _, None -> 0, 0
      | Some p1, Some p2 -> Price.to_int p1, Price.to_int p2 in
    let valbz_highest_buy, valbz_lowest_sell =
      match
        Map.find state.highest_buy opp_symbol, Map.find state.lowest_sell opp_symbol
      with
      | None, _ | _, None -> 0, 0
      | Some p1, Some p2 -> Price.to_int p1, Price.to_int p2
    in
    if vale_lowest_sell = 0 || valbz_lowest_sell = 0
    then ()
    else (
      match order.dir with
      | Buy ->
        let order_id = Order_id_generator.next_id state.order_id_generator in
        Exchange_driver.add_order
          state.exchange_driver
          ~order_id
          ~symbol:opp_symbol
          ~dir:Sell
          ~price:(Price.of_int_exn (valbz_lowest_sell - 2))
          ~size:(Size.of_int_exn size)
        |> don't_wait_for;
        let order_id = Order_id_generator.next_id state.order_id_generator in
        Exchange_driver.add_order
          state.exchange_driver
          ~order_id
          ~symbol
          ~dir:Sell
          ~price:(Price.of_int_exn (vale_lowest_sell - 8))
          ~size:(Size.of_int_exn size)
        |> don't_wait_for
      | Sell ->
        let order_id = Order_id_generator.next_id state.order_id_generator in
        Exchange_driver.add_order
          state.exchange_driver
          ~order_id
          ~symbol:opp_symbol
          ~dir:Buy
          ~price:(Price.of_int_exn (valbz_highest_buy + 2))
          ~size:(Size.of_int_exn size)
        |> don't_wait_for;
        let order_id = Order_id_generator.next_id state.order_id_generator in
        Exchange_driver.add_order
          state.exchange_driver
          ~order_id
          ~symbol
          ~dir:Buy
          ~price:(Price.of_int_exn (vale_highest_buy + 8))
          ~size:(Size.of_int_exn size)
        |> don't_wait_for))
;;

let exchange_if_needed (state : State.t) =
  if Position.to_int (Map.find_exn state.positions Symbol.vale) = 10 && Position.to_int (Map.find_exn state.positions Symbol.valbz) = -10
  then (
    let order_id = Order_id_generator.next_id state.order_id_generator in
    Exchange_driver.convert
      state.exchange_driver
      ~order_id
      ~dir:Sell
      ~symbol:Symbol.vale
      ~size:(Size.of_int_exn 10)
    |> don't_wait_for);
    state.positions <- Map.change state.positions Symbol.vale ~f:(fun _ -> Some (Position.of_int_exn 0));
    state.positions <- Map.change state.positions Symbol.valbz ~f:(fun _ -> Some (Position.of_int_exn 0));
    if Position.to_int (Map.find_exn state.positions Symbol.vale) = -10 && Position.to_int (Map.find_exn state.positions Symbol.valbz) = 10
  then (
    let order_id = Order_id_generator.next_id state.order_id_generator in
    Exchange_driver.convert
      state.exchange_driver
      ~order_id
      ~dir:Buy
      ~symbol:Symbol.vale
      ~size:(Size.of_int_exn 10)
    |> don't_wait_for);
    state.positions <- Map.change state.positions Symbol.vale ~f:(fun _ -> Some (Position.of_int_exn 0));
    state.positions <- Map.change state.positions Symbol.valbz ~f:(fun _ -> Some (Position.of_int_exn 0));
;;

let reset_all_bond_orders state = ()
