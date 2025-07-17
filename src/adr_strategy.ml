open! Core
open Async
open Import

let initialize_adr_orders (state : State.t) =
  let positions = Map.find_exn state.positions Symbol.valbz in
  let position_as_int = Position.to_int positions in
  let amount_to_buy = 10 - position_as_int in
  let amount_to_sell = 20 - amount_to_buy in
  let fair_value = Map.find_exn state.fair_value Symbol.valbz in
  let order_id = Order_id_generator.next_id state.order_id_generator in
  Exchange_driver.add_order
    state.exchange_driver
    ~order_id
    ~symbol:Symbol.vale
    ~dir:Buy
    ~price:(Price.of_int_exn (Price.to_int fair_value - 7))
    ~size:(Size.of_int_exn amount_to_buy)
  |> don't_wait_for;
  let order_id = Order_id_generator.next_id state.order_id_generator in
  Exchange_driver.add_order
    state.exchange_driver
    ~order_id
    ~symbol:Symbol.vale
    ~dir:Sell
    ~price:(Price.of_int_exn (Price.to_int fair_value + 7))
    ~size:(Size.of_int_exn amount_to_sell)
  |> don't_wait_for
;;

let adjust_adr_orders (state : State.t) (order : Exchange_message.Fill.t) =
  if Symbol.equal order.symbol Symbol.vale
  then (
    let fair_value =
      Map.find_exn state.fair_value Symbol.valbz |> Price.to_int
    in
    match order.dir with
    | Buy ->
      let order_id = Order_id_generator.next_id state.order_id_generator in
      Exchange_driver.add_order
        state.exchange_driver
        ~order_id
        ~dir:Sell
        ~symbol:Symbol.valbz
        ~price:(Price.of_int_exn (fair_value + 7))
        ~size:order.size
      |> don't_wait_for;
      let order_id = Order_id_generator.next_id state.order_id_generator in
      Exchange_driver.add_order
        state.exchange_driver
        ~order_id
        ~dir:Sell
        ~symbol:Symbol.vale
        ~price:(Price.of_int_exn (fair_value + 7))
        ~size:order.size
      |> don't_wait_for
    | Sell ->
      let order_id = Order_id_generator.next_id state.order_id_generator in
      Exchange_driver.add_order
        state.exchange_driver
        ~order_id
        ~dir:Buy
        ~symbol:Symbol.valbz
        ~price:(Price.of_int_exn (fair_value - 7))
        ~size:order.size
      |> don't_wait_for;
      let order_id = Order_id_generator.next_id state.order_id_generator in
      Exchange_driver.add_order
        state.exchange_driver
        ~order_id
        ~dir:Buy
        ~symbol:Symbol.vale
        ~price:(Price.of_int_exn (fair_value - 7))
        ~size:order.size
      |> don't_wait_for)
;;

let exchange_if_needed (state : State.t) =
  if
    Position.to_int (Map.find_exn state.positions Symbol.vale) = 10
    && Position.to_int (Map.find_exn state.positions Symbol.valbz) = -10
  then (
    let order_id = Order_id_generator.next_id state.order_id_generator in
    Exchange_driver.convert
      state.exchange_driver
      ~order_id
      ~dir:Sell
      ~symbol:Symbol.vale
      ~size:(Size.of_int_exn 10)
    |> don't_wait_for);
  state.positions
  <- Map.change state.positions Symbol.vale ~f:(fun _ ->
       Some (Position.of_int_exn 0));
  state.positions
  <- Map.change state.positions Symbol.valbz ~f:(fun _ ->
       Some (Position.of_int_exn 0));
  if
    Position.to_int (Map.find_exn state.positions Symbol.vale) = -10
    && Position.to_int (Map.find_exn state.positions Symbol.valbz) = 10
  then (
    let order_id = Order_id_generator.next_id state.order_id_generator in
    Exchange_driver.convert
      state.exchange_driver
      ~order_id
      ~dir:Buy
      ~symbol:Symbol.vale
      ~size:(Size.of_int_exn 10)
    |> don't_wait_for);
  state.positions
  <- Map.change state.positions Symbol.vale ~f:(fun _ ->
       Some (Position.of_int_exn 0));
  state.positions
  <- Map.change state.positions Symbol.valbz ~f:(fun _ ->
       Some (Position.of_int_exn 0))
;;

let reset_all_bond_orders state = ()
