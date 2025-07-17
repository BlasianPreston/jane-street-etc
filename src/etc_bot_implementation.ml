open! Core
open Async
open Import

(* [run_every seconds ~f] is a utility function that will run a given function, [f], every
   [num_seconds] seconds. *)
let run_every seconds ~f =
  Async.Clock.every (Time_float.Span.of_sec seconds) f
;;

let run exchange_type =
  (* Set up a connection to the exchange. *)
  Exchange_driver.connect_and_run
    exchange_type
    ~f:(fun ~exchange_driver ~exchange_messages ->
      let state = State.create exchange_driver in
      let read_messages_and_do_some_stuff () =
        (* Read the messages from the exchange. In general, a good rule of
           thumb is to read a LOT more than you write messages to the exchange.
           (Can you see why? Feel free to ask a TA if you're not sure.)

           [iter_without_pushback] is a way of reminding us that the stuff we
           do while reading the message probably shouldn't cause us to slow
           down reading, but feel free to change it if you would like. *)
        Async.Pipe.iter_without_pushback exchange_messages ~f:(fun message ->
          match message with
          | Open _ -> Bond_strategy.initialize_bond_orders state
          | Hello positions -> State.on_hello state positions
          | Book message ->
            State.on_book state message;
            if not state.initialize_adr
            then (
              match Map.find state.fair_value Symbol.valbz with
              | None -> ()
              | Some price ->
                Adr_strategy.initialize_adr_orders state;
                state.initialize_adr <- true)
          | Fill order ->
            State.on_fill state order;
            Bond_strategy.adjust_bond_orders state order;
            (match Map.find state.fair_value Symbol.valbz with
             | None -> ()
             | Some _ -> Adr_strategy.adjust_adr_orders state order);
            Adr_strategy.exchange_if_needed state
          | Close _ -> failwith "Market Closed"
          | Reject msg -> print_s [%sexp (msg : Exchange_message.Reject.t)]
          | _ ->
            (* Ignore all other messages (for now) *)
            ())
      in
      read_messages_and_do_some_stuff ())
;;

let command =
  Async.Command.async
    ~summary:"My etc bot"
    (* This bot starts off with flags to choose between available exchanges
       and set which log levels are displayed. Feel free to add any other
       flags you need! *)
    [%map_open.Command
      let exchange_type = Exchange_type.param in
      fun () -> run exchange_type]
;;
