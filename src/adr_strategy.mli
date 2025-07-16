open! Core
open Import


val initialize_adr_orders : State.t -> unit

val adjust_adr_orders : State.t -> Exchange_message.Fill.t -> unit

val exchange_if_needed: State.t -> unit