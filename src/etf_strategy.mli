open! Core
open Import


val initialize_etf_orders : State.t -> unit

val adjust_etf_orders : State.t -> Exchange_message.Fill.t -> unit