open! Core
open Import

val initialize_bond_orders : State.t -> unit

val adjust_bond_orders : State.t -> Exchange_message.Fill.t -> unit