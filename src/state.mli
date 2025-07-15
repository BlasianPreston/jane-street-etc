open! Core
open Import

type t =
    { mutable positions : Position.t Symbol.Map.t
    ; mutable fair_values : Price.t Symbol.Map.t
    ; order_id_generator : Order_id_generator.t
    ; exchange_driver : (Exchange_driver.t [@sexp.opaque])
    }
  [@@deriving sexp]

val create : Exchange_driver.t -> t

val next_id : t -> Order_id.t

val on_hello : t -> (Symbol.t * Position.t) list -> unit

val on_fill : t -> Exchange_message.Fill.t -> unit