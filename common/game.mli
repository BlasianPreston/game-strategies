open! Core

type t = { game_kind : Game_kind.t; board : Piece.t Position.Map.t }
[@@deriving sexp_of, bin_io]

val empty : Game_kind.t -> t
val set_piece : t -> Position.t -> Piece.t -> t
