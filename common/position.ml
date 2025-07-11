open! Core

module T = struct
  type t = { row : int; column : int } [@@deriving sexp, equal, bin_io, compare]
end

include T
include Comparable.Make_binable (T)

let to_string = Fn.compose Sexp.to_string_hum sexp_of_t

let in_bounds t ~game_kind =
  let board_length = Game_kind.board_length game_kind in
  let open Int.O in
  List.for_all [ t.row; t.column ] ~f:(fun x -> x >= 0 && x < board_length)

let down { row; column } = { row = row + 1; column }
let right { row; column } = { row; column = column + 1 }
let up { row; column } = { row = row - 1; column }
let left { row; column } = { row; column = column - 1 }

let all_offsets =
  let ( >> ) = Fn.compose in
  [
    up; up >> right; right; right >> down; down; down >> left; left; left >> up;
  ]

let no_redundant_offsets =
  let ( >> ) = Fn.compose in
  [ right; right >> down; down; down >> left ]

let distance_from_center t game_kind =
  let board_length = Game_kind.board_length game_kind in
  let center = board_length / 2 in
  let distance_from_center_not_square_rooted =
    Int.pow (center - t.row) 2 + Int.pow (center - t.column) 2
  in
  Float.sqrt (Float.of_int distance_from_center_not_square_rooted)

let surrounding_positions =
  let ( >> ) = Fn.compose in
  [
    right;
    right >> right;
    right >> down >> right >> down;
    right >> down;
    down;
    down >> down;
    down >> left >> down >> left;
  ]

let check_opp =
  let ( >> ) = Fn.compose in
  [
    right;
    right >> right;
    right >> right >> right;
    right >> down;
    right >> right >> down >> down;
    right >> right >> right >> down >> down >> down;
    down;
    down >> down;
    down >> down >> down;
    down >> left;
    down >> down >> left >> left;
    down >> down >> down >> left >> left >> left;
  ]
