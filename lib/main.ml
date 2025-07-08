open! Core
open! Async
open! Game_strategies_common_lib

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let non_win =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

let print_game (game : Game.t) =
  let board = game.board in
  let board_width = Game_kind.board_length game.game_kind in
  let board_lst =
    List.init board_width ~f:(fun row_idx ->
        List.init board_width ~f:(fun col_idx ->
            match Map.find board { row = row_idx; column = col_idx } with
            | Some piece ->
                if col_idx = board_width - 1 then Piece.to_string piece
                else Piece.to_string piece ^ " | "
            | None -> if col_idx = board_width - 1 then " " else "  | "))
  in
  List.iteri board_lst ~f:(fun row_idx lst ->
      if not (row_idx = board_width - 1) then (
        print_endline (String.concat ~sep:"" lst);
        print_endline (String.make ((board_width * 4) - 3) '-'))
      else print_endline (String.concat ~sep:"" lst))

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  let board = game.board in
  let board_width = Game_kind.board_length game.game_kind in
  let board_lst =
    List.init board_width ~f:(fun row_idx ->
        List.init board_width ~f:(fun col_idx ->
            match Map.find board { row = row_idx; column = col_idx } with
            | Some _ -> None
            | None -> Some { Position.row = row_idx; column = col_idx }))
  in
  let filtered_lst = List.map board_lst ~f:(fun row -> List.filter_opt row) in
  List.concat filtered_lst

let%expect_test "print_available moves" =
  print_s (sexp_of_list Position.sexp_of_t (available_moves non_win));
  [%expect
    {|
    (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
     ((row 1) (column 2)) ((row 2) (column 1)))|}];
  return ()

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  ignore game;
  failwith "Implement me!"

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  ignore me;
  ignore game;
  failwith "Implement me!"

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  ignore me;
  ignore game;
  failwith "Implement me!"

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  ignore game;
  ignore you_play;
  failwith "Implement me!"
