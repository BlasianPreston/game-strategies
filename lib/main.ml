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

let all_positions (game : Game.t) =
  let row = List.init (game.game_kind |> Game_kind.board_length) ~f:Fn.id in
  let cartesian_prod = List.cartesian_product row row in
  List.map cartesian_prod ~f:(fun (x, y) ->
      { Position.row = x; Position.column = y })

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  let board = game.board in
  let board_win_length = Game_kind.win_length game.game_kind in
  let rec check_position target depth direction position =
    match (depth >= board_win_length, Map.find board position) with
    | true, _ -> true
    | false, None -> false
    | false, Some piece -> (
        match Piece.equal piece target with
        | false -> false
        | true ->
            check_position target (depth + 1) direction (direction position))
  in
  let all_positions = all_positions game in
  match
    List.fold ~init:None all_positions ~f:(fun winner position ->
        match (winner, Map.find board position) with
        | Some _, _ -> winner
        | None, None -> None
        | None, Some piece ->
            let possible_win =
              List.exists Position.no_redundant_offsets ~f:(fun direction ->
                  check_position piece 0 direction position)
            in
            if possible_win then Some piece else None)
  with
  | Some winner -> Game_over { winner=Some winner }
  | None -> let available_moves = available_moves game in
  if List.is_empty available_moves then Game_over {winner=None} else Game_continues

let%expect_test "evalute_non_win" =
  print_s (Evaluation.sexp_of_t (evaluate non_win));
  [%expect {|
      Game_continues
      |}];
  return ()

let%expect_test "evalute_non_win" =
let test_win = Game.set_piece non_win {Position.row = 1; column = 1} (Piece.of_string "X") in
  print_s (Evaluation.sexp_of_t (evaluate test_win));
  [%expect {|
      (Game_over (winner (X)))
      |}];
  return ()

let%expect_test "evalute_win" =
  print_s (Evaluation.sexp_of_t (evaluate win_for_x));
  [%expect {|
      (Game_over (winner (X)))
      |}];
  return ()

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let available_moves = available_moves game in
  List.map available_moves ~f:(fun move ->
      match evaluate (Game.set_piece game move me) with
      | Game_over { winner } -> (
          match winner with
          | Some winner -> if Piece.equal winner me then Some move else None
          | None -> None)
      | _ -> None)
  |> List.filter_opt

let%expect_test "evalute_winning_moves" =
  print_s
    (sexp_of_list Position.sexp_of_t
       (winning_moves ~me:(Piece.of_string "X") non_win));
  [%expect {|
      (((row 1) (column 1)))
      |}];
  return ()

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let opponent_winning_moves = winning_moves ~me:(Piece.flip me) game in
  let available_moves = available_moves game in
  match List.length opponent_winning_moves >= 2 with
    true -> available_moves
    | false -> List.filter available_moves ~f:(fun move -> not (List.mem opponent_winning_moves move ~equal:(Position.equal)))

let%expect_test "evalute_losing_moves" =
  print_s
    (sexp_of_list Position.sexp_of_t
       (losing_moves ~me:(Piece.of_string "O") non_win));
  [%expect {|
    (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2)) 
     ((row 2) (column 1)))|}];
  return ()

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

let available_moves_that_do_not_immediately_lose game ~me =
  let available_moves = available_moves game in
  let losing_moves = losing_moves ~me game in
  List.filter available_moves ~f:(fun move -> not (List.mem losing_moves move ~equal:Position.equal))

let%expect_test "evalute_moves_that_do_not_immediately_lose" =
  print_s
    (sexp_of_list Position.sexp_of_t
       (available_moves_that_do_not_immediately_lose ~me:(Piece.of_string "O") non_win));
  [%expect {|
    (((row 1) (column 1)))
    |}];
  return ()

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let win_moves = winning_moves ~me:you_play game in
  let opponent_winning_moves = winning_moves ~me:(Piece.flip you_play) game in
  if (not (List.is_empty (win_moves))) then 
    (List.hd_exn win_moves) else if not (List.is_empty opponent_winning_moves) then List.hd_exn opponent_winning_moves else List.hd_exn (available_moves_that_do_not_immediately_lose game ~me:you_play)
