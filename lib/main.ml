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
    List.init board_width ~f:(fun row ->
        List.init board_width ~f:(fun column ->
            let position = { Position.row; column } in
            match Map.find board position with
            | Some _ -> None
            | None -> Some position))
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
        (* Change to find_map *)
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
  | Some winner -> Game_over { winner = Some winner }
  | None ->
      let available_moves = available_moves game in
      if List.is_empty available_moves then Game_over { winner = None }
      else Game_continues

let%expect_test "evalute_non_win" =
  print_s (Evaluation.sexp_of_t (evaluate non_win));
  [%expect {|
      Game_continues
      |}];
  return ()

let%expect_test "evalute_non_win" =
  let test_win =
    Game.set_piece non_win
      { Position.row = 1; column = 1 }
      (Piece.of_string "X")
  in
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
  | true -> available_moves
  | false ->
      List.filter available_moves ~f:(fun move ->
          not (List.mem opponent_winning_moves move ~equal:Position.equal))

let%expect_test "evalute_losing_moves" =
  print_s
    (sexp_of_list Position.sexp_of_t
       (losing_moves ~me:(Piece.of_string "O") non_win));
  [%expect
    {|
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
  List.filter available_moves ~f:(fun move ->
      not (List.mem losing_moves move ~equal:Position.equal))

let%expect_test "evalute_moves_that_do_not_immediately_lose" =
  print_s
    (sexp_of_list Position.sexp_of_t
       (available_moves_that_do_not_immediately_lose ~me:(Piece.of_string "O")
          non_win));
  [%expect {|
    (((row 1) (column 1)))
    |}];
  return ()

(* Exercise 5 *)
let _make_move_tictactoe ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let win_moves = winning_moves ~me:you_play game in
  let opponent_winning_moves = winning_moves ~me:(Piece.flip you_play) game in
  if not (List.is_empty win_moves) then List.hd_exn win_moves
  else if not (List.is_empty opponent_winning_moves) then
    List.hd_exn opponent_winning_moves
  else
    match
      List.hd (available_moves_that_do_not_immediately_lose game ~me:you_play)
    with
    | Some move -> move
    | None -> List.random_element_exn (available_moves game)

let check_for_opponents board_lst (game : Game.t) me =
  List.fold board_lst ~init:0. ~f:(fun acc (position, piece) ->
      match piece with
      | Some p ->
          if Piece.equal (Piece.flip me) p then
            let surroundings =
              List.map Position.check_opp ~f:(fun direction ->
                  direction position)
              |> List.filter ~f:(fun position ->
                     not (Position.in_bounds position ~game_kind:game.game_kind))
            in
            acc
            +. List.fold surroundings ~init:0. ~f:(fun acc surround_pos ->
                   match Map.find game.board surround_pos with
                   | Some piece ->
                       if Piece.equal piece (Piece.flip me) then acc +. 10.
                       else acc
                   | None -> acc)
          else acc
      | None -> acc)

let all_surrounding_positions (game : Game.t) =
  let board = game.board in
  let closed_positions = Map.keys board in
  List.concat_map closed_positions ~f:(fun pos ->
      List.map Position.all_offsets ~f:(fun direction -> direction pos))

let calculate_sum_of_distances_from_game board_lst (game : Game.t) me =
  List.fold board_lst ~init:0. ~f:(fun acc (position, piece) ->
      match piece with
      | Some p ->
          if Piece.equal me p then
            acc +. (10. *. Position.distance_from_center position game.game_kind)
          else acc
      | None -> acc)

let get_score game me =
  match evaluate game with
  | Game_over { winner } -> (
      match winner with
      | Some winner ->
          if Piece.equal winner me then Float.max_value else Float.min_value
      | None -> 0.)
  | _ ->
      let board = game.board in
      let board_width = Game_kind.board_length game.game_kind in
      let board_lst =
        List.init board_width ~f:(fun row_idx ->
            List.init board_width ~f:(fun col_idx ->
                let position = { Position.row = row_idx; column = col_idx } in
                match Map.find board position with
                | Some piece -> (position, Some piece)
                | None -> (position, None)))
      in
      let board_lst = List.concat board_lst in
      0.
      -. calculate_sum_of_distances_from_game board_lst game me
      +. check_for_opponents board_lst game me

let rec minimax game depth maximizing_player me alpha beta =
  if depth = 0 then get_score game me
  else if maximizing_player then
    let rec loop moves alpha =
      match moves with
      | [] -> alpha
      | move :: rest ->
          let score =
            minimax
              (Game.set_piece game move me)
              (depth - 1) false me alpha beta
          in
          let alpha = Float.max alpha score in
          if Float.(beta <= alpha) then alpha else loop rest alpha
    in
    loop (all_surrounding_positions game) alpha
  else
    let rec loop moves beta =
      match moves with
      | [] -> beta
      | move :: rest ->
          let score =
            minimax
              (Game.set_piece game move me)
              (depth - 1) false me alpha beta
          in
          let beta = Float.min beta score in
          if Float.(beta <= alpha) then beta else loop rest beta
    in
    loop (all_surrounding_positions game) beta

let make_move ~(game : Game.t) ~you_play =
  if Map.is_empty game.board then
    let center = (Game_kind.board_length game.game_kind - 1) / 2 in
    { Position.row = center; column = center }
  else
    let available_moves = available_moves game in
    let score_move_pairing =
      List.fold available_moves
        ~init:(Float.min_value, { Position.row = 0; column = 0 })
        ~f:(fun acc move ->
          let new_game = Game.set_piece game move you_play in
          let move_and_score =
            ( minimax new_game 3 false you_play Float.min_value Float.max_value,
              move )
          in
          match (acc, move_and_score) with
          | (old_score, old_move), (new_score, new_move) ->
              if Float.(new_score > old_score) then (new_score, new_move)
              else (old_score, old_move))
    in
    match score_move_pairing with _, move -> move
