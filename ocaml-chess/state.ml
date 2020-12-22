open Board
open Piece
open Command
open Move

type board = Board.board
type captured_pieces = Piece.t list
type move_history = Command.command list
type current_player = Piece.color
type winner = string
type draw_requested = bool
type win = Winner of Piece.color option | Draw
type en_passant = (Piece.t * Piece.loc) option
type king_notmoved = bool * bool
type rook_notmoved = (bool * bool) * (bool * bool)

type t = {
  board : board;
  captured_pieces : captured_pieces;
  move_history : move_history;
  winner : win;
  current_player : current_player;
  draw_requested : bool;
  promotion : Piece.t option; 
  en_passant : en_passant;
  king_notmoved : king_notmoved;
  rook_notmoved : rook_notmoved;
}

exception Invalid_Move of string
type result = Illegal of string | Legal of t

let create_state board captured move winner current draw : t = 
  {
    board = board;
    captured_pieces = captured;
    move_history = move;
    winner = winner;
    current_player = current;
    draw_requested = draw;
    promotion = None; 
    en_passant = None;
    king_notmoved = (true, true);
    rook_notmoved = (true, true), (true, true);
  }

let init_state () = 
  {
    board = Board.init_board ();
    captured_pieces = [];
    move_history = [];
    winner = Winner None;
    current_player = White;
    draw_requested = false;
    promotion = None; 
    en_passant = None;
    king_notmoved = (true, true);
    rook_notmoved = (true, true), (true, true);
  }

let flip_player cur_player state =
  if cur_player = White
  then {state with current_player = Black}
  else {state with current_player = White}

let get_board state = 
  state.board

let get_captured state =
  state.captured_pieces

let get_winner state = 
  state.winner 

let get_winner_string state = 
  match state.winner with 
  | Draw -> ""
  | Winner (Some White) -> "White"
  | Winner (Some Black) -> "Black"
  | Winner (None) -> ""

let get_cur_player state =
  state.current_player 

let get_prev_moves state =
  state.move_history

let get_draw_requested state =
  state.draw_requested

let get_promotion state = 
  state.promotion

(** [check_validity_of_all_moves piece state] is true when [piece] has any
    legal moves in the given [state], false otherwise *)
let check_validity_of_all_moves piece board =
  let moves = Board.prune_locs (Piece.all_moves piece) [] in
  let rec check_all_helper lst piece state =
    match lst with
    | [] -> false
    | (r, c) :: t -> 
      let dst_list = [string_of_int r ^ string_of_int c] in
      match (Move.move_piece piece dst_list board) with
      | Invalid x -> check_all_helper t piece board
      | Valid b -> true
  in
  check_all_helper moves piece board

(** [return_winner w] is type [Winner] of [w]. *)
let return_winner winner =
  match winner with
  | (true, true) -> Winner (None)
  | (false, false) -> Draw
  | (true, false) -> Winner (Some White)
  | (false, true) -> Winner (Some Black)

(** [next_winner] is a helper function for [update_winner_helper]. *)
let next_winner p board next_update white black = 
  let any_valid = check_validity_of_all_moves p board in
  if any_valid
  then match Piece.get_color p with
    | White -> next_update true black
    | Black -> next_update white true
  else next_update white black

(** [update_winner board] is the winner of the given [board]*)
let update_winner board =
  let rec update_winner_helper r c board white black = 
    if r = 8
    then (white, black)
    else
      let updated_r = if c = 7 then r + 1 else r in
      let updated_c = if c = 7 then 0 else c + 1 in
      let next_update = update_winner_helper updated_r updated_c board in
      match Board.get_piece r c board with
      | None -> next_update white black
      | Some p -> next_winner p board next_update white black
  in
  return_winner (update_winner_helper 0 0 board false false)

(** [piece_at_loc loc state] is the piece at [loc] in the board of [state]. *)
let piece_at_loc loc state = 
  let row = int_of_string (String.sub loc 0 1) in 
  let col = int_of_string (String.sub loc 1 1) in 
  get_piece row col (get_board state)

(** [dest_tuple dst] is the tuple version of [dst] *)
let dest_tuple dst = 
  let dest = match dst with 
    | [x] -> x
    | _ -> ""
  in ((Move.char_to_int dest.[0]), (Move.char_to_int dest.[1]))

(** [captured_pieces captured state] is the updated list of captured
    pieces in [state] *)
let captured_pieces captured state= 
  match captured with 
  | None -> state.captured_pieces 
  | Some p -> p :: state.captured_pieces

(** [get_promotion piece dst] is the piece option of [piece] if it is a pawn
    and None if it isn't *)
let promotion piece dst = 
  let loc_tup = dest_tuple dst in
  match (get_rank piece, get_color piece, loc_tup) with 
  | (Pawn, White, (7,_)) -> Some (from_tuple (Pawn, loc_tup, White ))
  | (Pawn, Black, (0,_)) -> Some (from_tuple (Pawn, loc_tup, Black ))
  | _ -> None

(** [en_passant piece dst] is the piece reflected from performing en passant
    on [piece] at [dst] *)
let en_passant piece dst = 
  match get_rank piece, get_color piece, dest_tuple dst, pos piece with 
  | Pawn, c, (r2, col), (r1, _) when r2 - r1 = -2 ->
    Some (from_tuple (Pawn, (r2, col ), c), (r1 - 1, col))
  | Pawn, c, (r2, col), (r1, _) when r2 - r1 = 2 ->
    Some (from_tuple (Pawn, (r2, col ), c), (r1 + 1, col))
  | _ -> None 

(** [new_state_king_helper player state piece] updates [state] if white or
    black have moved their kings in the current game*)
let new_state_king_helper player state =
  match player, state.king_notmoved with 
  | White, (white_move, black_move) -> (false, black_move)
  | Black, (white_move, black_move)-> (white_move, false)

(** [new_state_rook_helper player state piece] updates [state] if white or
    black have moved their rooks in the current game*)
let new_state_rook_helper player state piece = 
  match player, state.rook_notmoved, Piece.pos piece with 
  | White, ((wq, wk), (bq, bk)), (0, 0) -> ((false, wk), (bq, bk))
  | White, ((wq, wk), (bq, bk)), (0, 7) -> ((wq, false), (bq, bk))
  | Black, ((wq, wk), (bq, bk)), (7, 0) -> ((wq, wk), (false, bk))
  | Black, ((wq, wk), (bq, bk)), (7, 7) -> ((wq, wk), (bq, false))
  | _ -> state.rook_notmoved

(** [new_state b c p dst state player] is the state defined by [player] moving
    [p] to [dst] in board [b] capturing [captured] with origin state [state]  *)
let new_state board captured piece dst state player = 
  Legal {
    board = board;
    captured_pieces = (captured_pieces captured state);
    move_history = (Move (Piece.get_str_pos piece :: dst ))
                   :: state.move_history;
    winner = update_winner board; 
    current_player = player;
    draw_requested = false;
    promotion = (promotion piece dst);
    en_passant = (en_passant piece dst);   
    king_notmoved = (if Piece.get_rank piece = King 
                     then new_state_king_helper player state
                     else state.king_notmoved);
    rook_notmoved = (if Piece.get_rank piece = Rook
                     then new_state_rook_helper player state piece
                     else state.rook_notmoved);
  }

(** [move_piece_aux piece dst state player] returns a [result] including the 
    rule of enpassant. *)
let move_piece_aux piece dst state player = 
  let dst_tuple = dest_tuple dst in 
  match state.en_passant with 
  | Some (p, loc) when loc = dst_tuple -> begin 
      let captured = Some p in
      let aux_board = Board.copy_board state.board in 
      (Board.add_piece p loc aux_board);
      match Move.move_piece piece dst aux_board with 
      | Invalid x -> Illegal x
      | Valid b -> 
        (Board.remove_piece p b); new_state b captured piece dst state player
    end
  | _ -> begin
      let captured = Board.get_piece_tuple (dst_tuple) state.board in
      match Move.move_piece piece dst state.board with 
      | Invalid x -> Illegal x
      | Valid b -> new_state b captured piece dst state player
    end

(** [get_piece_from_option p] returns type Piece.t

    Raises: Failure if [p] is [None].  *)
let get_piece_from_option = function 
  | Some p -> p 
  | None -> failwith "impossible"

(** [check_rook_moved_w dst_tuple state] checks if the white rook has moved in 
    [state] depending on which castle is being done returns true if rook has 
    not moved.

    Requires: dst_tuple is a valid castling destination for white player. *)
let check_rook_moved_w dst_tuple state = 
  if dst_tuple = (0, 6) then begin 
    match state.rook_notmoved with 
    | (_, b), _ -> b
  end
  else 
    match state.rook_notmoved with 
    | (b, _), _ -> b

(** [check_rook_moved_b dst_tuple state] checks if the white rook has moved in 
    [state] depending on which castle is being done returns true if rook 
    has not moved.

    Requires: dst_tuple is a valid castling destination for black player. *)
let check_rook_moved_b dst_tuple state = 
  if dst_tuple = (7, 6) then begin 
    match state.rook_notmoved with 
    | _, (_, b) -> b
  end
  else 
    match state.rook_notmoved with 
    | _, (b, _) -> b

(** [create_rook_castle dst_tuple player] returns a rook after a castle has 
    happened *)
let create_rook_castle dst_tuple player : Piece.t = 
  match player with 
  | White -> 
    if dst_tuple = (0, 6) 
    then Piece.from_tuple (Rook, (0, 5), player)  
    else Piece.from_tuple (Rook, (0, 3), player)
  | Black -> 
    if dst_tuple = (7, 6) 
    then Piece.from_tuple (Rook, (7, 5), player)  
    else Piece.from_tuple (Rook, (7, 3), player) 

(** [add_king_and_rook_castle dst_tuple copy_board player] updates [copy_board] 
    after a castle has been executed. *)
let add_king_and_rook_castle dst_tuple copy_board player : unit = 
  let king = Piece.from_tuple (King, dst_tuple, player) in 
  let rook = create_rook_castle dst_tuple player in 
  Board.add_piece king (pos king) copy_board;
  Board.add_piece rook (pos rook) copy_board 

(** [remove_king_and_rook dst_tuple piece copy_board player] updates 
    [copy_board] by removing the king and rook that will castle. *)
let remove_king_and_rook dst_tuple piece copy_board player : unit = 
  Board.remove_piece piece copy_board;
  match player with 
  | White -> if dst_tuple = (0, 6) then
      Board.remove_piece (Piece.from_tuple (Rook, (0, 7), player)) copy_board  
    else
      Board.remove_piece (Piece.from_tuple (Rook, (0, 0), player)) copy_board
  | Black -> if dst_tuple = (7, 6) then
      Board.remove_piece (Piece.from_tuple (Rook, (7, 7), player)) copy_board  
    else
      Board.remove_piece (Piece.from_tuple (Rook, (7, 0), player)) copy_board 

(** [castle_check piece dst_tuple player] returns true if the castle movement
    will cause king to go into check. 

    Requires: all other castling criteria pass. *)
let castle_check dst_tuple piece state player = 
  let copy_board = Board.copy_board state.board in 
  remove_king_and_rook dst_tuple piece copy_board player; 
  add_king_and_rook_castle dst_tuple copy_board player;
  in_check player copy_board

(** [check_row_empty colbegin colend state] checks if the board of [state] is 
    empty exclsuively from [colbegin] to [colend]. *)
let check_row_empty row colbegin colend state : bool = 
  let x = ref (colbegin + 1) in 
  let b = ref true in 
  while !x < colend do 
    match Board.get_piece_tuple (row, !x) state.board with 
    | Some _ -> b := false; x := !x + 1
    | None -> x := !x + 1
  done;
  !b

(** [rook_to_king_empty_castle dst state player] returns true if rook there 
    are no pieces between the rook and king that will be castled

    Requires: [dst_tuple] is a valid castling destination and the rook and king
    have not moved and the player is not in check. *)
let rook_to_king_empty_castle dst_tuple state player = 
  match player with 
  | White -> if dst_tuple = (0, 6) 
    then check_row_empty 0 4 7 state 
    else check_row_empty 0 0 4 state
  | Black -> if dst_tuple = (7, 6) 
    then check_row_empty 7 4 7 state 
    else check_row_empty 7 0 4 state

(** [castle_through_check dst_tuple piece state player] returns true if [piece] 
    will castle through a check.

    Requires: all other castling criteria to pass. *)
let castle_through_check dst_tuple piece state player = 
  let copy_board = Board.copy_board state.board in
  let new_king =
    match player with 
    | White -> if dst_tuple = (0, 6) 
      then Piece.from_tuple (King, (0, 5), player)
      else Piece.from_tuple (King, (0, 3), player)
    | Black -> if dst_tuple = (7, 6) 
      then Piece.from_tuple (King, (7, 5), player)
      else Piece.from_tuple (King, (7, 3), player)
  in Board.remove_piece piece copy_board;
  Board.add_piece new_king (pos new_king) copy_board;
  in_check player copy_board

(** [check_valid_castle piece dst state player] check if the castling critieria 
    all passes.

    Requires: [dst] is a valid castling location and [piece] is of rank king. *)
let check_valid_castle piece dst state player =
  let dst_tuple = dest_tuple dst in
  let no_movement = match player, state.king_notmoved with
    | White, (b, _) -> b && check_rook_moved_w dst_tuple state
    | Black, (_, b) -> b && check_rook_moved_b dst_tuple state
  in
  not (in_check player state.board) && no_movement && 
  rook_to_king_empty_castle dst_tuple state player && 
  not (castle_check dst_tuple piece state player || 
       castle_through_check dst_tuple piece state player)

(** [move_piece_caste piece dst state player] returns the [Legal] result using 
    castling movement. 

    Requires: all castling criteria have passed. *)
let move_piece_castle piece dst state player = 
  let dst_tuple = dest_tuple dst in
  let board = Board.copy_board state.board in 
  remove_king_and_rook dst_tuple piece board player;
  add_king_and_rook_castle dst_tuple board player;
  new_state board None piece dst state player

(** [to_castle_dst dst player] returns true if [dst] is a location on the board
    that is for castling based on [player]. *)
let to_castle_dst dst player = 
  let dst_tuple = dest_tuple dst in
  match player with 
  | White -> dst_tuple = (0, 6) || dst_tuple = (0, 2)
  | Black -> dst_tuple = (7, 6) || dst_tuple = (7, 2)

(** [move_piece_helper p dst state color] returns a [result] after 
    attempting to move piece [p] to [dst] in the the [board] of [state] 
    by player [color].
    Requires:
    - [p] has color [color]
    - [p] is a valid piece of Piece.t type 
    - [dst] is a list of valid strings 
    - [t] is a valid state of State.t type
    - [color] is a valid color of type Piece.color*)
let move_piece_helper piece dst state player = 
  if get_rank piece = King && to_castle_dst dst player && 
     check_valid_castle piece dst state player 
  then move_piece_castle piece dst state player 
  else move_piece_aux piece dst state player

let move_piece piece dst state player : result = 
  if (Piece.get_color piece = player) 
  then move_piece_helper piece dst state player
  else Illegal "You have tried to move your opponents piece. 
  Please choose your own piece."

let set_board board state = 
  {
    state with
    board = board;
  }

let set_captured captured state = 
  {
    state with
    captured_pieces = captured;
  }

let set_prev_moves prev_moves state = 
  {
    state with
    move_history = prev_moves;
  }

let set_cur_player player state =
  {
    state with
    current_player = player;
  }

let set_draw_requested req state =
  {
    state with 
    draw_requested = req;
  }

let set_promotion promotion state = 
  {
    state with 
    promotion = promotion;
  }

let set_winner winner state = 
  {
    state with 
    winner = winner;
  }
