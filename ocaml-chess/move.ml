open Piece
open Board 

(** An outcome represents whether a move is Invalid or Valid. If the move
    made is invalid, it will also return a string with an error message 
    detailing why. If the move is valid, it will return the updated board with 
    the moved piece in a new location on the board

    Requires:
    - [board] be a valid board 
    - [string] be a valid string *)
type outcome = Invalid of string | Valid of board

(** An Invalid_Move exception is raised when an invalid move has been made. 
    The string returned contains an error message detailing why the move 
    was invalid. 
    Requires:
    - [string] be a valid string *)
exception Invalid_Move of string

(** [flip_player cur_player] returns the other [Piece.color] of the other player
    based on [cur_player]. *)
let flip_player cur_player =
  if cur_player = White
  then Black
  else White

(**[string_to_char_list str acc] returns a char list constructed from the 
   characters of [str]. *)
let rec string_to_char_list str acc =
  match str with
  | "" -> acc
  | a -> 
    string_to_char_list (String.sub a 1 ((String.length a) - 1)) ((a.[0]):: acc)

(**[char_list_to_string lst] returns a string representation of a char list 
   [lst]. *)
let rec char_list_to_string lst =
  List.fold_right (fun h acc -> Char.escaped h ^ acc) lst ""

(** [char_to_int char] returns the int [char] represents. *)
let char_to_int char = (int_of_char char) mod 48

let get_move_locs move : ((int*int)*(int*int))=
  match move with
  | src :: dst :: [] -> 
    ((char_to_int src.[0], char_to_int src.[1]),
     (char_to_int dst.[0], char_to_int dst.[1]))
  | _ -> raise (Invalid_Move "Command invalid")

(** [operand a b] returns either the identity operation, an addition operation,
    or subtraction operation on parameters [a] and [b] depending if they are the
    same number, a is greater than b, or b is greater than a, respectively.*)
let operand a b = 
  match (a - b) with 
  | 0 -> (fun x y -> x)
  | x -> 
    if x > 0 
    then ( + )
    else ( - )

(** [find_no_pieces row1 col1 row2 col2 board] returns false if there is a 
    piece in the shortest path from [(row2, col2)] to [(row1, col1)] exclusive, 
    otherwise it returns true.
    Example:
    - (1, 0) -> (2, 4): checks the locations (2, 1), (2, 2), (2, 3)
    - (0, 0) -> (0, 0) is true 
    - (1, 1) -> (1, 2) is true. 
*)
let rec find_no_pieces row1 col1 row2 col2 board : bool =
  let row2 = (operand row1 row2) row2 1 in
  let col2 = (operand col1 col2) col2 1 in
  (row1 = row2 && col1 = col2) || 
  (match (Board.get_piece row2 col2 board) with
   | None -> (find_no_pieces row1 col1 row2 col2 board)
   | Some p ->  false)

(** 
   [clear_path piece pos2 board] is true when there are no pieces between
   the location of [piece] and [pos2] on board. Importantly, locations of 
   [piece] and [pos2] are not included in this check.

   Requires: Location of [piece] and [pos2] are relatively horiontal, vertical
   or diagonal from each other.
*)
let clear_path piece pos2 board = 
  match Piece.pos piece, pos2 with
  | (row1, col1), (row2, col2) -> find_no_pieces row2 col2 row1 col1 board

(** [pawn_diag_helper color row1 row2 col1 col2 p] is a helper function for
    the pawn_diag_movement function.*)
let pawn_diag_helper color row1 row2 col1 col2 p = 
  match color with 
  | White -> ((row2 = row1 + 1) 
              && (Int.abs (col1 - col2) = 1)) 
             && Piece.get_color p = Black
  | Black -> ((row2 = row1 - 1) 
              && (Int.abs (col1 - col2) = 1))
             && Piece.get_color p = White

(** [pawn_diag_movement piece dst board] is true when there exists a piece at
    [dst] in [board] of the opposite color to [piece] and that piece is 
    diagonally "forward" (down from "Black" pieces, up from "White" pieces) 
    from [piece]. *)
let pawn_diag_movement piece dst board =
  let color = Piece.get_color piece in
  match Piece.pos piece, dst with
  | (row1, col1) , (row2, col2) -> 
    begin
      match (Board.get_piece_tuple dst board) with
      | None -> false
      | Some p -> pawn_diag_helper color row1 row2 col1 col2 p
    end

(** [pm_helper piece p std_func dst board] is a helper for the 
    piece_move_helper function. *)
let pm_helper piece p std_func dst board = 
  not (Piece.get_color piece = Piece.get_color p) && 
  ((not (Piece.get_rank piece = Pawn)) && (std_func piece dst board))

(** [piece_move_helper piece dst board std_func outside_func] returns true if 
    [dst] satisfies these conditions:
    - [dst] is in the list of locations [piece] can move to based on its rank

    if there is a piece of the opposite color of [piece] at [dst] then
    - [std_func piece dst board] returns true from the location of
      [piece] to [dst] *)
let piece_move_helper piece dst board std_func outside_func : bool =
  let standard_moves = Piece.all_moves piece in
  if List.mem dst standard_moves then 
    begin
      match (Board.get_piece_tuple dst board) with
      | None -> std_func piece dst board
      | Some p -> pm_helper piece p std_func dst board
    end
  else outside_func piece dst board

(** [valid_move piece dst board] is true if and only if [piece] can move to
    [dst] according to the pieces movement and capture abilities*)
let valid_move piece dst board  : bool =
  match Piece.get_rank piece with
  | Pawn -> 
    (piece_move_helper piece dst board clear_path pawn_diag_movement)
  | Knight -> 
    (piece_move_helper piece dst board (fun _ _ _ -> true) (fun _ _ _-> false))
  | Bishop -> 
    (piece_move_helper piece dst board clear_path (fun _ _ _-> false))
  | Rook -> 
    (piece_move_helper piece dst board clear_path (fun _ _ _-> false))
  | Queen -> 
    (piece_move_helper piece dst board clear_path (fun _ _ _-> false))
  | King -> 
    (piece_move_helper piece dst board (fun _ _ _ -> true) (fun _ _ _-> false))

(** [ic_helper locs board cond_func playerColor] is a helper function for the
    in_check_helper function. It returns true if every piece in [locs] is 
    the enemy's piece and satisfies the [cond_func], and false otherwise. *)
let rec ic_helper locs board cond_func playerColor =
  match locs with
  | [] -> false
  | h :: t ->
    begin
      match Board.get_piece_tuple h board with
      | None -> ic_helper t board cond_func playerColor 
      | Some p ->
        let cond = 
          ((Piece.get_color p = (flip_player playerColor)) && (cond_func p)) in 
        cond || ic_helper t board cond_func playerColor 
    end

(** [in_check_helper playerColor king_r king_c board mimic_piece cond_func] is
    true when [cond_func] returns true on Piece p retrieved from all of the
    possible move locations of [mimic_piece] from (king_r, king_c)*)
let in_check_helper playerColor king_r king_c board mimic_piece cond_func =
  let new_piece = (Piece.from_tuple (mimic_piece, (king_r, king_c), White)) in 
  let risk_moves = Piece.all_moves new_piece in
  let risk_locs = Board.prune_locs risk_moves [] in
  ic_helper risk_locs board cond_func playerColor

(** [in_check_from_knights playerColor king_r king_c board] is true when
    [playerColor] is in check from knights on [board]*)
let in_check_from_knights playerColor king_r king_c board =
  in_check_helper playerColor king_r king_c board Knight 
    (fun p -> (Piece.get_rank p = Knight) &&
              (valid_move p (king_r, king_c) board))

(** [in_check_from_horizontals playerColor king_r king_c board] is true when
    [playerColor] is in check from Rooks or Queens horizontally on [board]*)
let in_check_from_horizontals playerColor king_r king_c board =
  in_check_helper playerColor king_r king_c board Rook 
    (fun p -> (Piece.get_rank p = Rook || Piece.get_rank p = Queen) &&
              (valid_move p (king_r, king_c) board))

(** [in_check_from_diagonals playerColor king_r king_c board] is true when
    [playerColor] is in check from Bishops or Queens diagonally on [board]*)
let in_check_from_diagonals playerColor king_r king_c board =
  in_check_helper playerColor king_r king_c board Bishop 
    (fun p -> (Piece.get_rank p = Bishop || Piece.get_rank p = Queen) &&
              (valid_move p (king_r, king_c) board))

(** [check_enemy_pawn playerColor p] returns true if p is the enemy's pawn
    and false if otherwise*)
let check_enemy_pawn playerColor p = 
  Piece.get_color p = (flip_player playerColor) &&
  Piece.get_rank p = Pawn

(** [rowfunc playerColor king_r] is a partial function which also takes in 
    a king_c and board, and returns a piece option *)
let rowfunc playerColor king_r = 
  match playerColor with
  | White -> Board.get_piece (king_r + 1)
  | Black -> Board.get_piece (king_r - 1)

(** [in_check_from_pawns playerColor king_r king_c board] is true when
    [playerColor] is in check from pawns on [board]*)
let in_check_from_pawns playerColor king_r king_c board =
  let rf = rowfunc playerColor king_r in 
  match rf (king_c + 1) board, rf (king_c - 1) board with
  | None, None -> false
  | None, Some p -> check_enemy_pawn playerColor p
  | Some p, None -> check_enemy_pawn playerColor p
  | Some p1, Some p2 -> 
    check_enemy_pawn playerColor p1 || check_enemy_pawn playerColor p2

(** [in_check_from_diagonals playerColor king_r king_c board] is true when
    [playerColor] is in check from a king on [board]*)
let in_check_from_king playerColor king_r king_c board = 
  in_check_helper playerColor king_r king_c board King 
    (fun p -> Piece.get_rank p = King && valid_move p (king_r, king_c) board)

(** [king_pos playerColor board] returns the position of the king on the 
    [board] and raises a Failure if there is no king on the board.*)
let king_pos playerColor board = 
  match Board.get_king playerColor board with
  | None -> failwith ("There is no king on the board")
  | Some k -> Piece.pos k

(** [in_check playerColor board] is true when [playerColor] is in check on
    [board] and false otherwise *)
let in_check playerColor board =
  match king_pos playerColor board with
  | (r, c) ->
    in_check_from_pawns playerColor r c board ||
    in_check_from_diagonals playerColor r c board ||
    in_check_from_horizontals playerColor r c board ||
    in_check_from_knights playerColor r c board ||
    in_check_from_king playerColor r c board

(** [update_board] is [board] with the [dst] location occupied by [piece]*)
let update_board piece dst board =
  Board.update piece dst board; board

(** [valid_move_board piece dst state] is the board after [piece] moves to [dst]
    Raises: Invalid_Move when [piece] cannot legally move to [dst] in [state]*)
let valid_move_board piece dst board = 
  match (valid_move piece dst board) with 
  | true -> ((update_board piece dst board))
  | false -> raise (Invalid_Move "Piece selected cannot move there")

(** [dst dst_list] returns the single element in [dst_list].
    Raises: Invalid_Move when dst_list contains more than one element. *)
let dst dst_list = 
  match dst_list with 
  | x :: [] -> x
  | _ -> raise (Invalid_Move "Not a correct destination command")

(** [move_piece_helper b] is the helper function for move_piece.
    Raises: Invalid_Move if the move is trying to capture a king.*)
let move_piece_helper b = 
  match Board.get_king White b, Board.get_king Black b with
  | Some p1, Some p2 -> Valid b
  | _ -> raise (Invalid_Move "You cannot capture the king")

(** [move_piece piece locs board] is the outcome of trying to move [piece] 
    to the destination in [locs] on [board]*)
let move_piece piece dst_list board =
  try
    begin
      let dst' = dst dst_list in 
      let dest = ((char_to_int dst'.[0]), (char_to_int dst'.[1])) in
      let b = (valid_move_board piece dest (Board.copy_board board)) in
      if in_check (Piece.get_color piece) b
      then raise (Invalid_Move "Move will put you in check")
      else move_piece_helper b
    end
  with Invalid_Move x -> Invalid x

