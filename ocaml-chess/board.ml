open Piece 

(* Abstraction function: the array [[Piece x ...; Piece y]....
   [Piece z]...Piece t]] represents the
 * chess board with bottom left position being 00 and top right position as 77.
   The first value is for the row number, and the second value is for the column 
   number.  
 * The double array is populated with piece of [Piece], where None represents an 
   empty cell in the board and a Piece of _ is that kind of piece in the board
   Representaion Invariant: There are never duplicates of any pieces of the same
   color on the board except in the case of pawns, in which there are 8, and
   when a pawn has been promoted to another piece, as per the official rules of 
   chess. 
 *
*)
type board = Piece.t option array array

(** [add_pawns x y color board] adds a pawn to to the position [x,y] on 
    [b] of color [color]*)
let rec add_pawns x y color board = 
  board.(x).(y) <- Some (Piece.from_tuple (Pawn, (x,y), color));
  if y < 7 then add_pawns x (y + 1) color board

(** [add_pieces x color board] adds the appropriate pieces to the position
    [x,y] on [b] of color [color] where y is incremented from 0..7*)
let rec add_pieces x color board = 
  for y = 0 to 7 do 
    match y with 
    | 0 -> board.(x).(y) <- Some (Piece.from_tuple (Rook, (x,y), color));
    | 1 -> board.(x).(y) <- Some (Piece.from_tuple (Knight, (x,y), color));
    | 2 -> board.(x).(y) <- Some (Piece.from_tuple (Bishop, (x,y), color));
    | 3 -> board.(x).(y) <- Some (Piece.from_tuple (Queen, (x,y), color));
    | 4 -> board.(x).(y) <- Some (Piece.from_tuple (King, (x,y), color));
    | 5 -> board.(x).(y) <- Some (Piece.from_tuple (Bishop, (x,y), color));
    | 6 -> board.(x).(y) <- Some (Piece.from_tuple (Knight, (x,y), color));
    | 7 -> board.(x).(y) <- Some (Piece.from_tuple (Rook, (x,y), color));
    | _ -> board.(3).(3) <- None;
  done

(** [make_board] sets each indice [empty] that corresponds to a chess piece
    in its intial state to the piece that corresponds to that position *)
let make_board empty =
  add_pieces 0 White empty;
  add_pawns 1 0 White empty;
  add_pieces 7 Black empty;
  add_pawns 6 0 Black empty;
  empty 

let init_board () = 
  let empty = Array.make_matrix 8 8 (None) in
  let empty = make_board empty in
  empty

let get_piece (row : int) (col : int) (b : board): Piece.t option = 
  if row > 7 || row < 0 || col > 7 || col < 0
  then None
  else b.(row).(col)

(** [is_piece_at] returns true if the piece [p] is at the position 
    ([row], [col]) on board [b], and false if otherwise.
    Requires: 
    - [row] is an integer 0..7
    - [col] is an integer 0..7
    - [b] is a valid board 
    - [p] is a valid piece *)
let is_piece_at (row : int) (col : int) (b : board) (p : Piece.t option) : bool
  = if row > 7 || row < 0 || col > 7 || col < 0
  then false
  else b.(row).(col) = p

let is_piece_at_tuple tup b p =
  match tup with
  | (x,y) -> is_piece_at x y b p

let get_piece_tuple tup board  =
  match tup with
  | (a, b) -> get_piece a b board

let remove_piece p b = 
  match Piece.pos p with 
  | (row, col) -> b.(row).(col) <- None

let add_piece p d b = 
  match d with 
  | (row,col) -> b.(row).(col) <-  Some (
      Piece.from_tuple (Piece.get_rank p, (row,col), Piece.get_color p))

let update (piece : Piece.t) (dst : (int*int)) (board : board) =
  remove_piece piece board; add_piece piece dst board

(** [copy_board_aux curx cury b copy] inserts whatever piece is at 
    [curx, cury] on [b] into [copy] at the same position*)
let rec copy_board_aux curx cury board copy =
  if curx = 8 then copy else
    match get_piece_tuple (curx, cury) board with
    | None -> 
      copy_check_cury curx cury board copy
    | Some p -> 
      let p2 = 
        Piece.from_tuple(Piece.get_rank p, Piece.pos p, Piece.get_color p) in
      copy.(curx).(cury) <- Some p2;
      copy_check_cury curx cury board copy

(** [copy_check_cury x y board copy] updates x and y coordinates of [board] 
    in order to place the pieces into [copy]. *)
and copy_check_cury curx cury board copy =
  if cury < 7 
  then copy_board_aux curx (cury + 1) board copy 
  else copy_board_aux (curx + 1) 0 board copy

let copy_board board =
  let copy = Array.make_matrix 8 8 (None) in
  copy_board_aux 0 0 board copy

(** [get_king_aux curx cury color b] if the piece at [curx, cury] is the piece
    King of [color] then it returns that piece. Otherwise is increments to 
    the next piece on the board
*)
let rec get_king_aux curx cury color board =
  if curx = 8 then None else check_king_at_loc curx cury color board

(** [king_check_cury y x color board] updates x and y through [board] to find 
    king of [color].  *)
and king_check_cury cury curx color board =
  if cury < 7 
  then get_king_aux curx (cury + 1) color board 
  else get_king_aux (curx + 1) 0 color board

(** [check_king_at_loc x y color board] checks if [board] at position [x,y] has
    a king of [color] *)
and check_king_at_loc curx cury color board = 
  match get_piece_tuple (curx, cury) board with
  | None -> 
    king_check_cury cury curx color board
  | Some p -> 
    if (Piece.get_rank p) = King && (Piece.get_color p) = color 
    then Some p 
    else
      king_check_cury cury curx color board

let get_king color board = 
  get_king_aux 0 0 color board

let rec prune_locs locs acc =
  match locs with
  | [] -> acc
  | (r,c)::t -> 
    if r>7 || c>7 || r<0 || c<0 
    then prune_locs t acc
    else prune_locs t ((r,c):: acc)