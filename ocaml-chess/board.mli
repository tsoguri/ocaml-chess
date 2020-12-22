(** Board is the representation of the chess board at any given time.*)

(** The array [[Piece x;...; Piece y]; ...;[Piece t;...; Piece z]]represents the
     chess board with bottom left position being 00 and top right position as 
     77. The first value is for the row number, and the second value is for the 
     column number. The double array is populated with piece of [Piece], where 
     None represents an empty cell in the board and a Piece of _ is that kind 
     of piece in the board
*)
type board = Piece.t option array array

(** [init_board] creates a 2 dimensional array chess board in its initial state
    as per the official rules of chess, with rows 0-1 for white pieces, and 6-7 
    for black pieces. In the 2-dimensional array, the first value stands for the 
    row of the chess board, while the second value stands for the column. *)
val init_board : unit -> board

(** [get_piece] retrieves the piece at the position ([row], [col])
    Requires: 
    - [row] is an integer 0..7
    - [col] is an integer 0..7
    - [b] is a valid board *)
val get_piece : int -> int -> board -> Piece.t option

(** [get_piece_tuple tuple] retrieves the piece at the position [tuple] where 
    [tuple] is of structure (a,b)
    Requires: 
    - [a] is an integer 0..7
    - [b] is an integer 0..7
    - [b] is a valid board  *)
val get_piece_tuple : (int * int) -> board -> Piece.t option


(** [is_piece_at_tuple] returns true if the piece [p] is at the position 
    (a, b) on the board, and false if otherwise.
    Requires: 
    - [row] is an integer 0..7
    - [col] is an integer 0..7
    - [p] is a valid piece *)
val is_piece_at_tuple : (int * int) -> board -> Piece.t option -> bool

(** [remove_piece p b] removes piece [p] from board [b]  *)
val remove_piece : Piece.t -> 'a option array array -> unit

(** [add_piece p d b] adds piece [p] to board [b] at destination [p] *)
val add_piece : Piece.t -> int * int -> Piece.t option array array -> unit

(** [update p dst b] inserts [p] into [b] at [dst] and removes any piece
    that was there already 
    Requires:
    [p] is a valid piece
    [dst] is a valid destination 
    [b] is a valid board*)
val update : Piece.t -> (int * int) -> board -> unit

(** [get_king color b] returns the piece King of color [c] on board [b]. 
    If no such King exists, it returns None. 
    Requies :
    - [color] is a valid color (white or black)
    - [b] is a valid board *)
val get_king : Piece.color -> board -> Piece.t option

(** [copy_board b] returns a copy of board b*)
val copy_board : board -> board

(** [prune_locs locs acc] removes any locs in [locs] that would not be
    on the board *)
val prune_locs: (int * int) list -> (int * int) list -> (int * int) list