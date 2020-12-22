(** Move is the representation of the validity of each chess piece movement.*)

(** The type [outcome] represents a Valid move or Invalid move.*)
type outcome = Invalid of string | Valid of Board.board 

(** [clear_path piece loc board] returns true if there is a clear path 
    between the piece [piece] and location [loc] on the board [board] and false
    otherwise. 
    Requires:
    - [piece] is a valid Piece
    - [loc] is a valid Piece.loc 
    - [board] is a valid Board *)
val clear_path : Piece.t -> Piece.loc -> Board.board -> bool

(** [move_piece piece locs board] is the outcome of trying to move [piece] 
    to the destination in [locs] on [board]
    Requires:
    - [piece] is a valid Piece
    - [locs] is a valid list of a location on the [board]
    - [board] is a valid Board*)
val move_piece : Piece.t -> string list-> Board.board -> outcome

(** [valid_move piece dst board] is true if and only if [piece] can move to
    [dst] according to the pieces movement and capture abilities and false
    otherwise.
    Requires:
    - [piece] is a valid Piece
    - [dst] is a valid Piece.loc destination on the [board]
    - [board] is a valid Board*)
val valid_move : Piece.t -> Piece.loc -> Board.board -> bool

(** [valid_move_board piece dst state] is the board after [piece] moves to [dst]
    Raises: Invalid_Move when [piece] cannot legally move to [dst] in [state]
    Requires:
    - [piece] is a valid Piece
    - [dst] is a valid Piece.loc destination on the [board]
    - [board] is a valid Board*)
val valid_move_board : Piece.t -> Piece.loc -> Board.board -> Board.board

(** [in_check playerColor board] is true when [playerColor] is in check on
    [board] and false otherwise.
    Requires:
    - [playerColor] is a valid Piece.color
    - [board] is a valid Board*)
val in_check : Piece.color -> Board.board -> bool

(** [char_to_int char] returns the int [char] represents.
    Requires:
    - [char] is a valid char*)
val char_to_int : char -> int