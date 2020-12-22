(** State is the representation of the chess game at any given stage of the
    game. *)

(** This type represents the state of the game, which includes
    the data stored at each stage of the chess game. These stages can be
    before a player makes a move, after a player makes a move, or when 
    a pawn is promoted. *)
type t

(** The type of the board*)
type board = Board.board

(**  The type of the captured pieces at the current state of the game*)
type captured_pieces = Piece.t list

(**  The type of the valid commmands that have already occurred at the current 
     state of the game. *)
type move_history = Command.command list

(** The type of the winner at the current state of the game. *)
type winner = string

(** The type of the player whose turn it is in the game. *)
type current_player = Piece.color

(** The type that is returned after executing a command on a state. *)
type result = Illegal of string | Legal of t

(** The type of the states win state. *)
type win = Winner of Piece.color option | Draw

(** the type representing if a draw has been requested this turn. *)
type draw_requested = bool

(** the type repsentation of a pawn that is in en passant. *)
type en_passant = (Piece.t * Piece.loc) option

(** the type representation of if a king has moved the first value 
    represents the white player and the second value is the black player. *)
type king_notmoved = bool * bool 

(** the type representation if a rook has moved the first tuple is the white,
    the second tuple is the black player, with the first value of a tuple being 
    the rook on the queen side the second value is the rook on the king side.  *)
type rook_notmoved = (bool * bool) * (bool * bool)


(** [init_state ()] returns the initial state of the game, with the initial 
    board setup and the current player being white. 
*)
val init_state : unit -> t

(**[get_board t] returns the current board in the game
   Requires:
   - [t] is a valid state of State.t type**)
val get_board : t -> Board.board

(**[get_captured t] returns all of the captured pieces in the game 
   Requires:
   - [t] is a valid state of State.t type* *)
val get_captured : t -> Piece.t list

(** [flip_player c t] is the state with [current_player] being the opposite 
    as [c] and all other fields beging the same as [t] 
    Requires:
    - [c] is a valid color of Piece.color type
    - [t] is a valid state of State.t type**)
val flip_player: Piece.color -> t -> t

(** [get_prev_moves t] returns the previous moves made in the game
    Requires:
    - [t] is a valid state of State.t type**)
val get_prev_moves : t -> Command.command list

(**[get_winner t]  returns the current winner of the game
   Requires:
   - [t] is a valid state of State.t type**)
val get_winner : t -> win

(**[get_winner t]  returns the current winner in string form of the game
   Requires:
   - [t] is a valid state of State.t type**)
val get_winner_string : t -> string 

(**[get_cur_player t] returns the current player of the game 
   Requires:
   - [t] is a valid state of State.t type**)
val get_cur_player : t -> current_player

(**[get_draw_requested t] returns if a draw was requested or not 
   Requires:
   - [t] is a valid state of State.t type**)
val get_draw_requested : t -> draw_requested

(**[get_promotion t] returns the promoted pawn option
   Requires:
   - [t] is a valid state of State.t type*)
val get_promotion: t -> Piece.t option 

(** [set_board board state] returns a state with [board]. 
    Requires:
    - [board] is a valid board of Board.board type 
    - [state] is a valid state of State.t type*)
val set_board : Board.board -> t -> t

(** [set_captured lst state] returns a state with captured pieces [lst]. 
    Requires:
    - [lst] is a valid list of pieces of type Piece.t
    - [t] is a valid state of State.t type*)
val set_captured : Piece.t list -> t -> t

(** [set_prev_moves lst state] returns a state with [move_history] as [lst].
    Requires:
    - [lst] is a valid list of commands of type Command.command
    - [t] is a valid state of State.t type*)
val set_prev_moves : Command.command list -> t -> t

(** [set_cur_player p state] returns a state with [current_player] as [p]. 
    Requires:
    - [p] is a valid current player type 
    - [t] is a valid state of State.t type*)
val set_cur_player : current_player -> t -> t

(** [set_draw_requested b state] returns a state with [draw_requested] as 
    [b].
    Requires:
    - [b] is a valid bool type
    - [t] is a valid state of State.t type*)
val set_draw_requested : bool -> t -> t

(** [set_promotion p state]returns a state with [promotion] as [p].
    Requires:
    - [p] is a valid Piece.t option type
    - [t] is a valid state of State.t type*)
val set_promotion: Piece.t option -> t -> t

(** [set_winner p state] returns a state with [winner] as [p].
    Requires:
    - [p] is a valid win type
    - [t] is a valid state of State.t type *)
val set_winner: win -> t -> t

(** [move_piece p lst state color] returns a [result] after attempting to move 
    piece [p] in the the [board] of [state] by player [color]
    using the command [lst].
    Requires:
    - [p] is a valid piece of Piece.t type 
    - [lst] is a list of valid strings 
    - [t] is a valid state of State.t type
    - [color] is a valid color of type Piece.color*)
val move_piece : Piece.t -> string list -> t -> Piece.color -> result

(** [piece_at_loc str state] checks if the [board] of [state] has a chess piece
    at location [str].
    Requires:
    - [str] is a valid string 
    - [t]  is a valid state of State.t type *)
val piece_at_loc : string -> t -> Piece.t option

(** [update_winner board] is the winner of the given [board]
    Requires:
    - [board] is a valid board of type Board.board*)
val update_winner : Board.board -> win 