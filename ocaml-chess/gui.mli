(** A terminal representation of the chess game GUI, made up of print functions 
    to display the board state and user commands *)

(** [print_board st p] is a terminal representation of the board of state [st] 
    with the current player being [p].
    Requires: [st] be a valid state and [p] a valid current player *)
val print_board : State.t -> State.current_player -> unit 

(** [print_captured st] is a terminal representation of the captured pieces 
    in the board of state [st].
    Requires: [st] be a valid state *)
val print_captured: State.t -> unit

(** [print_quit ()] displays the message when a player quits the game. *)
val print_quit : unit -> unit 

(** [print_instructions ()] displayes the instructions to the players. *)
val print_instructions: unit -> unit

(** [print_welcome ()] displays the welcome message to the players. *)
val print_welcome : unit -> unit 