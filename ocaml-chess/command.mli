(** Commmand is the representation of the user input command to play 
    the chess game. *)

(** The type [position_phrase] is the position phrase that is part of a players 
    command. The first element of the list represents a position on the 
    board that the piece the player is trying to manipulate is located. The 
    second element is the position on the board to which the piece will move to.
    Thus, each position must contain no internal and trailing spaces.
    For example:
    - If the player command is ["move e1 e2"], then the position phrase is
      ["e1"; "e2"].

    An [position_phrase] cannot be an empty list and must have two elements. *)
type position_phrase = string list

(** The type [command] represents a player command that is decomposed into a 
    a verb and possible a [position_phrase] *)
type command = 
  | Move of position_phrase
  | Concede
  | Quit
  | Draw

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command] as follows. The first 
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the position phrase.
    Examples:
    - [parse "move e4 e5"] is [Move ["e4"; "e5"]]

    Requires: [str] contains only lower case alphanumeric and space characters 
    (only ASCII character code 32). 

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if 
    the verb is not "move", "quit", "draw" or "concede", or if the 
    verb is "quit", "draw" or "concede" and there is a non-empty position 
    phrase, if the verb is "move", and the position phrase is empty or does not 
    contain two positions.
*)
val parse : string -> command