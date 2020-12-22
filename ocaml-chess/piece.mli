(** 
   Representation of individual piece data.
   This module represents the data stored in an individual piece.
*)

(** The abstract type of values representing pieces. *)
type t

(** The type of piece ranks. *)
type rank = King | Queen | Rook | Bishop | Knight | Pawn

(** The type of piece loactions on a board. *)
type loc = (int * int)

(** The type of piece colors. *)
type color = White | Black

(** [from_tuple tup] is the piece that [tup] represents.
    Requires: [tup] is a valid piece representation. *)
val from_tuple : (rank * loc * color) -> t

(** [get_rank a] is the rank of [a]. *)
val get_rank : t -> rank

(** [pos a] is the location of [a] in "cartesian" coordinates.
    Example: If [a] is located at d2, [pos a] is (3 , 1)*)
val pos : t -> loc

(** [set_rank piece string] is the piece [piece] with rank changed to 
    the rank representation of [string] *)
val set_rank: t -> string -> t

(** [get_color a] is the color of [a]
    Outputs either "White" or "Black"*)
val get_color : t -> color

(** [all_moves a] is a list of locations [a] can move to without taking into
    account other pieces. For pieces like the Rook, Bishop, and Queen with 
    infinite directional movement, only locations within 0..7 by 0..7 board will
    be returned. For all other pieces all moves will be returned regardless of
    board length validity. *)
val all_moves : t -> loc list

(** [get_print_rank r c] returns the string representation of a [piece]
    with the rank [r] and color [c] *)
val get_print_rank : rank -> color -> string

(** [get_print_color c] is the string representation of color [c]. *)
val get_print_color : color -> string

(** [get_ste_pos p] is the string representation of the position of piece [p]. 
*)
val get_str_pos: t -> string

(** [get_str_tuple pos] is the string respresentation of [pos]. *)
val get_str_tuple: (int*int) -> string
