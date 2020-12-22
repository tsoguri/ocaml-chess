
type rank = King | Queen | Rook | Bishop | Knight | Pawn
type loc = (int * int)
type color = White | Black

type t = {
  rank : rank;
  position : loc;
  color : color;
}

(** [get_row loc] is the row of [loc]. *)
let get_row loc =
  match loc with
  | (r,c) -> r

(** [get_col loc] is the colum of [loc]. *)
let get_col loc =
  match loc with
  | (r,c) -> c

let set_rank piece string = 
  match string with 
  | "rook" -> {piece with rank = Rook}
  | "knight" -> {piece with rank = Knight}
  | "queen" -> {piece with rank = Queen}
  | "bishop" -> {piece with rank = Bishop}
  | _ -> failwith "invalid piece"

let from_tuple (tup : (rank*loc*color)) : t = 
  match tup with
  | (r,l,c) -> {rank = r; position = l; color = c;}

let get_rank (p : t) : rank =
  p.rank

let pos (p : t) : loc =
  p.position

let get_str_pos (p : t) : string = 
  match p.position with 
  | (a,b) -> "(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")"

let get_str_tuple (a,b) : string =
  "(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")"

let get_color (p : t) : color =
  p.color

let get_print_color c : string =
  match c with 
  | Black -> "Black"
  | White -> "White"

let get_print_rank r c : string =
  match r, c with
  | Pawn, White -> "♙"
  | Pawn, Black -> "♟︎"
  | Knight, White -> "♘"
  | Knight, Black -> "♞"
  | Bishop, White -> "♗"
  | Bishop, Black -> "♝"
  | Rook, White -> "♖"
  | Rook, Black ->  "♜"
  | Queen, White -> "♕"
  | Queen, Black -> "♛"
  | King, White -> "♔"
  | King, Black -> "♚"

(** [pawn_movement_helper r c p_color] is a list of the locations that the piece
    with rank pawn and color [p_color] at postion [(r, c)] can move to. *)
let pawn_movement_helper row col (p_color : color) =
  if p_color = White
  then let lst = [(row + 1, col)] in
    if row = 1
    then (row + 2, col) :: lst
    else lst
  else let lst = [(row - 1, col)] in
    if row = 6
    then (row - 2, col) :: lst
    else lst

(** [knight_move_helper r c] is a list of locations that the
    piece with rank knight at positon [(r, c)] can move to. *)
let knight_movement_helper row col = 
  [
    (row + 2, col + 1); (row + 2, col - 1); (** Up and to the sides *)
    (row + 1, col + 2); (row + 1, col - 2); (** To the sides and up *)
    (row - 1, col - 2); (row - 1, col + 2); (** To the sides and down *)
    (row - 2, col - 1); (row - 2, col + 1); (** Down and to the sides *)
  ]

(** [directional_positions loc acc transform] constructs a list of locations 
    based on [transform] from location [loc] to the ends of the chess board,
    which are (0, 0), (0, 7), (7, 0), (7, 7). *)
let rec directional_positions loc acc transform =
  let loc = transform loc in
  let row = get_row loc in
  let col = get_col loc in
  if row < 0 || col < 0 || row > 7 || col > 7
  then acc
  else loc :: (directional_positions loc acc transform)

(** [bishop_movement_helper loc] is the list of locations that a piece of rank 
    bishop at location [loc] can move to. *)
let bishop_movement_helper (p_loc : loc) : loc list =
  directional_positions p_loc [] (fun loc ->
      match loc with | (r,c) -> (r+1, c+1)) @
  directional_positions p_loc [] (fun loc ->
      match loc with | (r,c) -> (r+1, c-1)) @
  directional_positions p_loc [] (fun loc ->
      match loc with | (r,c) -> (r-1, c+1)) @
  directional_positions p_loc [] (fun loc -> 
      match loc with | (r,c) -> (r-1, c-1))

(** [rook_movement_helper loc] is the list of locations that a piece of rank  
    rook at location [loc] can move to. *)
let rook_movement_helper (p_loc : loc) : loc list =
  directional_positions p_loc [] (fun loc ->
      match loc with | (r,c) -> (r, c+1)) @
  directional_positions p_loc [] (fun loc ->
      match loc with | (r,c) -> (r, c-1)) @
  directional_positions p_loc [] (fun loc ->
      match loc with | (r,c) -> (r-1, c)) @
  directional_positions p_loc [] (fun loc -> 
      match loc with | (r,c) -> (r+1, c))

(** [queen_movement_helper loc] is the list of locations that a piece of rank
    queen at location [loc] can move to. *)
let queen_movement_helper (p_loc : loc) : loc list = 
  List.rev_append (rook_movement_helper p_loc) (bishop_movement_helper p_loc)

(** [king_movement_helper r c] is the list of locations that a piece of rank
    queen at location [(r, c)] can move to. *)
let king_movement_helper row col : loc list =
  [
    (row + 1, col); (row + 1, col - 1); (row + 1, col + 1); (** Up *)
    (row, col + 1); (row, col - 1); (** To the sides*)
    (row - 1, col); (row - 1, col - 1); (row - 1, col + 1); (** Down *)
  ]

let all_moves (p : t) : loc list =
  let row = get_row p.position in
  let col = get_col p.position in
  match p.rank with
  | Pawn -> pawn_movement_helper row col p.color
  | Knight -> knight_movement_helper row col
  | Bishop -> bishop_movement_helper p.position
  | Rook -> rook_movement_helper p.position
  | Queen -> queen_movement_helper p.position
  | King -> king_movement_helper row col