open Piece
open State

(** [piece_str r c s] outputs the string representation of the piece at 
    location row [r], col [c] inside the board of state [s]. *)
let piece_str row col state = 
  match Board.get_piece row col (get_board state) with 
  | None -> " "
  | Some piece -> (get_print_rank (get_rank piece) (get_color piece))

(** [print_row r c s af] outputs the string representation of a row [r] 
    of a board of state [s]. [af] is a accumulation function that determines
    if the board display has white on the botttom or black on the bottom.*)
let rec print_row row col state acc_func = 
  match acc_func 1 with 
  | 1 -> 
    begin
      match col with 
      | 0 -> print_endline ((piece_str row (7-col) state) ^ "|");
      | c -> print_string ((piece_str row (7-c) state) ^ "|"); 
        print_row row (col - 1) state acc_func
    end 
  | x -> 
    begin
      match col with 
      | 7 -> print_endline ((piece_str row (7-col) state) ^ "|");
      | c -> print_string ((piece_str row (7-c) state) ^ "|"); 
        print_row row (col + 1) state acc_func
    end 

(** [print_board_helper st acc af] outputs the string representation of an 
    entire board of state [s]. [acc] represents the row number, and [af] is the 
    accumulation function that determines if the board display has white on the 
    bottom or black on the bottom. *)
let rec print_board_helper state acc acc_func = 
  if acc <> -1 then print_string ((string_of_int (acc_func acc + 1)) ^ "|");
  match acc, acc_func 1 with 
  | (-1), 1 -> print_endline "  a b c d e f g h";
  | (-1), x -> print_endline "  h g f e d c b a";
  | a, b-> print_row (acc_func acc) (acc_func 7) state acc_func; 
    print_board_helper state (a-1) acc_func

(** [captured cap w b c] displays the captured pieces of team color [c]. It 
    outputs a string of pieces that were captured.*)
let rec captured cap white black color =
  match cap with 
  | [] -> if color = White then white else black
  | h :: t -> 
    match get_color h with 
    | Black -> 
      captured t white 
        ((get_print_rank (get_rank h) (get_color h)) ^ " " ^ black) color
    | White -> captured t 
                 ((get_print_rank (get_rank h) (get_color h)) ^ " " ^ white)
                 black color

(** [print_captured st] displays all of the captured pieces split up into 
    captured pices of the white team and black team, with their proper labels
    in the terminal.*)
let print_captured state = 
  print_endline ("Captured Pieces");
  print_endline ("White Team: " ^ (captured (get_captured state) "" "" Black));
  print_endline ("Black Team: " ^ (captured (get_captured state) "" "" White));
  print_endline ""

(** [print_board st p] is a terminal representation of the board of state [st] 
    with the current player being [p].*)
let print_board state curr_player =
  print_captured state;
  match curr_player with 
  | White -> print_board_helper state 7 (fun x -> x)
  | Black -> print_board_helper state 7 (fun x -> (7-x))

(** [print_quit ()] displays the message when a player quits the game. *)
let print_quit () = 
  print_endline ("Thanks for playing!");
  Stdlib.exit 0

(** [print_instructions ()] displayes the instructions to the players. *)
let print_instructions () = 
  print_endline("
  This is a two-player, interactive game.
  Grab a friend and come join in on the fun!

  The objective of this game is to checkmate the opponent's King. This is
  done by moving the pieces of your color across the board, capturing
  your opponent's other pieces. These pieces all have restricted
  which are outlined in resources such as Chess.com.

  While playing this game, all standard chess rules apply. The player is
  able to move their piece by typing out the appropriate command. 
  The player must abide by all correct piece selections and movements.
  If the player chooses an invalid piece or location, the player will be
  notified by a short error message displayed on top of the board.

  We have implemented the special chess rules of Promotion, Casteling, and 
  En Passant. Castling and En Passant will happen automatically, but 
  once a pawn is promoted, the game will prompt you to choose either a 
  Bishop, Knight, Rook or Queen to take its place. 

  At any point in the game you can quit the game. If it is your turn, you can
  also concede, or request a draw to your opponent. If you concede, your 
  opponent will automatically win. However, if you request a draw, your
  opponent can either accept or deny your draw. If they choose to accept, the
  game will end in a draw. If not, the game will continue. You can only 
  request a draw once in your turn.

  This game can either end in a quit, draw, concession, or if you have 
  successfully checkmated your opponent! The game will warn you if your
  king movement, or lack of king movement, will put you in check. 
  However, once checkmate occurs, the game will automatically end, declaring
  a winner.

  Here are the commands avaiable to you: 

    - quit 
    - move a0 a2 
      (a0 will indicate the location of the piece you would like to move on the 
      board with 'a' being the column letter and '0' the row number. a2 will 
      indicate the location of the destination on the chess board where you 
      would like your piece to move to, once again 'a' being the column letter 
      and '2' the row number)
    - concede 
    - draw 

  Note:
  ♙ is a white piece 
  ♟︎ is a black piece

  Happy playing!

  Note: Please use the numbering given on the chess board to dictate your 
  choices. The game will prompt  you for any necessary user input as you play.

  ")
(** [print_welcome ()] displays the welcome message to the players. *)
let print_welcome () = 
  ANSITerminal.(print_string [red]"\n\nWelcome to the 3110 Chess Game.\n");
