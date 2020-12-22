open State
open Command
open Board
open Piece
open Gui
open Graphics
open Gui2

(** [format input] formats the string [input], trimming the white space and 
    making it all lowercase *)
let format input = input |> String.trim |> String.lowercase_ascii

(** [color_of_string str] returns the piece color that [str] represents. *)
let color_of_string str =
  match format str with 
  | "white" -> White 
  | "black" -> Black
  | _ -> failwith "invalid color"

(** [print_winner] prints the winner of the game. *)
let print_winner state = function 
  | Winner None -> failwith "error"
  | Winner Some color -> 
    if color = Black then print_string "Black wins!!! \n" 
    else print_string "White wins!!! \n";
    print_board state color
  | Draw -> print_string "The game has ended in a draw."

(** [check_valid_promotion str] returns true if [str] is a valid rank of a
    chess piece. *)
let check_valid_promotion str = 
  str = "rook" || str = "queen" || str = "bishop" || str = "knight"

(** [check_winner current_state] check if [current_state] has a winner and
    returns true if so false otherwise. *)
let check_winner current_state = 
  match get_winner current_state with 
  | Winner (Some _) -> true 
  | Draw -> true
  | _ -> false

(** [swap_players state] changes the player of [state] to the other player. *)
let swap_players state = 
  let current_player = get_cur_player state in 
  flip_player current_player state

(** [promote_action current_player] prompts the [current_player] to promote a
    pawn if needed. *)
let rec promote_action current_player = 
  print_endline "";
  (if current_player = Black then 
     print_endline "\n Black player, you have a pawn to promote.\n Please enter
     what piece you want to promote your pawn to."
   else print_endline"\n White player, you have a pawn to promote.\n Please
    enter what piece you want to promote your pawn to");
  print_string "> ";
  match read_line () with 
  | exception End_of_file -> "end of file"
  | string when format string = "quit" -> "quit"
  | string -> if check_valid_promotion (format string) then format string else
      begin 
        print_endline "\n That is not a valid promotion option.";
        promote_action current_player
      end

(** [command_action current_state] prompts the current player to input a 
    command*)
let rec command_action current_state = 
  print_endline "";
  let current_player = get_cur_player current_state in 
  print_board current_state current_player;
  (if current_player = Black then 
     print_endline "\n Black player, enter a command."
   else print_endline"\n White player, enter a command.");
  print_string "> ";
  match read_line () with 
  | exception End_of_file -> ()
  | command -> check_command current_state command

(** [handle_draw] prompts the other user to agree to a draw or not. *)
and handle_draw current_state = 
  match read_line () with 
  | exception End_of_file -> () 
  | s -> if format s = "yes" || format s = "yes" 
    then print_endline "Game has ended in a draw"
    else print_endline "The draw request has failed plese input a new command.
     \n";
    print_endline "> "; 
    let new_state = set_draw_requested true current_state in
    command_action new_state

(** [check_command command] prompts the user to reinput a [command] if 
    [parse command] raises [Malformed] or [Empty], or it changes the 
    state based on the command. *)
and check_command current_state command = 
  let current_player = get_cur_player current_state in
  match parse (format command) with 
  | exception Malformed -> 
    print_endline "You have input a malformed command. Input a new command.";
    command_action current_state
  | exception Empty -> 
    print_endline 
      "You have not input a command. Input a new command.";
    command_action current_state
  | Quit -> print_quit ();
  | Concede -> print_endline "You have conceded. \n";
    if current_player = Black then print_endline "White wins!!!. \n"
    else print_endline "Black wins!!!. \n"
  | Draw -> print_draw current_state
  | Move (loc1 :: loc2)  -> handle_move loc1 loc2 current_state current_player
  | _ -> failwith "impossible"

(** [print_draw s] handles the draw command in terminal by checking if a 
    draw has been requested thsi turn. *)
and print_draw current_state = 
  if not (get_draw_requested current_state) then
    (
      print_endline "You have requested a draw. \n"; 
      print_endline 
        "Other player, do you accept the draw? Please input yes or no";
      print_endline "< "; 
      handle_draw current_state)
  else (print_endline "A draw has already been requested this turn. \n";
        print_endline "Input a new command. \n"; 
        command_action current_state)

(** [handle_move l1 l2 s p] handles the move command in terminal by checking
    if there is a piece at l1. *)
and handle_move loc1 loc2 current_state current_player = 
  match piece_at_loc (correct_loc loc1) current_state with 
  | None -> 
    print_endline 
      "There is no piece there. Please input a new command.";
    command_action current_state
  | Some p -> handle_result current_state 
                (move_piece p (correct_loclist loc2)
                   current_state current_player)

(** [correct_loc loc] returns the string representation of [loc] where the 
    first letter of [loc] is replaced by its repective number. *)
and correct_loc loc = 
  let new_row = int_of_char loc.[1] - 49 in 
  match loc.[0] with 
  | 'a' -> (string_of_int new_row) ^ "0"
  | 'b' -> (string_of_int new_row) ^ "1"
  | 'c' -> (string_of_int new_row) ^ "2"
  | 'd' -> (string_of_int new_row) ^ "3"
  | 'e' ->(string_of_int new_row) ^ "4"
  | 'f' -> (string_of_int new_row) ^ "5"
  | 'g' -> (string_of_int new_row) ^ "6"
  | 'h' -> (string_of_int new_row) ^ "7"
  | x -> "00"

(** [correct_loclist loc] returns the [correct_loc] represenatation of the 
    strings in [loc]. *)
and correct_loclist loc = 
  match loc with 
  | [loc] -> [correct_loc loc]
  | _ -> ["00"]

(** [handle_result current_state] checks whether the command the user input is
    valid for the [current state] and prompts the user to input a new one if 
    not. 
    Otherwise it changes the state based on the result and checks for a 
    winner. *)
and handle_result current_state = function 
  | Illegal x -> print_endline x; command_action current_state
  | Legal new_state -> begin
      match get_promotion new_state, check_winner new_state with 
      | _, true -> print_winner new_state (get_winner new_state)
      | None, false -> begin
          let s = swap_players new_state in 
          command_action s 
        end
      | Some p, false -> begin
          handle_promotion new_state p;
          let new_s = swap_players new_state in 
          command_action new_s
        end
    end

(** [handle_promotion state piece] updates [state] by promoting a pawn to 
    [piece] which is the rank of the piece the pawn is to promoted to. *)
and handle_promotion state piece = 
  let piece_string = promote_action (get_cur_player state) in 
  if piece_string = "quit" then print_quit () else begin
    let new_piece = set_rank piece piece_string in 
    remove_piece piece (get_board state);
    let dst = Piece.pos new_piece in 
    add_piece new_piece dst (get_board state)
  end

(** [play_game] initializes the game state and determines which player 
    goes first and takes in the very first command. *)
let init_play_game = function 
  | White -> print_endline "You are going first. \n"; 
    command_action (init_state ())
  | Black -> print_endline "Other player is going first. \n";
    command_action (init_state ())

(** [check_valid_color] prompts the user to input a new color if it raises
    a [Failure]. *)
let rec check_valid_color () = 
  match read_line () with 
  | exception End_of_file -> ()
  | color -> if format color = "quit" then print_quit ()
    else begin
      match color_of_string color with 
      | exception _  -> 
        print_string "\nColor was invalid. Please input a new color. \n";
        print_string "> " |> check_valid_color 
      | _ -> init_play_game (color_of_string color) 
    end

(** [main ()] prompts for the game to play, then starts it. *)
let main_terminal () =
  print_welcome ();
  print_instructions ();
  print_endline "
  Please enter the color of the chess player you would like to
  be, which is either black or white. Please type in either 'white' or 
  'black'. \n";
  print_string  "> ";
  check_valid_color ()

(** [main_giu ()] prompts the game with the gui to start. *)
let main_gui () =
  open_graph " 600x900";
  start ();
  while true do () done

(** [main ()] prompts the game to start. *)
let main () = 
  print_string
    "\nWould you like to play in the terminal or in an XQuartz GUI?";
  print_endline " Please type 'y' for GUI and 'n' for terminal.";
  let rec gui_or_terminal () = 
    match format (read_line ()) with 
    | "y" -> main_gui ()
    | "n" -> main_terminal ()
    | "quit" -> print_quit ()
    | x -> print_endline "Please type again!" |> gui_or_terminal 
  in gui_or_terminal ()

(* Execute the game engine. *)
let () = main ()