(**
   Test plan: 
   We did a mix of testing with OUnit test cases vs manual game testing. 
   Things tested with OUnit:
   - valid command
   - creating valid boards
   - valid moves for each different piece (piece logic)
   - movement in check
   - checking for a winner, or a draw
   - update winner
   - promotion logic
   - en passant logic
   - castling logic

   However, the way that we tested each of those aspects was by loading a
   board of some game state from a .txt file into our test cases. So, the OUnit 
   test cases guarantee that those pieces of logic work separately given a game 
   state, but we also needed to test that the pieces could work in tandem to 
   get to said game state. 

   This is what we tested manually by playing the game, and getting to a 
   specific board state. Theoretically, if all of the logic works 
   separately, it should hopefully work together. But, we wanted to test our 
   main module and make sure it tied the game together properly.
   This is the extra manual testing we performed. In summary, the modules
   Piece, Board, Command, State, and Move were all tested with OUnit, and Main,
   Gui, and Gui2 were tested manually. 

   In testing manually, we ran the game in both the terminal Gui and XQuartz 
   Gui to test its functionalities.
   Things that we tested manually:
   - Piece movements and chess logic in tandem (as outlined above)
   - Handling of user commands to move a chess piece 
   - Terminal and XQuartz outputs for each piece movement, state 
      and board updates, and winner and draw declarations
   - User input when promoting a pawn 
   - User inputs when drawing, conceding, and quitting

   Our testing logic mainly relied on black box testing, where we used what 
   we knew about the rules of chess and piece movement in order to develop 
   tests that would guarantee that our game works. The reason we mainly went
    with black box testing was because no matter what our internal 
   implementation was, at the end of the day, the user needs to be able to play 
   the game of chess and move pieces properly. 
   Similarly, the game needs to catch any illegal moves that 
   the player may try to make. The way to ensure this was to go through each 
   possible piece movement or error that can occur, given our specifications,
   and create a test based on that. However, we furthered our testing by 
   implementing glass box testing as well. We utilized our knowledge of the 
   implementation in testing our Main, Gui, and Gui2 modules. In our manual 
   tests, we tested all of the execution paths of what the user could input 
   (different commands, not standard spacing, capitalization, etc.), clicking
   empty spaces in the window, and made sure we handled them accordingly. 
   We also used glass-box testing when testing our checkmate logic. When 
   testing our checkmate logic, we first began with black-box testing using
   our understanding of the chess game. However, when we ran into an error,
   we implemented glass-box testing to look at all of the paths that our 
   checkmate logic goes through and identified the branch that was causing 
   the error. 

   Overall, our test suite in the test module and our manual testing implies 
   correctness of our game. In our test module we import different board states 
   from .txt files, and perform each type of chess piece movement on it, where 
   we can see that all of the tests pass. This guarantees that our 
   individualized logic of each movement is correct. The only thing not
   guaranteed by our test file is the GUI, and the flow of our game, all
   of which was tested manually. The combination of the OUnit tests and 
   the manual testing proves that our game is correct, both in logic, in the 
   game play, and in the user interface. 
*)

open OUnit2
open Board 
open Piece
open Command
open State
open Gui
open Move

(** [get_piece_test name row col b expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_piece row col b]. *)
let get_piece_test
    (name: string)
    (row: int)
    (col: int)
    (b : board)
    (expected_output : Piece.t option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_piece row col b))

(** [is_piece_at_test name row col b p expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [is_piece_at row col b p]. *)
let is_piece_at_test
    (name: string)
    (row: int)
    (col: int)
    (b : board)
    (p : Piece.t option)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_piece_at_tuple (row,col) b p))

let test_board = init_board ()

let board_tests = 
  [
    get_piece_test "intial board - white queen at 0,3" 0 3 test_board
      (Some (Piece.from_tuple (Queen, (0,3), White)));
    get_piece_test "intial board - white rook at 0,0" 0 0 test_board 
      (Some (Piece.from_tuple (Rook, (0,0), White)));
    get_piece_test "intial board - white pawn at 1,5" 1 5 test_board 
      (Some (Piece.from_tuple (Pawn, (1,5), White)));
    get_piece_test "inital board - empty piece in middle of board" 5 5 
      test_board None;
    get_piece_test "intial board - black queen at 7,3" 7 3 test_board 
      (Some ((Piece.from_tuple(Queen, (7,3), Black))));
    get_piece_test "intial board - black rook at 7,7" 7 7 test_board
      (Some (Piece.from_tuple(Rook, (7,7), Black)));
    get_piece_test "intial board - black at 6,6" 6 6 test_board 
      (Some (Piece.from_tuple(Pawn, (6,6), Black)));

    is_piece_at_test "initial board - white queen at 0,3 true" 0 3 test_board 
      (Some (Piece.from_tuple (Queen, (0,3), White))) true;
    is_piece_at_test "inital board - white queen at 0,4 false" 0 4 test_board 
      (Some (Piece.from_tuple (Queen, (0,4), White))) false;
    is_piece_at_test "inital board - black queen at 7,3 true" 7 3 test_board 
      (Some (Piece.from_tuple(Queen, (7,3), Black))) true;
    is_piece_at_test "inital board - black queen at 5,5 false" 5 5  test_board 
      (Some (Piece.from_tuple(Queen, (6,6), Black))) false;
    is_piece_at_test "inital board - white queen at 1,7 false" 1 7 test_board 
      (Some (Piece.from_tuple (Queen, (1,7), White))) false;
  ]

let loc_to_string loc =
  match loc with
  | (r,c) -> (string_of_int r) ^ ", " ^ (string_of_int c)

let piece_printer p =
  "Rank : " ^ (Piece.get_print_rank (Piece.get_rank p) (Piece.get_color p) ) ^ 
  "; Positon : " ^ (loc_to_string (Piece.pos p)) ^ 
  "; Color : " ^ (Piece.get_print_color (Piece.get_color p))

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [rank_test name p expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [Piece.get_rank p]. *)
let rank_test 
    (name : string)
    (p : Piece.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Piece.get_print_rank (Piece.get_rank p) (Piece.get_color p) ) 
        ~printer:pp_string)

(** [location_test name p expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [Piece.pos p]. *)
let location_test 
    (name : string)
    (p : Piece.t)
    (expected_output : Piece.loc) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Piece.pos p))

(** [rank_test name p expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [Piece.get_color p]. *)
let color_test 
    (name : string)
    (p : Piece.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Piece.get_print_color (Piece.get_color p)) ~printer:pp_string)

(** [movement_test name p expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [Piece.all_moves p]. *)
let movement_test 
    (name : string)
    (p : Piece.t)
    (expected_output : Piece.loc list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (Piece.all_moves p) ~printer:(pp_list loc_to_string))

let piece_tests =
  [
    rank_test "pawn 00 white" (Piece.from_tuple (Pawn, (0,0), White)) "♙";
    location_test "pawn 00 white"
      (Piece.from_tuple (Pawn, (0,0), White)) (0,0);
    color_test "pawn 00 white"
      (Piece.from_tuple (Pawn, (0,0), White)) "White";

    rank_test "knight 55 black" 
      (Piece.from_tuple (Knight, (5,5), Black)) "♞";
    location_test "knight 55 black" 
      (Piece.from_tuple (Knight, (5,5), Black)) (5,5);
    color_test "knight 55 black" 
      (Piece.from_tuple (Knight, (5,5), Black)) "Black";

    movement_test "white pawn not first" 
      (Piece.from_tuple (Pawn, (0,0), White)) [1,0];
    movement_test "black pawn not first" 
      (Piece.from_tuple (Pawn, (7,0), Black)) [6,0];
    movement_test "white pawn first" (
      Piece.from_tuple (Pawn, (1,0), White))[(3,0); (2,0)];
    movement_test "black pawn first" 
      (Piece.from_tuple (Pawn, (6,0), Black)) [(4,0); (5,0)];

    movement_test "left white knight bottom" 
      (Piece.from_tuple (Knight, (0,1), White))
      [(2, 2); (2, 0); (1, 3); (1, -1); (-1, -1); (-1, 3); (-2, 0); (-2, 2)];
    movement_test "right white knight bottom" 
      (Piece.from_tuple (Knight, (0,6),White))
      [(2, 7); (2, 5); (1, 8); (1, 4); (-1, 4); (-1, 8); (-2, 5); (-2, 7)];
    movement_test "3,3 white knight" (Piece.from_tuple (Knight, (3,3), White))
      [(5, 4); (5, 2); (4, 5); (4, 1); (2, 1); (2, 5); (1, 2); (1, 4)];
    movement_test "left white bishop bottom" 
      (Piece.from_tuple (Bishop, (0,2), White))
      [(1, 3); (2, 4); (3, 5); (4, 6); (5, 7); (1, 1); (2, 0)];
    movement_test "right white bishop bottom" 
      (Piece.from_tuple (Bishop, (0,5), White))
      [(1, 6); (2, 7); (1, 4); (2, 3); (3, 2); (4, 1); (5, 0)];
    movement_test "3,3 white bishop" (Piece.from_tuple (Bishop, (3,3), White))
      [(4, 4); (5, 5); (6, 6); (7, 7); (4, 2); (5, 1); (6, 0);
       (2, 4); (1, 5); (0, 6); (2, 2); (1, 1); (0, 0)];
    movement_test "left white rook bottom" 
      (Piece.from_tuple (Rook, (0,0), White))
      [(0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7);
       (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0)];
    movement_test "right white rook bottom" 
      (Piece.from_tuple (Rook, (0,7), White))
      [(0, 6); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1); (0, 0);
       (1, 7); (2, 7); (3, 7); (4, 7); (5, 7); (6, 7); (7, 7)];
    movement_test "3,3 white rook" (Piece.from_tuple (Rook, (3,3), White))
      [(3, 4); (3, 5); (3, 6); (3, 7); (3, 2); (3, 1); (3, 0);
       (2, 3); (1, 3); (0, 3); (4, 3); (5, 3); (6, 3); (7, 3)];
    movement_test "white queen bottom" 
      (Piece.from_tuple (Queen, (0,3), White))
      [(7, 3); (6, 3); (5, 3); (4, 3); (3, 3); (2, 3); (1, 3); (0, 0); 
       (0, 1); (0, 2); (0, 7); (0, 6); (0, 5); (0, 4); (1, 4); (2, 5); 
       (3, 6); (4, 7); (1, 2); (2, 1); (3, 0)];
    movement_test "3,3 white Queen" (Piece.from_tuple (Queen, (3,3), White))
      [(7, 3); (6, 3); (5, 3); (4, 3); (0, 3); (1, 3); (2, 3);
       (3, 0); (3, 1); (3, 2); (3, 7); (3, 6); (3, 5); (3, 4);
       (4, 4); (5, 5); (6, 6); (7, 7); (4, 2); (5, 1); (6, 0);
       (2, 4); (1, 5); (0, 6); (2, 2); (1, 1); (0, 0)];
    movement_test "white king bottom" (Piece.from_tuple (King, (0,4), White))
      [(1, 4); (1, 3); (1, 5); (0, 5); (0, 3); (-1, 4); (-1, 3); (-1, 5)];
    movement_test "3,3 white king" (Piece.from_tuple (King, (3,3), White)) 
      [(4, 3); (4, 2); (4, 4); (3, 4); (3, 2); (2, 3); (2, 2); (2, 4)];
  ]

(** [pp_command c] is the string representation of [c]. *)
let pp_command = function
  | Quit -> "Quit"
  | Concede -> "Concede"
  | Draw -> "Draw"
  | Move a -> "Go " ^ (pp_list pp_string a)

(** [command_test_exn name exn arg] is the OUnit test case named [name] that 
    tests if [exn] is raised by [parse arg]. *)
let command_test_exn (name : string) exn arg =
  name >:: (fun _ -> assert_raises exn (fun () -> parse arg))

(** [command_test name arg expected_output] is the OUnit test case named [name] 
    that tests if [parse arg] is equal to [expected_output]. *)
let command_test (name : string) (arg : string) (expected_output : command) =
  name >:: (fun _ -> 
      assert_equal expected_output (parse arg)  ~printer:pp_command)

let command_tests = 
  [
    command_test_exn "parse empty string raises Empty" Empty "";
    command_test_exn "parse string with spaces raises Empty" Empty " ";
    command_test_exn "parse 'move' raises Malformed" Malformed "move";
    command_test_exn "parse 'move t' raises Malformed" Malformed "move t";
    command_test_exn "parse 'move 23' raises Malformed" Malformed "move 23";
    command_test_exn "parse 'move t 23' raises Malformed" Malformed "move t 23";
    command_test_exn "parse 'move 80 23' raises Malformed" Malformed
      "move 80 23";
    command_test_exn "parse 'move 23 08' raises Malformed" Malformed 
      "move 23 80";
    command_test_exn "parse 'move tt 40' raises Malformed" Malformed
      "move tt 40";
    command_test_exn "parse 'move 40 tt' raises Malformed" Malformed
      "move 40 tt";
    command_test_exn "parse 'move 0 0' raises Malformed" Malformed
      "move 0 0";
    command_test_exn "parse 'move z0 z0' raises Malformed" Malformed
      "move z0 z0";
    command_test_exn "parse 'move a0 a0' raises Malformed" Malformed
      "move a0 a0";
    command_test_exn "parse 'move a9 a9' raises Malformed" Malformed
      "move a9 a9";
    command_test_exn "parse 'move 00 77' raises Malformed" Malformed
      "move 00 77";
    command_test_exn "parse 'move 401 44' raises Malformed" Malformed
      "move 401 44";
    command_test_exn "parse 'bob' is raises Malformed" Malformed  "bob";
    command_test "parse 'move c1 c2' is Move ['c1'; 'c2']" "move c1 c2"
      (Move ["c1"; "c2"]);
    command_test "parse 'move   c2    c3' is Move ['c2'; 'c3']" "move   c8   h8"
      (Move ["c8"; "h8"]);
    command_test "parse 'draw' is Draw" "draw" Draw;
    command_test "parse 'concede' is Concede" "concede" Concede;
    command_test "parse 'quit' is Quit" "quit" Quit;
  ]

let test_board_empty = Array.make_matrix 8 8 (None)

let test_board_1 = 
  let empty = Array.make_matrix 8 8 (None) in 
  empty.(3).(0) <- Some (Piece.from_tuple (Rook, (3, 0), White));
  empty.(5).(5) <- Some (Piece.from_tuple (Rook, (5, 5), Black));
  empty.(5).(0) <- Some (Piece.from_tuple (Queen, (5, 0), White));
  empty.(5).(0) <- Some (Piece.from_tuple (Queen, (3, 0), Black));
  empty.(3).(5) <- Some (Piece.from_tuple (King, (3, 5), Black));
  empty.(5).(3) <- Some (Piece.from_tuple (King, (5, 3), White));
  empty

let test_board_two = 
  let board = (Array.make_matrix 8 8 (None)) in 
  board.(6).(0) <- Some (Piece.from_tuple (Pawn, (6, 0), Black)); board

let test_board_three = 
  let board = Array.make_matrix 8 8 (None) in 
  board.(1).(7) <- Some (Piece.from_tuple (Pawn, (1, 7), White)); board

let test_board_one = test_board_1

(** [valid_move_test name piece dst board expected_output] is the OUnit test 
    case named [name] that asserts if [valid_move piece dst board] is 
    structurally equal to [expected_output]. *)
let valid_move_test (name : string) (piece : Piece.t) (dst : Piece.loc) 
    (board : board) (expected_output : bool) =
  name >:: (fun _ -> 
      assert_equal expected_output (Move.valid_move piece dst board) 
        ~printer:string_of_bool)

let movement_tests = [
  valid_move_test "pawn can't go sideways" 
    (Piece.from_tuple (Pawn, (1,0), White)) (1,1) test_board_empty false;
  valid_move_test "pawn can go straight" 
    (Piece.from_tuple (Pawn, (1,0), White)) (2,0) test_board_empty true;
  valid_move_test "pawn can't go diagonal because nothing there" 
    (Piece.from_tuple (Pawn, (1,0), White)) (2,1) test_board_empty false;
  valid_move_test "pawn can move forward two on first move" 
    (Piece.from_tuple (Pawn, (1,0), White)) (3,0) test_board_empty true;
  valid_move_test "pawn can't go straight more than one" 
    (Piece.from_tuple (Pawn, (2,0), White)) (4,0) test_board_empty false;
  valid_move_test "pawn can't go backwards" 
    (Piece.from_tuple (Pawn, (1,0), White)) (0,0) test_board_empty false;
  valid_move_test "black pawn can't go sideways" 
    (Piece.from_tuple (Pawn, (6,0), Black)) (6,1) test_board_empty false;
  valid_move_test "black pawn can go straight down" 
    (Piece.from_tuple (Pawn, (6,0), Black)) (5,0) test_board_empty true;
  valid_move_test "black pawn can move down two on first turn" (
    Piece.from_tuple (Pawn, (6,0), Black)) (4,0) test_board_empty true;
  valid_move_test "black pawn can't go backwards" 
    (Piece.from_tuple (Pawn, (6,0), Black)) (7,0) test_board_empty false;
  valid_move_test "white bishop can't move forward" 
    (Piece.from_tuple (Bishop, (0,2), White)) (1,2) test_board_empty false;
  valid_move_test "white bishop can move diagonal two spaces" 
    (Piece.from_tuple (Bishop, (0,2), White)) (2,4) test_board_empty true;
  valid_move_test "white bishop can't move diagonal to a piece" 
    (Piece.from_tuple (Bishop, (0,2), White)) (1,3) test_board false;
  valid_move_test "white bishop can move diagonal to (5,7)" 
    (Piece.from_tuple (Bishop, (0,2), White)) (5,7) test_board_empty true;
  valid_move_test "white bishop can't move sideways" 
    (Piece.from_tuple (Bishop, (0,2), White)) (0,3) test_board_empty false;
  valid_move_test "black bishop can't move forward" 
    (Piece.from_tuple (Bishop, (7,2), Black)) (6,2) test_board_empty false;
  valid_move_test "black bishop can move diagonal two spaces" 
    (Piece.from_tuple (Bishop, (7,2), Black)) (5,4) test_board_empty true;
  valid_move_test "black bishop can't move diagonal to a piece" 
    (Piece.from_tuple (Bishop, (7,2), Black)) (6,3) test_board false;
  valid_move_test "black bishop can move diagonal to (2,7)" 
    (Piece.from_tuple (Bishop, (7,2), Black)) (2,7) test_board_empty true;
  valid_move_test "black bishop can't move sideways" 
    (Piece.from_tuple (Bishop, (7,2), Black)) (7,3) test_board_empty false;
  valid_move_test "white knight can move to (2,2) over a pawn" 
    (Piece.from_tuple (Knight, (0,1), White)) (2,2) test_board true;
  valid_move_test "white knight can move to (2,0) over a rook and pawn" 
    (Piece.from_tuple (Knight, (0,1), White)) (2,0) test_board true;
  valid_move_test "white knight can't move only forward" 
    (Piece.from_tuple (Knight, (0,1), White)) (2,1) test_board_empty false;
  valid_move_test "white knight can't move only sideways" 
    (Piece.from_tuple (Knight, (0,1), White)) (0,2) test_board_empty false;
  valid_move_test "black knight can move to (5,5) over a pawn" 
    (Piece.from_tuple (Knight, (7,6), Black)) (5,5) test_board true;
  valid_move_test "black knight can move to (5,7) " 
    (Piece.from_tuple (Knight, (7,6), Black)) (5,7) test_board true;
  valid_move_test "black knight can't move only forward " 
    (Piece.from_tuple (Knight, (7,6), Black)) (5,6) test_board_empty false;
  valid_move_test "black knight can't move to only sideways" 
    (Piece.from_tuple (Knight, (7,6), Black)) (7,7) test_board_empty false;
  valid_move_test "white rook can't move diagonally " 
    (Piece.from_tuple (Rook, (0,0), White)) (1,1) test_board_empty false;
  valid_move_test "white rook can't to a piece" 
    (Piece.from_tuple (Rook, (0,0), White)) (1,0) test_board false;
  valid_move_test "white rook can move to a piece at (6,0)" 
    (Piece.from_tuple (Rook, (0,0), White)) (6,0) test_board_two true;
  valid_move_test "white rook can move sideways" 
    (Piece.from_tuple (Rook, (3,0), White)) (3,7) test_board_empty true;
  valid_move_test "white rook can move backwards" 
    (Piece.from_tuple (Rook, (3,0), White)) (2,0) test_board_empty true;
  valid_move_test "black rook can't move diagonally " 
    (Piece.from_tuple (Rook, (7,7), Black)) (6,6) test_board_empty false;
  valid_move_test "black rook can move to a piece (1,7) " 
    (Piece.from_tuple (Rook, (7,7), Black)) (1,7) test_board_three true;
  valid_move_test "black rook can move sidways " 
    (Piece.from_tuple (Rook, (5,5), Black)) (5,7) test_board_empty true;
  valid_move_test "white queen can move forward " 
    (Piece.from_tuple (Queen, (0,4), White)) (2,4) test_board_empty true;
  valid_move_test "white queen can move diagonal left " 
    (Piece.from_tuple (Queen, (0,4), White)) (2,2) test_board_empty true;
  valid_move_test "white queen can move diagonal right" 
    (Piece.from_tuple (Queen, (0,4), White)) (2,6) test_board_empty true;
  valid_move_test "white queen can't knight movements " 
    (Piece.from_tuple (Queen, (0,4), White)) (2,5) test_board_empty false;
  valid_move_test "white queen can move forward to (6,4) " 
    (Piece.from_tuple (Queen, (0,4), White)) (6,4) test_board_empty true;
  valid_move_test "white queen can move backwards" 
    (Piece.from_tuple (Queen, (5,0), White)) (3,2) test_board_empty true;
  valid_move_test "black queen can move forward to (4,4) " 
    (Piece.from_tuple (Queen, (7,4), Black)) (4,4) test_board_empty true;
  valid_move_test "black queen can move forward to (5,2) " 
    (Piece.from_tuple (Queen, (7,4), Black)) (5,2) test_board_empty true;
  valid_move_test "black queen can move forward to (5,6) " 
    (Piece.from_tuple (Queen, (7,4), Black)) (5,6) test_board_empty true;
  valid_move_test "black queen can't do knight movements " 
    (Piece.from_tuple (Queen, (7,4), Black)) (5,5) test_board_empty false;
  valid_move_test "black queen can move backwards" 
    (Piece.from_tuple (Queen, (3,0), Black)) (5,2) test_board_empty true;
  valid_move_test "white king can move one square vertically " 
    (Piece.from_tuple (King, (3,5), Black)) (4,5) test_board_empty true;
  valid_move_test "white king can move one square horizontally" 
    (Piece.from_tuple (King, (3,5), Black)) (3,6) test_board_empty true;
  valid_move_test "white king can move one square down" 
    (Piece.from_tuple (King, (3,5), Black)) (2,5) test_board_empty true;
  valid_move_test "white king can move one square left" 
    (Piece.from_tuple (King, (3,5), Black)) (3,4) test_board_empty true;
  valid_move_test "white king can move one square diagonally" 
    (Piece.from_tuple (King, (3,5), Black)) (4,6) test_board_empty true;
  valid_move_test "white king can't move two spaces up" 
    (Piece.from_tuple (King, (3,5), Black)) (5,5) test_board_empty false;
  valid_move_test "white king can capture a black pawn"
    (Piece.from_tuple (King, (5, 0), White)) (6, 0) test_board_two true;
  valid_move_test "white pawn can diagonally forward to capture a black pawn"
    (Piece.from_tuple (Pawn, (5, 1), White)) (6, 0) test_board_two true;
  valid_move_test 
    "white pawn cannot move diagonally downward to capture a black pawn" 
    (Piece.from_tuple (Pawn, (7, 1), White)) (6, 0) test_board_two false;
  valid_move_test 
    "black pawn can move diagonally downward to capture a white pawn"
    (Piece.from_tuple (Pawn, (2, 6), Black)) (1, 7) test_board_three true;
  valid_move_test 
    "black pawn cannot move diagonally forward to capture a white pawn"
    (Piece.from_tuple (Pawn, (0, 6), Black)) (1, 7) test_board_three false;
  valid_move_test
    "white pawn cannot move forwards to capture a black pawn"
    (Piece.from_tuple (Pawn, (5, 0), White)) (6, 0) test_board_two false;
  valid_move_test 
    "black pawn cannot move forwards to capture a white pawn"
    (Piece.from_tuple (Pawn, (1, 6), Black)) (1, 7) test_board_three false;
]

(** [string_to_piece s] takes in the string representation of a chess piece
    and returns a piece option and its color *)
let string_to_piece s = 
  match s with 
  | "♙" -> Some Pawn, White
  | "♟" -> Some Pawn, Black
  | "♘" -> Some Knight, White
  | "♞" -> Some Knight, Black
  | "♗" -> Some Bishop, White
  | "♝" -> Some Bishop, Black
  | "♖" -> Some Rook, White
  | "♜" -> Some Rook, Black
  | "♕" -> Some Queen, White
  | "♛" -> Some Queen, Black
  | "♔" -> Some King, White
  | "♚" -> Some King, Black
  | _ -> None, White

(** [stringlst_to_board slst board row col] takes in a string list of pieces
    and updates the board with the string list of pieces for row [row]. *)
let rec stringlst_to_board slst board row col = 
  match slst with
  | [] -> board
  | h :: t ->
    match string_to_piece h with 
    | Some p, c -> 
      board.(row).(col) <- Some (Piece.from_tuple (p, (row, col),c));
      stringlst_to_board t board row (col + 1)
    | None, _ -> board.(row).(col) <- None; 
      stringlst_to_board t board row (col + 1)

(** [safe_input_line chan] returns a new line option of the opened channel *)
let safe_input_line chan =
  try Some (input_line chan) with End_of_file -> None

(** [file_to_board filename] takes in a .txt file named [filename] and returns
    a board representation of the .txt file*)
let file_to_board filename = 
  let f = filename ^ ".txt" in 
  let fp = try "boards" ^ Filename.dir_sep ^ f with _ -> raise Not_found in 
  let chan = open_in fp in 
  let rec loop row acc = 
    match safe_input_line chan with 
    | None -> acc
    | Some line -> 
      let stringlst = String.split_on_char '|' line in
      let new_board = stringlst_to_board stringlst acc row 0 in 
      loop (row - 1) new_board
  in let board' = loop 7 (Array.make_matrix 8 8 (None)) in 
  close_in chan;
  board'

let test_init_board = file_to_board "init_board"

let checkmate_2major = file_to_board "checkmate_2majorbackrank"

let test_checkmate2major = 
  let empty = (Array.make_matrix 8 8 (None)) in 
  empty.(0).(3) <- (Some (Piece.from_tuple (King, (0,3), White)));
  empty.(7).(0) <- (Some (Piece.from_tuple (Rook, (7,0), White)));
  empty.(6).(1) <- (Some (Piece.from_tuple (Rook, (6,1), White)));
  empty.(7).(7) <- (Some (Piece.from_tuple (King, (7,7), Black)));
  empty

(** [txt_import_tests] tests whether or not the .txt files were imported 
    correctly. *)
let txt_import_tests = [
  "initial board import = init board" >::
  (fun _ -> assert_equal test_init_board (Board.init_board ()));

  get_piece_test "intial board - white queen at 0,3"  0 3 
    test_init_board (Some (Piece.from_tuple (Queen, (0,3), White)));

  "checkmate2major = checkmate2major" >::
  (fun _ -> assert_equal checkmate_2major test_checkmate2major);

  get_piece_test "2 major back rank checkmate - white king at 0,3" 0 3 
    checkmate_2major  (Some (Piece.from_tuple (King, (0,3), White)));

  get_piece_test "2 major back rank checkmate - white rook at 5,0" 7 0 
    checkmate_2major  (Some (Piece.from_tuple (Rook, (7,0), White)));

  get_piece_test "2 major back rank checkmate - white rook at 6,1" 6 1 
    checkmate_2major (Some (Piece.from_tuple (Rook, (6,1), White)));

  get_piece_test "2 major back rank checkmate - black king at 7,7" 7 7 
    checkmate_2major (Some (Piece.from_tuple (King, (7,7), Black)));
]

let checkmate_2pawn= file_to_board "checkmate_twopawn"
let checkmate_2pawn_black = file_to_board "checkmate_twopawn_black"
let checkmate_backrank = file_to_board "checkmate_backrank"
let checkmate_diagonal = file_to_board "checkmate_diagonal"
let checkmate_smothered = file_to_board "checkmate_smothered"
let checkmate_anastasia = file_to_board "checkmate_anastasia"
let checkmate_kingcap = file_to_board "checkmate_king_cap"
let checkmate_kingcap_black = file_to_board "checkmate_king_cap_black"
let checkmate_fianchetto = file_to_board "checkmate_fianchetto"
let checkmate_2bishop = file_to_board "checkmate_2bishop"
let checkmate_2bishop_black = file_to_board "checkmate_2bishop_black"
let checkmate_anastasia_black = file_to_board "checkmate_anastasia_black"

(** [print_win win] is the printer helper function for Winner piece option. *)
let print_win win =
  match win with
  | Draw -> "Draw"
  | Winner (Some White) ->  "White"
  | Winner (Some Black) -> "Black"
  | Winner (None) -> ""

(** [checkmate_tests] tests whether the State.update_winner function is working
    properly. *)
let checkmate_test
    (name : string)
    (board : board) 
    (expected_output : State.win) =
  name >:: (fun _ -> 
      assert_equal expected_output (update_winner board) ~printer: print_win)

let checkmate_tests = [
  checkmate_test "2 major back rank checkmate, white wins" 
    checkmate_2major (Winner (Some White));
  checkmate_test "2 pawn checkmate, white wins"
    checkmate_2pawn (Winner (Some White));
  checkmate_test "back rank checkmate, white wins"
    checkmate_backrank (Winner (Some White));
  checkmate_test "diagonal checkmate, white wins"
    checkmate_diagonal (Winner (Some White));
  checkmate_test "smothered checkmate, white wins" 
    checkmate_smothered (Winner (Some White));
  checkmate_test "anastasia checkmate, white wins"
    checkmate_anastasia (Winner (Some White));
  checkmate_test "king_cap checkmate, white wins"
    checkmate_kingcap (Winner (Some White));
  checkmate_test "fianchetto checkmate, white wins"
    checkmate_fianchetto (Winner (Some White));
  checkmate_test "two bishop checkmate, white wins"
    checkmate_2bishop (Winner (Some White));

  checkmate_test "2 pawn checkmate, black wins"
    checkmate_2pawn_black (Winner (Some Black));
  checkmate_test "king_cap checkmate, black wins"
    checkmate_kingcap_black (Winner (Some Black));
  checkmate_test "2 bishop checkmate, black wins"
    checkmate_2bishop_black (Winner (Some Black));
  checkmate_test "anastasia checkmate, black wins"
    checkmate_anastasia_black (Winner (Some Black));

]

(*[get_promotion_test] returns a Piece.t option based on if the move made by
  [piece] to [dst] on the [board] by [player] was an illegal move, or 
  if a pawn would be promoted by that move. *)
let get_promotion_test piece dst board player = 
  let s = set_board board (State.init_state()) in 
  match (State.move_piece piece dst s player) with 
  | Illegal x -> None
  | Legal new_state -> State.get_promotion (new_state)

(** [str_piece_opt p] outputs the string representation of Piece.t option p *)
let str_piece_opt p =
  match p with 
  | None -> "None"
  | Some p -> piece_printer p 

let promotion_pawn = file_to_board "promotion_pawn"
let promotion_notpawn = file_to_board "promotion_notpawn"

(** [promotion_test] tests whether the pawn promotions functions are working
    accordingly.*)
let promotion_test
    (name : string)
    (piece : Piece.t)
    (dst : string list)
    (board : board) 
    (player : Piece.color)
    (expected_output : Piece.t option) =
  name >:: (fun _ -> 
      assert_equal expected_output 
        (get_promotion_test piece dst board player) ~printer: str_piece_opt)

let promotion_tests = [
  promotion_test "move white pawn to (7,7), promote"
    (Piece.from_tuple (Pawn, (6,7), White)) ["77"] promotion_pawn White 
    (Some (Piece.from_tuple (Pawn, (7,7), White))); 
  promotion_test "move white pawn to (7,0), promote"
    (Piece.from_tuple (Pawn, (6,0), White)) ["70"] promotion_pawn White 
    (Some (Piece.from_tuple (Pawn, (7,0), White))); 
  promotion_test "move black pawn to (0,0), promote"
    (Piece.from_tuple (Pawn, (1,0), Black)) ["00"] promotion_pawn Black
    (Some (Piece.from_tuple (Pawn, (0,0), Black)));
  promotion_test "move black pawn to (0,7), promote"
    (Piece.from_tuple (Pawn, (1,7), Black)) ["07"] promotion_pawn Black
    (Some (Piece.from_tuple (Pawn, (0,7), Black))); 
  promotion_test "move white pawn to (2,1), no promote"
    (Piece.from_tuple (Pawn, (1,1), White)) ["21"] promotion_pawn 
    White None; 

  promotion_test "move white knight to (7,0), no promote"
    (Piece.from_tuple (Knight, (5,1), White)) ["70"] promotion_notpawn 
    White None; 
  promotion_test "move black rook to (7,7), no promote"
    (Piece.from_tuple (Rook, (6,7), Black)) ["77"] promotion_notpawn 
    Black None; 
  promotion_test "move black queen to (0,0), no promote"
    (Piece.from_tuple (Queen, (1,0), Black)) ["00"] promotion_notpawn 
    Black None; 
  promotion_test "move black rook to (0,7), no promote"
    (Piece.from_tuple (Rook, (1,7), Black)) ["07"] promotion_notpawn 
    Black None; 
]

(** [get_enp_test piece dst board player] returns a list of captured 
    Piece.t pieces made by [player] moving [piece] to [dst] on the [board].
    Include the a Black King at (0,0) to differentiate it from an illegal 
    move. *)
let get_enp_test piece1 dst1 piece2 dst2 board player1 player2 = 
  let s = set_board board (State.init_state()) in 
  match (State.move_piece piece1 dst1 s player1) with 
  | Illegal x -> []
  | Legal new_state -> 
    begin
      match (State.move_piece piece2 dst2 new_state player2) with 
      | Illegal x -> []
      | Legal state'' -> 
        (Piece.from_tuple (King, (0,0), Black)) :: State.get_captured (state'')
    end 


(**  [str_piece_lst p] returns the string representation of a list of 
     Piece.t pieces.*)
let str_piece_lst p =
  let rec helper_piece_lst p acc = 
    match p with 
    | [] -> acc
    | h :: t -> helper_piece_lst t ((acc ^ "--" ^ piece_printer h))
  in helper_piece_lst p ""

let en_passant = file_to_board "en_passant"

(** [enp_test] tests whether the en passant capturing functions are working
    accordingly.*)
let enp_test
    (name : string)
    (piece1 : Piece.t)
    (dst1 : string list)
    (piece2 : Piece.t)
    (dst2 : string list)
    (board : board) 
    (player1 : Piece.color)
    (player2 : Piece.color)
    (expected_output : Piece.t list) =
  name >:: (fun _ -> 
      assert_equal expected_output 
        (get_enp_test piece1 dst1 piece2 dst2 board player1 player2) 
        ~printer: str_piece_lst)

let enp_tests = [
  enp_test "enp white pawn capture black pawn" 
    (Piece.from_tuple (Pawn, (6,0), Black)) ["40"] 
    (Piece.from_tuple (Pawn, (4,1), White)) ["50"]
    en_passant Black White 
    [(Piece.from_tuple (King, (0,0), Black));
     (Piece.from_tuple (Pawn, (4,0), Black))];
  enp_test "enp white pawn can't capture black pawn, left illegal diagonal" 
    (Piece.from_tuple (Pawn, (5,2), Black)) ["42"] 
    (Piece.from_tuple (Pawn, (4,3), White)) ["52"]
    en_passant Black White [];
  enp_test "enp white pawn can't capture black pawn, right illegal diagonal" 
    (Piece.from_tuple (Pawn, (5,2), Black)) ["42"] 
    (Piece.from_tuple (Pawn, (4,1), White)) ["52"]
    en_passant Black White [];
  enp_test "enp white pawn can't capture black pawn, no capture" 
    (Piece.from_tuple (Pawn, (5,2), Black)) ["42"] 
    (Piece.from_tuple (Pawn, (4,3), White)) ["53"]
    en_passant Black White [(Piece.from_tuple (King, (0,0), Black))];
  enp_test "enp black pawn capture white pawn"
    (Piece.from_tuple (Pawn, (1,7), White)) ["37"] 
    (Piece.from_tuple (Pawn, (3,6), Black)) ["27"]
    en_passant White Black
    [(Piece.from_tuple (King, (0,0), Black));
     (Piece.from_tuple (Pawn, (3,7), White))];
  enp_test "enp black pawn can't capture white pawn, left illegal diagonal"
    (Piece.from_tuple (Pawn, (2,5), White)) ["35"] 
    (Piece.from_tuple (Pawn, (3,4), Black)) ["25"]
    en_passant White Black [];
  enp_test "enp black pawn can't capture white pawn, right illegal diagonal"
    (Piece.from_tuple (Pawn, (2,5), White)) ["35"] 
    (Piece.from_tuple (Pawn, (3,6), Black)) ["25"]
    en_passant White Black [];
  enp_test "enp black pawn can't capture white pawn, no capture"
    (Piece.from_tuple (Pawn, (2,5), White)) ["35"] 
    (Piece.from_tuple (Pawn, (3,6), Black)) ["26"]
    en_passant White Black [(Piece.from_tuple (King, (0,0), Black))];
]

let castling = file_to_board "castling"
let not_castling = file_to_board "not_castling"
let castling_incheck = file_to_board "castling_incheck"

(** [get_castling_test piece dst board player row col] returns the Piece.t 
    option of location (row, col) on the board once [player] has moved
    the [piece] to its [dst] on the [board] *)
let get_castling_test piece dst board player row col= 
  let s = set_board board (State.init_state()) in 
  match (State.move_piece piece dst s player) with 
  | Illegal x -> None
  | Legal new_state -> 
    let b = State.get_board new_state in get_piece row col b

(** [castling_test] tests whether the castling functions are working
    accordingly.*)
let castling_test
    (name : string)
    (piece : Piece.t)
    (dst : string list)
    (board : board) 
    (player : Piece.color)
    (row : int)
    (col : int)
    (expected_output : Piece.t option) =
  name >:: (fun _ -> 
      assert_equal expected_output 
        (get_castling_test piece dst board player row col) 
        ~printer: str_piece_opt)

let castling_tests = [
  castling_test "white castling to the left, rook at 0 3" 
    (Piece.from_tuple (King, (0,4), White)) ["02"]
    castling White 0 3
    (Some (Piece.from_tuple (Rook, (0,3), White)));
  castling_test "white castling to the left, no rook at 0 0" 
    (Piece.from_tuple (King, (0,4), White)) ["02"]
    castling White 0 0 None;
  castling_test "white move 1 to the left, rook at 0 0" 
    (Piece.from_tuple (King, (0,4), White)) ["03"]
    castling White 0 0
    (Some (Piece.from_tuple (Rook, (0,0), White)));
  castling_test "white move 1 to the left, king at 03" 
    (Piece.from_tuple (King, (0,4), White)) ["03"]
    castling White 0 3
    (Some (Piece.from_tuple (King, (0,3), White)));
  castling_test "white castling to the right, rook at 0 5" 
    (Piece.from_tuple (King, (0,4), White)) ["06"]
    castling White 0 5
    (Some (Piece.from_tuple (Rook, (0,5), White)));
  castling_test "white castling to the right, no rook at 0 7" 
    (Piece.from_tuple (King, (0,4), White)) ["06"]
    castling White 0 7 None;
  castling_test "white move 1 to the right, king at 0 5" 
    (Piece.from_tuple (King, (0,4), White)) ["05"]
    castling White 0 5
    (Some (Piece.from_tuple (King, (0,5), White)));

  castling_test "black castling to the left, rook at 7 3" 
    (Piece.from_tuple (King, (7,4), Black)) ["72"]
    castling Black 7 3
    (Some (Piece.from_tuple (Rook, (7,3), Black)));
  castling_test "black castling to the left, no rook at 7 0" 
    (Piece.from_tuple (King, (7,4), Black)) ["72"]
    castling Black 7 0 None;
  castling_test "black move 1 to the left, rook at 7 0" 
    (Piece.from_tuple (King, (7,4), Black)) ["73"]
    castling Black 7 0
    (Some (Piece.from_tuple (Rook, (7,0), Black)));
  castling_test "black move 1 to the left, king at 7 3" 
    (Piece.from_tuple (King, (7,4), Black)) ["73"]
    castling Black 7 3
    (Some (Piece.from_tuple (King, (7,3), Black)));
  castling_test "black castling to the right, rook at 7 5" 
    (Piece.from_tuple (King, (7,4), Black)) ["76"]
    castling Black 7 5
    (Some (Piece.from_tuple (Rook, (7,5), Black)));
  castling_test "black castling to the right, no rook at 7 7" 
    (Piece.from_tuple (King, (7,4), Black)) ["76"]
    castling Black 7 7 None;
  castling_test "black move 1 to the right, king at 7 5" 
    (Piece.from_tuple (King, (7,4), Black)) ["75"]
    castling Black 7 5
    (Some (Piece.from_tuple (King, (7,5), Black)));

  castling_test "white can't castle left, king has moved" 
    (Piece.from_tuple (King, (0,3), White)) ["01"]
    not_castling White 0 0 None;
  castling_test "white can't castle right, king + rook have moved" 
    (Piece.from_tuple (King, (0,3), White)) ["05"]
    not_castling White 0 7 None;

  castling_test "black can't castle left, knight in between"
    (Piece.from_tuple (King, (7,4), Black)) ["72"]
    not_castling Black 7 3 None;
  castling_test "black can't castle right, king in check"
    (Piece.from_tuple (King, (7,4), Black)) ["76"]
    not_castling Black 7 6 None;

  castling_test "white can't castle, will put in check"
    (Piece.from_tuple (King, (0,4), White)) ["02"]
    castling_incheck White 0 3 None;
]

let suite = 
  "test suit for Chess" >::: List.flatten[
    board_tests;
    piece_tests;
    command_tests;
    movement_tests;
    txt_import_tests;
    checkmate_tests;
    promotion_tests;
    enp_tests;
    castling_tests;
  ]

let _ = run_test_tt_main suite