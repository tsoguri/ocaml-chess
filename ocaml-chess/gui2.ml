open Graphics
open Piece
open State
open Gui
open Board

(** [draw_text_block x y s t] displays a block of text [text] that has
    ([x], [y]) as the upper left coordinate, with each line spaced out by 
    [space] *)
let rec draw_text_block x y space text = 
  match text with 
  | [] -> () 
  | h :: t -> 
    moveto x y; draw_string h; draw_text_block x (y - space) space t

(** [welcome] is the set of instructions on the home page*)
let welcome = 
  ["   This is a two-player, interactive game."; 
   "  Grab a friend and come join in on the fun!";]

(** [rules] is the set of rules on the instructions page*)
let rules = 
  [
    "The objective of this game is to checkmate the opponent's King. This is";
    "done by moving the pieces of your color across the board, capturing";
    "your opponent's other pieces. These pieces all have restricted movements";
    "which are outlined in resources such as Chess.com.";
    "";
    "While playing this game, all standard chess rules apply. The player is "; 
    "able to move their piece by selecting their desired piece on the board,";
    "and then selecting a destination on the board to place that piece."; 
    "The player will be prompted each time to choose a piece and a location.";
    "The player must abide by all correct piece selections and movements. If ";
    "the player chooses an invalid piece or location, the player will be ";
    "notified by a short error message displayed on top of the board.";
    "";
    "We have implemented the special chess rules of Promotion, Castling, and ";
    "En Passant. Castling and En Passant will happen automatically, but ";
    "once a pawn is promoted, the game will prompt you to choose either a ";
    "Bishop, Knight, Rook or Queen to take its place. ";
    "";
    "At any point in the game you can either restart or quit the game. If it ";
    "is your turn, you can also concede, or request a draw to your opponent.";
    "If you concede, your opponent will automatically win. However, if you ";
    "request a draw, your opponent can either accept or deny your draw. If ";
    "they choose to accept, the game will end in a draw. If not, the game will";
    "continue. You can only request a draw once in your turn.";
    "";
    "This game can either end in a restart, quit, draw, concession, or if you";
    "have successfully checkmated your opponent! Once this occurs, the game";
    "will automatically end, declaring a winner. At this point, you will have";
    "the option to play again.";
    "";
    "We hope you have fun! Happy playing!";
    "";
    "** Note ** Our game of chess will not time you on your moves";
  ]

(** Design Constants *) 
(** These are all constants used to define coordinates in the GUI window,
    colors, or spacers.*) 
(*----------------------------------------------------------------------------*)
let title_x = 225 
let title_y = 800

let welcome_x = 100
let welcome_y = 750

let board_topx = 100
let board_topy = 700
let board_cellw = 50

let background = rgb 220 220 220

let light_square = rgb 237 184 121
let dark_square = rgb 185 116 85
let chess_board_color = rgb 139	71 38

let text_spacer = 25
let small_button_w = 50
let small_button_h = 20
let med_button_w = 90
let med_button_h = 20
let icon_w = 20
let icon_h = 20

let color_button_x = 200
let color_button_y = 700

let rules_button_x = 260
let rules_button_y = 320

let play_button_x = 280
let play_button_y = 360

let quit_button_x = (- 45)
let quit_button_y = (- 45)
let quit_color = rgb 203 67 53

let captured_x = board_topx - 20
let captured_y = board_topy - 8*board_cellw - 110

let back_button_x = 25
let back_button_y = (- 45)
let back_color = rgb 139 134 130

let yes_button_x = 100
let yes_button_y = 725
let yes_color = rgb 171 235 198  

let no_button_x = yes_button_x + 100 
let no_button_y = yes_button_y
let no_color = rgb 236 112 99

let draw_button_x = 150
let draw_button_y = captured_y - 50

let concession_button_x = draw_button_x + 200 
let concession_button_y = draw_button_y

let dc_color = rgb 254 245 231

let piece_spacer = 75

let bishop_button_x = 150
let bishop_button_y = board_topy + text_spacer + 7

let knight_button_x = bishop_button_x + piece_spacer
let knight_button_y = bishop_button_y

let rook_button_x = bishop_button_x + 2*piece_spacer
let rook_button_y = bishop_button_y 

let queen_button_x = bishop_button_x + 3*piece_spacer
let queen_button_y = bishop_button_y 

let restart_button_x = 250
let restart_button_y = 550


(** Draw Elements *) 
(** These are all vector representations of an element in the chess game, 
    with the bottom left coordinates being (x,y) and with the optional paramter 
    [scale] indicating their scale/size. *) 
(*----------------------------------------------------------------------------*)
(** [draw_quit x y] draws the quit button with (x,y) as the bottom left
    coordinates. *)
let draw_quit x y = 
  set_color white;
  Array.of_list [((x + 3*icon_w/10), (y + (2*icon_w/10))); 
                 ((x + 2*icon_w/10), (y + (3*icon_w/10)));
                 ((x + 4*icon_w/10), (y + (5*icon_w/10)));
                 ((x + 2*icon_w/10), (y + (7*icon_w/10)));
                 ((x + 3*icon_w/10), (y + (8*icon_w/10)));
                 ((x + 5*icon_w/10), (y + (6*icon_w/10)));
                 ((x + 7*icon_w/10), (y + (8*icon_w/10)));
                 ((x + 8*icon_w/10), (y + (7*icon_w/10)));
                 ((x + 6*icon_w/10), (y + (5*icon_w/10)));
                 ((x + 8*icon_w/10), (y + (3*icon_w/10)));
                 ((x + 7*icon_w/10), (y + (2*icon_w/10)));
                 ((x + 5*icon_w/10), (y + (4*icon_w/10)));
                 ((x + 3*icon_w/10), (y + (2*icon_w/10)));] |> fill_poly

(** [draw_back x y] draws the back button with (x,y) as the bottom left
    coordinates. *)
let draw_back x y = 
  set_color white;
  Array.of_list [((x + 2*icon_w/10), (y + (5*icon_w/10))); 
                 ((x + 7*icon_w/10), (y + (8*icon_w/10))); 
                 ((x + 8*icon_w/10), (y + (7*icon_w/10))); 
                 ((x + 4*icon_w/10), (y + (5*icon_w/10))); 
                 ((x + 8*icon_w/10), (y + (3*icon_w/10))); 
                 ((x + 7*icon_w/10), (y + (2*icon_w/10))); 
                 ((x + 2*icon_w/10), (y + (5*icon_w/10))); ] |> fill_poly

(** [draw_restart x y] draws the restart button with (x,y) as the bottom left
    coordinates. *)
let draw_restart x y = 
  set_color white;
  draw_arc (x + icon_w/2) (y + icon_w/2) (3*icon_w/10) (3*icon_w/10) 0 275;
  Array.of_list [((x + 5*icon_w/10), (y + (5*icon_w/10))); 
                 ((x + 19*icon_w/20), (y + (5*icon_w/10))); 
                 ((x + 7*icon_w/10), (y + (2*icon_w/10))); 
                 ((x + 5*icon_w/10), (y + (5*icon_w/10))); ] |> fill_poly

(** [draw_crown x y scale] draws a crown with (x,y) as the bottom left 
    coordinates and [scale] as the size *)
let draw_crown x y scale =
  Array.of_list [((x + 6*scale/20),(y + 5*scale/20));
                 ((x + 4*scale/20),(y + 10*scale/20));
                 ((x + 6*scale/20),(y + 8*scale/20));
                 ((x + 7*scale/20),(y + 12*scale/20));
                 ((x + 8*scale/20),(y + 9*scale/20));
                 ((x + 10*scale/20),(y + 14*scale/20));
                 ((x + 12*scale/20),(y + 9*scale/20));
                 ((x + 13*scale/20),(y + 12*scale/20));
                 ((x + 14*scale/20),(y + 8*scale/20));
                 ((x + 16*scale/20),(y + 10*scale/20));
                 ((x + 14*scale/20),(y + 5*scale/20));
                 ((x + 6*scale/20),(y + 5*scale/20)); ] |> fill_poly;
  fill_circle (x + 4*scale/20) (y + 10*scale/20) (scale/60); 
  fill_circle (x + 7*scale/20) (y + 12*scale/20) (scale/60); 
  fill_circle (x + 10*scale/20) (y + 14*scale/20) (scale/30); 
  fill_circle (x + 13*scale/20) (y + 12*scale/20) (scale/60); 
  fill_circle (x + 16*scale/20) (y + 10*scale/20) (scale/60)

(** [draw_castle x y scale] draws the castle on the home page with (x,y) as the
    bottom left coordinates and [scale] as the size *)
let draw_castle x y scale = 
  Array.of_list [((x + 4*scale/20),(y + 3*scale/20));
                 ((x + 4*scale/20),(y + 9*scale/20));
                 ((x + 7*scale/40),(y + 10*scale/20));
                 ((x + 15*scale/40),(y + 10*scale/20));
                 ((x + 7*scale/20),(y + 9*scale/20));
                 ((x + 7*scale/20),(y + 8*scale/20));
                 ((x + 8*scale/20),(y + 8*scale/20));
                 ((x + 8*scale/20),(y + 12*scale/20));
                 ((x + 10*scale/20),(y + 15*scale/20));
                 ((x + 12*scale/20),(y + 12*scale/20));
                 ((x + 12*scale/20),(y + 8*scale/20));
                 ((x + 13*scale/20),(y + 8*scale/20));
                 ((x + 13*scale/20),(y + 9*scale/20));
                 ((x + 25*scale/40),(y + 10*scale/20));
                 ((x + 33*scale/40),(y + 10*scale/20));
                 ((x + 16*scale/20),(y + 9*scale/20));
                 ((x + 16*scale/20),(y + 3*scale/20));
                 ((x + 11*scale/20),(y + 3*scale/20));
                 ((x + 11*scale/20),(y + 6*scale/20));
                 ((x + 9*scale/20),(y + 6*scale/20));
                 ((x + 9*scale/20),(y + 3*scale/20));
                 ((x + 4*scale/20),(y + 3*scale/20)); ] |> fill_poly;
  moveto (x + 11*scale/40) (y + 10*scale/20);
  lineto (x + 11*scale/40) (y + 11*scale/20);
  moveto (x + 29*scale/40) (y + 10*scale/20);
  lineto (x + 29*scale/40) (y + 11*scale/20);
  Array.of_list [((x + 11*scale/40),(y + 11*scale/20));
                 ((x + 11*scale/40),(y + 12*scale/20));
                 ((x + 15*scale/40),(y + 23*scale/40));
                 ((x + 11*scale/40),(y + 11*scale/20))] |> fill_poly;
  Array.of_list [((x + 29*scale/40),(y + 11*scale/20));
                 ((x + 29*scale/40),(y + 12*scale/20));
                 ((x + 33*scale/40),(y + 23*scale/40));
                 ((x + 29*scale/40),(y + 11*scale/20))] |> fill_poly

(** [draw_pawn x y scale] draws the pawn chess piece with (x,y) as the
    bottom left coordinates and [scale] as the size *)
let draw_pawn x y scale =
  fill_circle 
    (x + (6*scale/12)) (y + (8*scale/12) - (scale/48)) (2*scale/12 - scale/48);
  fill_rect 
    (x + (4*scale/12)) (y + (5*scale/12) + (scale/24)) (4*scale/12)(scale/24); 
  fill_rect 
    (x + (5*scale/12)) (y + (3*scale/12)) (2*scale/12) (4*scale/24); 
  fill_rect 
    (x + (3*scale/12)) (y + (2*scale/12)) (6*scale/12) (scale/24)

(** [draw_rook x y scale] draws the rook chess piece with (x,y) as the
    bottom left coordinates and [scale] as the size *)
let draw_rook x y scale =
  fill_rect 
    (x + (4*scale/12)) (y + (8*scale/12)) (4*scale/12) (scale/12); 
  fill_rect 
    (x + (4*scale/12))(y + (9*scale/12)) (scale/12) (scale/12); 
  fill_rect 
    (x + (5*scale/12)+scale/24) (y + (9*scale/12)) (scale/12) (scale/12); 
  fill_rect 
    (x + (6*scale/12)+scale/24 + scale/48) (y + (9*scale/12)) 
    (scale/12) (scale/12); 
  fill_rect 
    (x + (4*scale/12)) (y + (3*scale/12)) (4*scale/12) (4*scale/12 + scale/24); 
  fill_rect 
    (x + (3*scale/12)) (y + (2*scale/12)) (6*scale/12) (scale/24)

(** [draw_bishop x y scale] draws the bishop chess piece with (x,y) as the
    bottom left coordinates and [scale] as the size *)
let draw_bishop x y scale = 
  fill_circle 
    (x + (6*scale/12)) (y + (10*scale/12)) (scale/24 + scale/48);
  fill_ellipse 
    (x + (6*scale/12)) (y + (7*scale/12)) (2*scale/12) (2*scale/12 + scale/48);
  fill_rect 
    (x + (5*scale/12)) (y + (3*scale/12)) (2*scale/12 + scale/24) (3*scale/24); 
  fill_rect 
    (x + (3*scale/12)) (y + (2*scale/12)) (6*scale/12) (scale/24)

(** [draw_queen x y scale] draws the queen chess piece with (x,y) as the
    bottom left coordinates and [scale] as the size *)
let draw_queen x y scale = 
  fill_circle 
    (x + (6*scale/12)) (y + (9*scale/12)) (scale/12 + scale/48);
  fill_rect 
    (x + 4*scale/12) (y + (3*scale/12))(4 * scale/12) (3*scale/12 - scale/48);
  fill_rect 
    (x + (3*scale/12)) (y + (2*scale/12)) (6*scale/12) (scale/24);
  Array.of_list [((x + 3*scale/12), (y + (8*scale/12))); 
                 ((x + 4*scale/12), (y + (6*scale/12)));
                 ((x + 8*scale/12), (y + (6*scale/12)));
                 ((x + 9*scale/12), (y + (8*scale/12)));] |> fill_poly

(** [draw_king x y scale] draws the king chess piece with (x,y) as the
    bottom left coordinates and [scale] as the size *)
let draw_king x y scale = 
  fill_rect 
    (x + 4*scale/12+ scale/24) (y + (10*scale/12)) (3*scale/12) (scale/24); 
  fill_rect 
    (x + 11*scale/24 + scale/48) (y + (8*scale/12)) (scale/24) (3*scale/12); 
  fill_rect 
    (x + 4*scale/12) (y + (3*scale/12)) (4 * scale/12) (3*scale/12 - scale/48);
  fill_rect 
    (x + (3*scale/12)) (y + (2*scale/12)) (6*scale/12) (scale/24);
  Array.of_list [((x + 3*scale/12), (y + (9*scale/12))); 
                 ((x + 4*scale/12), (y + (6*scale/12)));
                 ((x + 8*scale/12), (y + (6*scale/12)));
                 ((x + 9*scale/12), (y + (9*scale/12)));]
  |> fill_poly

(** [draw_knight x y scale] draws the knight chess piece with (x,y) as the
    bottom left coordinates and [scale] as the size *)
let draw_knight x y scale = 
  fill_rect 
    (x + (3*scale/12)) (y + (2*scale/12)) (6*scale/12) (scale/24);
  fill_rect 
    (x + 4*scale/12) (y + (3*scale/12)) (4 * scale/12) (2*scale/12);
  Array.of_list [((x + 2*scale/12), (y + (6*scale/12))); 
                 ((x + 5*scale/12), (y + (9*scale/12)));
                 ((x + 8*scale/12), (y + (9*scale/12)));
                 ((x + 9*scale/12), (y + (7*scale/12)));
                 ((x + 8*scale/12), (y + (5*scale/12)));
                 ((x + 2*scale/12), (y + (5*scale/12)));
                 ((x + 2*scale/12), (y + (6*scale/12)));]
  |> fill_poly; 
  Array.of_list [((x + 6*scale/12), (y + (9*scale/12))); 
                 ((x + 7*scale/12), (y + (11*scale/12)));
                 ((x + 8*scale/12), (y + (9*scale/12)));
                 ((x + 6*scale/12), (y + (9*scale/12)));]
  |> fill_poly

(** Draw Large Elements *) 
(*----------------------------------------------------------------------------*)

(** [draw_pieces x y r c s] draws the piece according to [rank], in their 
    respective [color], at a size [scale] with (x,y) as the bottom left 
    coordinate*)
let draw_pieces x y rank color scale =
  (match color with 
   | White -> set_color white
   | Black -> set_color black);
  match rank with  
  | Pawn -> draw_pawn x y scale
  | Knight -> draw_knight x y scale
  | Bishop -> draw_bishop x y scale
  | Rook -> draw_rook x y scale
  | Queen -> draw_queen x y scale
  | King -> draw_king x y scale

(** [draw_piece x y row col st sc] draws the piece at (row,col) of the board of
    state [s]. It draws it with (x,y) as the bottom left coordinate at size 
    [scale].*)
let draw_piece x y row col state scale = 
  match Board.get_piece row col (get_board state) with 
  | None -> draw_string "" 
  | Some piece -> draw_pieces x y (get_rank piece) (get_color piece) scale

(** [params x y color row col func] returns the next set of parameters 
    needed for the fill board helper, according to whether the board is 
    displayed with white at the bottom or black. It returns new x, y, color,
     row, col parameters, and moves the current position of the Graphics 
     pointer to a cooordinate, depending on whether a new row should be 
     returned, and who the current player is.*)
let params x y color row col func = 
  match col, (func 0) with 
  | 8, 0 | (-1), 7 -> 
    begin
      moveto board_topx (y-board_cellw); 
      match color with 
      | White -> (board_topx, (y - board_cellw), Black, (row - 1), func 0)
      | Black -> (board_topx, (y - board_cellw), White, (row - 1), func 0)
    end 
  | a, b -> moveto x y; (x, y, color, row, a) 

(** [fill_board x y color row col state func acc] fills the board depending
    on who the current player is, with (x,y) as the top left coordinate of the
    board.*)
let rec fill_board x y color row col state func acc =
  match params x y color row col func with 
  | ( _, _, _, (-1), _) -> ()
  | (x', y', Black, row', col') -> 
    set_color dark_square;
    fill_rect x' y' board_cellw board_cellw;
    draw_piece x' y' (func row') col' state board_cellw;
    fill_board (x' + board_cellw) y' White row' (acc col' 1) state func acc
  | (x', y', White, row', col') -> 
    set_color light_square;
    fill_rect x' y' board_cellw board_cellw;
    draw_piece x' y' (func row') col' state board_cellw;
    fill_board (x' + board_cellw) y' Black row' (acc col' 1) state func acc

(** [draw_board_hor x y acc] draws the horizontal lines on the board with
    (x,y) as the top left coordinate of the board.*)
let rec draw_board_hor x y acc =
  match acc with 
  | 9 -> ()
  | a -> 
    moveto (x - x + board_topx) (y - board_cellw * a); 
    lineto x (y - board_cellw * a);
    draw_board_hor x y (a + 1)

(** [draw_board_vert x y acc] draws the vertical lines on the board with
    (x,y) as the top left coordinate of the board.*)
let rec draw_board_vert x y acc bty =
  match acc with 
  | 9 -> ()
  | a -> 
    moveto (x + board_cellw * a) bty; 
    lineto (x + board_cellw * a) (y - board_cellw * 8);
    draw_board_vert x y (a + 1) bty

let rec draw_numcol y color acc = 
  match acc with 
  | 0 -> ()
  | x ->
    match color with 
    | White ->
      moveto (board_topx - 8) y;
      draw_string (string_of_int acc);
      draw_numcol (y - board_cellw) color (acc - 1)
    | Black -> 
      moveto (board_topx - 8) y;
      draw_string (string_of_int (9 - acc));
      draw_numcol (y - board_cellw) color (acc - 1)

(** [draw_board x y turn state] draws the board of state [state] with the 
    current player as [turn] with (x,y) as the top left coordinate of the 
    board*)
let draw_board x y turn state = 
  moveto (x - 20) (y + 20); 
  set_color chess_board_color;
  fill_rect (x - 20)  (y - 20 - (board_cellw * 8)) (board_cellw * 8 + 40)
    (board_cellw * 8 + 40);
  set_color black;
  (match turn with 
   | White -> 
     fill_board x (y - board_cellw) turn 7 0 state (fun x -> x) ( + );
     moveto x (y - 15 - (board_cellw * 8));
     set_color light_square;
     draw_string 
       "    a       b        c       d       e        f       g       h ";
     moveto (x - 8) (y - board_cellw/2 - 5);
     draw_numcol (y - board_cellw/2 - 5) White 8;
   | Black -> 
     fill_board x (y - board_cellw) turn 7 7 state (fun x -> 7 - x) ( - );
     moveto x (y - 15 - (board_cellw * 8));
     set_color light_square;
     draw_string 
       "    h       g        f       e       d        c       b       a ";
     moveto (x - 8) (y - board_cellw/2 - 5);
     draw_numcol (y - board_cellw/2 - 5) Black 8;
  );
  moveto x y;
  set_color chess_board_color;
  lineto (x + board_cellw * 8) y;
  draw_board_hor (x + board_cellw * 8) y 0;
  moveto x y;
  lineto x (y - board_cellw * 8);
  draw_board_vert x y 0 y

(** [fill_captured_helper p xw xb y] draws the captured pieces from 
    the list of captured pieces [p] with [xw] as the x coordinate of the white 
    captured pieces, [xb] as the x coordinate of the black captured pieces, 
    with [y] as the bottom left y coordinate of the captured piece block.*)
let rec fill_captured_helper p xw xb y = 
  match p with 
  | [] -> ()
  | h :: t -> 
    match get_color h with 
    | Black -> draw_pieces xb
                 (y + text_spacer + 8) (get_rank h) Black 30;
      fill_captured_helper t xw (xb - 30) y
    | White -> draw_pieces xw (y + 5) (get_rank h) White 30;
      fill_captured_helper t (xw + 30) xb y

(** [fill_captured x y st] draws the captured pieces from state [st] with 
    (x,y) as the lower left coordinate*)
let fill_captured x y state = 
  let piece_list = get_captured state in 
  fill_captured_helper piece_list (x + 10) (x + board_cellw * 8 + 10) y

(** [draw_captured x y st] draws the captured pieces from state [st] with 
    (x,y) as the lower left coordinates, with a color background *)
let draw_captured x y state = 
  moveto x y; 
  set_color chess_board_color;
  fill_rect x y (board_cellw * 8 + 40) 70;
  fill_captured x y state

(** [make_button x y width height text color] makes a rectangle filled with 
    [color] with (x,y) as the bottom left coordinates, with a string [text] 
    drawn in the middle of the rectangle.*)
let make_button x y width height text color = 
  moveto (x + 10) (y + 5);
  set_color color;
  fill_rect x y width height;
  set_color black;
  draw_string text

(** [draw_text_button x y string color] makes with a width and height of 
    small_button_w and small_button_h with (x,y) as the bottom left coordinates,
    colored in with [color] and with text[string]*)
let draw_text_button x y string color = 
  make_button x y small_button_w small_button_h string color

(** [draw_icon_button x y string color] makes with a width and height of 
    icon_w and icon_h with (x,y) as the bottom left coordinates,
    colored in with [color] and with text[string]*)
let draw_icon_button x y string color = 
  make_button x y icon_w icon_h string color 

(** Click Events *) 
(*----------------------------------------------------------------------------*)
(** [click x y w h ev] returns whether the user clicked in the rectangle 
    location that has (x,y) as the bottom left coordinates with width as [w]
     and height as [h] during the event [ev] *)
let click x y width height ev = 
  ev.mouse_x > x && ev.mouse_x < (x+width) && 
  ev.mouse_y > y && ev.mouse_y < (y+height) 

(** [clicked str ev] returns whether the user clicked in the rectangle
    location designated by string [str]*)
let clicked str ev = 
  match str with 
  | "white" -> 
    click color_button_x color_button_y small_button_w small_button_h ev
  | "black" ->  click (color_button_x + 150) color_button_y
                  small_button_w small_button_h ev
  | "board" -> click (board_topx) (board_topy - (board_cellw*8))
                 (board_cellw*8) (board_cellw*8) ev
  | "rules" -> 
    click (rules_button_x) (rules_button_y) med_button_w med_button_h  ev
  | "play" -> 
    click (play_button_x) (play_button_y) small_button_w small_button_h ev
  | "quit" -> 
    click (size_x () + quit_button_x) (size_y () + quit_button_y) 
      icon_w icon_h ev
  | "back" ->
    click (back_button_x) (size_y () + back_button_y) icon_w icon_h ev
  | "yes" -> 
    click yes_button_x yes_button_y small_button_w small_button_h ev 
  | "no" -> 
    click no_button_x no_button_y small_button_w small_button_h ev
  | "draw" -> 
    click draw_button_x draw_button_y  med_button_w med_button_h ev
  | "concede" -> 
    click concession_button_x concession_button_y med_button_w med_button_h ev
  | "bishop" -> 
    click bishop_button_x bishop_button_y small_button_w small_button_h ev
  | "knight" ->
    click knight_button_x knight_button_y small_button_w small_button_h ev
  | "rook" ->
    click rook_button_x rook_button_y small_button_w small_button_h ev
  | "queen" -> 
    click queen_button_x queen_button_y small_button_w small_button_h ev
  | "restart" -> 
    click restart_button_x restart_button_y med_button_w med_button_h ev
  | _ -> false

(** Page Helper Functions *) 
(*----------------------------------------------------------------------------*)

(** [wfind_pos helper x' y' row col ev] returns the (row, col) of the board
    that the user has clicked on, when the current player is white. *)
let rec wfind_pos_helper x' y' row col ev = 
  match row, col with 
  | (-1), c -> (0,0)
  | r, 8 -> wfind_pos_helper board_topx (y' - board_cellw) (r - 1) 0 ev
  | r, c -> 
    begin
      match click x' y' board_cellw board_cellw ev with 
      | true ->  (r, c)
      | false -> wfind_pos_helper (x' + board_cellw) y' r (c + 1) ev
    end 

(** [bfind_pos helper x' y' row col ev] returns the (row, col) of the board
    that the user has clicked on, when the current player is black. *)
let rec bfind_pos_helper x' y' row col ev = 
  match row, col with 
  | 8, c -> (0,0)
  | r, (-1) -> bfind_pos_helper board_topx (y' - board_cellw) (r + 1) 7 ev
  | r, c -> 
    begin
      match click x' y' board_cellw board_cellw ev with 
      | true ->  (r, c)
      | false -> bfind_pos_helper (x' + board_cellw) y' r (c - 1) ev
    end 

(** [find_pos color ev] returns the (row,col) of the board that the user 
    has clicked on, when the current player is [color] *)
let find_pos color ev = 
  match color with 
  | White -> 
    wfind_pos_helper board_topx (board_topy - board_cellw) 7 0 ev
  | Black -> 
    bfind_pos_helper board_topx (board_topy - board_cellw) 0 7 ev

(** [switch_curr_player p] returns White if [p] is Black and Black if [p] is
    White*)
let switch_curr_player p = 
  match p with 
  | White -> Black
  | Black -> White

(** [player_str] returns the string representation of player [p] *)
let player_str p = 
  match p with 
  | White -> "White"
  | Black -> "Black"

(** [display_pos pos] returns the string representation of [pos] of the board, 
    by formatting it into the chess notation (column letter, row number) that
    goes from a to h and 1 to 8. *)
let display_pos pos = 
  match pos with 
  | a,b -> 
    begin
      match b with 
      | 0 -> draw_string ("@ (a, " ^ string_of_int (a + 1) ^ ")")
      | 1 -> draw_string ("@ (b, " ^ string_of_int (a + 1) ^ ")")
      | 2 -> draw_string ("@ (c, " ^ string_of_int (a + 1) ^ ")")
      | 3 -> draw_string ("@ (d, " ^ string_of_int (a + 1) ^ ")")
      | 4 -> draw_string ("@ (e, " ^ string_of_int (a + 1) ^ ")")
      | 5 -> draw_string ("@ (f, " ^ string_of_int (a + 1) ^ ")")
      | 6 -> draw_string ("@ (g, " ^ string_of_int (a + 1) ^ ")")
      | 7 -> draw_string ("@ (h, " ^ string_of_int (a + 1) ^ ")")
      | x -> draw_string "@ invalid position"
    end 

(** [display_choice x y pos curr_player curr_state] displays the chess piece 
    at location [pos] on the board of state [curr_state] that [curr_player]
    chose, along with its location. It is displayed at (x,y) coordiante on the
    GUI window *)
let display_choice x y pos curr_player curr_state = 
  match pos with 
  | a,b -> 
    moveto x (y+ 50);
    draw_string ((player_str curr_player) ^ " Chose : " );
    draw_piece (x+ 75) (y + 35 ) a b curr_state 40;
    moveto (x + 125) (y+ 50);
    set_color black;
    display_pos pos

(** Page Movements *) 
(*----------------------------------------------------------------------------*)
(** [start ()] displays the starting home page of the game *) 
let rec start () = 
  set_color background;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color black;
  set_window_title "Chess Game";
  draw_icon_button (
    size_x () + quit_button_x) (size_y () + quit_button_y) "" quit_color ;
  draw_quit (size_x () + quit_button_x) (size_y () + quit_button_y);
  moveto title_x title_y;
  set_color black;
  draw_string "Welcome to the Chess Game!";
  draw_text_block (welcome_x + 70) welcome_y text_spacer welcome;
  make_button rules_button_x rules_button_y med_button_w med_button_h 
    "Instructions" dc_color;
  draw_text_button play_button_x play_button_y "Play!" dc_color;
  set_color back_color;
  draw_castle 100 350 400;
  click_start ()

(** [standard_header ()] displays the standard header for the chess game,
    which includes the back and quit button. *) 
and standard_header () = 
  set_color background;
  fill_rect 0 0 (size_x ()) (size_y ());
  draw_icon_button 
    (size_x () + quit_button_x) (size_y () + quit_button_y) "" quit_color ;
  draw_quit (size_x () + quit_button_x) (size_y () + quit_button_y);
  draw_icon_button 
    back_button_x (size_y () + back_button_y) "" back_color;
  draw_back back_button_x (size_y () + back_button_y);
  moveto title_x title_y;
  set_color black;

  (** [quit_header ()] only displays the quit button at its appropriate 
      position *)
and quit_header () = 
  set_color background;
  fill_rect 0 0 (size_x ()) (size_y ());
  draw_icon_button 
    (size_x () + quit_button_x) (size_y () + quit_button_y) "" quit_color ;
  draw_quit (size_x () + quit_button_x) (size_y () + quit_button_y);
  moveto welcome_x title_y;
  set_color black;

  (** [game_header ()] displays the quit and restart buttons, as well as the 
      "Request Draw" and "Concede" buttons at the bottom of the window. *)
and game_header () = 
  set_color background;
  fill_rect 0 0 (size_x ()) (size_y ());
  draw_icon_button 
    (size_x () + quit_button_x) (size_y () + quit_button_y) "" quit_color ;
  draw_quit (size_x () + quit_button_x) (size_y () + quit_button_y);
  draw_icon_button 
    back_button_x (size_y () + back_button_y) "" back_color;
  draw_restart back_button_x (size_y () + back_button_y);
  make_button draw_button_x draw_button_y med_button_w med_button_h 
    "Request Draw" dc_color;
  make_button concession_button_x concession_button_y med_button_w med_button_h
    "  Concede" dc_color;
  moveto welcome_x title_y;
  set_color black

(** [choose_piece_header x y curr_player curr_state] displays the game_header
    along with the piece that [curr_player] chose.*)
and choose_piece_header x y curr_player curr_state = 
  game_header (); 
  display_choice welcome_x welcome_y (x,y) curr_player curr_state;
  moveto welcome_x welcome_y;

  (**  [helper_buttonrules] displays all of the game buttons in the 
       instructions page along with their instructions.*)
and helper_buttonrules () = 
  draw_string "Buttons:";
  draw_icon_button 
    (welcome_x) (welcome_y - 3*text_spacer) "" quit_color ;
  draw_quit (welcome_x) (welcome_y - 3*text_spacer);
  draw_icon_button 
    (welcome_x) (welcome_y - 4*text_spacer) "" back_color;
  draw_back (welcome_x) (welcome_y - 4*text_spacer);
  draw_icon_button 
    (welcome_x) (welcome_y - 5*text_spacer) "" back_color;
  draw_restart(welcome_x) (welcome_y - 5*text_spacer);
  set_color black;
  moveto (welcome_x + 25) (welcome_y - 3*text_spacer);
  draw_string " will exit you out of the game at any point.";
  moveto (welcome_x + 25) (welcome_y - 4*text_spacer);
  draw_string " will bring you back to the previous page.";
  moveto (welcome_x + 25) (welcome_y - 5*text_spacer);
  draw_string " will restart you during the game.";

  (** [page_rules ()] displays the instructions page*)
and page_rules () = 
  standard_header ();
  draw_string "   How to Play";
  moveto welcome_x welcome_y;
  draw_string "Welcome! We are excited to have you here.";
  moveto welcome_x (welcome_y - 15);
  draw_string "Before you play, here are a few things you need to know.";
  moveto welcome_x (welcome_y - 2*text_spacer);
  helper_buttonrules ();
  moveto welcome_x (welcome_y - 7*text_spacer);
  draw_string "Chess rules:";
  draw_text_block welcome_x (welcome_y - 7*text_spacer - 15) 15 rules;
  click_rules ()

(** [page_choose_color ()] displays the page to choose the color*)
and page_choose_color () = 
  standard_header ();
  moveto (title_x - 10) (color_button_y + 3*text_spacer);
  draw_string "  Player 1, choose your color.";
  draw_text_button color_button_x color_button_y "White" dc_color;
  draw_text_button (color_button_x + 150) color_button_y "Black" dc_color;
  click_color ()

(** [page_board s curr_player curr_state] displays string [s] at the top
    of the window, along with the board of [curr_state] according to who the 
    [curr_player] is.*)
and page_board s curr_player curr_state = 
  game_header ();
  draw_string s;
  moveto welcome_x welcome_y;
  draw_string "Select a piece to move.";
  draw_board board_topx board_topy curr_player curr_state;
  draw_captured captured_x captured_y curr_state;
  click_game curr_player curr_state s

(** [helper_moveto curr_player curr_state x y p] displays an error message
    if the [curr_player] tries to move their opponents piece [p], if not it 
    asks for the player's selection of location to move it to.*)
and helper_moveto curr_player curr_state x y p =
  match get_color p = curr_player with 
  | true -> 
    draw_string "Select a location on the board to move the piece to."; 
    draw_board board_topx board_topy curr_player curr_state;
    draw_captured captured_x captured_y curr_state;
    click_move p curr_player curr_state x y
  | false -> 
    page_board 
      "You are trying to move your opponents piece. Please try again." 
      curr_player curr_state

(**  [page_choose_piece x y curr_player curr_state] displays an error message
     if the [curr_player] chooses a location that doesn't have a piece, and if 
     not, processes that piece that was chosen, returning either an error 
     message or prompting the user for a location. *)
and page_choose_piece x y curr_player curr_state = 
  choose_piece_header x y curr_player curr_state;
  match get_piece x y (get_board curr_state) with 
  | None ->
    page_board 
      "You have picked a location without a piece. Please choose a piece."
      curr_player curr_state
  | Some p -> helper_moveto curr_player curr_state x y p

(** [helper_legal state p curr_player new_state] checks if the new_state has 
    ended in a tie or a winner, or if a pawn was promoted, and handles all of 
    those cases. If neither of those, it displays the board for the new player, 
    prompting them for a move.  *)
and helper_legalstate p curr_player new_state =
  match (get_promotion new_state), (get_winner new_state) with 
  | None, Winner None -> (
      let new_player = switch_curr_player curr_player in 
      let draw_false = set_draw_requested false new_state in
      page_board 
        ("Now it is " ^ (player_str new_player) ^ "'s turn.")
        new_player draw_false)
  | _, Draw -> (page_tie ())
  | _, Winner Some White -> (page_whitewin new_state)
  | _, Winner Some Black -> (page_blackwin new_state)
  | Some p, _  -> page_promote p curr_player new_state

(** [page_move_piece p curr_player curr_state loc2] checks whether the move
    of piece [p] to location [loc2] yields an illegal or legal state, and
     handles those cases*)
and page_move_piece p curr_player curr_state loc2 = 
  game_header ();
  match (move_piece p [loc2] curr_state curr_player) with 
  | Illegal x -> page_board x
                   curr_player curr_state
  | Legal new_state -> helper_legalstate p curr_player new_state

(** [page_promote p curr_player curr_state] displays the prompt for the user
    to handle their promoted pawn [p]. *)
and page_promote p curr_player curr_state = 
  quit_header ();
  draw_string "You have a pawn to promote!";
  moveto welcome_x (title_y - text_spacer);
  draw_string ((player_str curr_player) ^ 
               ", please choose a bishop, knight, rook, or queen to promote.");
  draw_board board_topx board_topy curr_player curr_state;
  draw_captured captured_x captured_y curr_state;
  draw_text_button bishop_button_x bishop_button_y "Bishop" dc_color;
  draw_text_button knight_button_x knight_button_y "Knight" dc_color;
  draw_text_button rook_button_x rook_button_y " Rook" dc_color;
  draw_text_button queen_button_x queen_button_y "Queen" dc_color;
  click_promote p curr_player curr_state

(** [handle_promote replace curr_player curr_state p] replaces the pawn [p]
    with the new rank [replace].*)
and handle_promote replace curr_player curr_state p =
  remove_piece p (get_board curr_state); 
  let pawn_dst = Piece.pos p in 
  let new_piece = from_tuple (replace, pawn_dst, curr_player) in 
  add_piece new_piece pawn_dst (get_board curr_state);
  let new_player = switch_curr_player curr_player in 
  page_board ("Now it is " ^ (player_str new_player) ^ "'s turn.")
    new_player curr_state

(** [page_restart prevpage curr_player curr_state string x y] displays the 
    page to restart the game, with the option to go back ot the previous page *)
and page_restart prevpage curr_player curr_state string x y = 
  quit_header ();
  draw_string "Do you want to restart?";
  draw_text_button yes_button_x yes_button_y " Yes" yes_color;
  draw_text_button no_button_x no_button_y "  No" no_color; 
  click_restart prevpage curr_player curr_state string x y "restart"

(** [page_draw prevpage curr_player curr_state string x y] displays the 
    page to request a draw, with the option to go back ot the previous page *)
and page_draw prevpage curr_player curr_state string x y = 
  quit_header ();
  draw_string (player_str curr_player ^ " has requested a draw.");
  moveto welcome_x (title_y - text_spacer);
  draw_string (player_str (switch_curr_player curr_player) ^ " do you accept?");
  draw_text_button yes_button_x yes_button_y " Yes" yes_color;
  draw_text_button no_button_x no_button_y "  No" no_color; 
  click_restart prevpage curr_player curr_state string x y "draw"

(** [page_concede prevpage curr_player curr_state string x y] displays the 
    page to concede, with the option to go back ot the previous page *)
and page_concede prevpage curr_player curr_state string x y = 
  quit_header ();
  draw_string (player_str curr_player ^ ", do you really wish to concede?");
  draw_text_button yes_button_x yes_button_y " Yes" yes_color;
  draw_text_button no_button_x no_button_y "  No" no_color; 
  click_restart prevpage curr_player curr_state string x y "concede"

(** [page_tie ()] displays the page when the game ends in a tie *)
and page_tie () = 
  quit_header (); 
  set_color white;
  draw_crown 30 400 250; 
  set_color black;
  draw_crown 315 400 250; 
  moveto 265 475;
  set_color black;
  draw_string "It's a tie!";
  make_button restart_button_x restart_button_y med_button_w med_button_h 
    " Play Again" dc_color;
  click_end ()

(** [page_whitewin()] displays the page when white wins the game *)
and page_whitewin state = 
  quit_header (); 
  set_color white;
  draw_crown 50 500 500; 
  moveto 265 590;
  set_color black;
  draw_string "White Won!";
  make_button restart_button_x restart_button_y med_button_w med_button_h 
    " Play Again" dc_color;
  draw_board board_topx (restart_button_y - 40) White state;
  click_end ()

(** [page_blackwin()] displays the page when black wins the game *)
and page_blackwin state = 
  quit_header (); 
  set_color black;
  draw_crown 50 500 500; 
  moveto 265 590;
  set_color black;
  draw_string "Black Won!";
  make_button restart_button_x restart_button_y med_button_w med_button_h 
    " Play Again" dc_color;
  draw_board board_topx (restart_button_y - 40) White state;
  click_end ()

(** [click_start ()] checks for click events in the start page and handles them
    according to the buttons that the user clicks *)
and click_start () = 
  let ev = wait_next_event [Button_down] in 
  match clicked "quit" ev, clicked "rules" ev, clicked "play" ev with 
  | true, _, _-> (close_graph (); print_quit ()) 
  | _, true, _ -> page_rules ()
  | _, _, true -> page_choose_color ()
  | _ -> click_start ()

(** [click_rules ()] checks for click events in the rules page and handles them
    according to the buttons that the user clicks *)
and click_rules () = 
  let ev = wait_next_event [Button_down] in 
  match clicked "quit" ev, clicked "back" ev with 
  | true, _ -> (close_graph (); print_quit ())
  | _, true -> start ()
  | _  -> click_rules ()

(** [click_color ()] checks for click events in the choose color page and 
    handles them according to the buttons that the user clicks *)
and click_color () = 
  let ev = wait_next_event [Button_down] in
  match clicked "quit" ev, clicked "back" ev, 
        clicked "white" ev, clicked "black" ev with
  | true, _, _, _ -> (close_graph (); print_quit ())
  | _, true, _, _ -> start ()
  | _, _, true, _ -> 
    page_board "Player 1, you go first!" White (State.init_state ())
  | _, _, _, true -> 
    page_board "Player 2, you go first!" White (State.init_state ())
  | _ -> click_color ()

(** [helper_drawreq] displays an error message if the [curr_player] tries to
    request a draw twice in one round, and if not, sets the draw_requested
     field in [curr_state] to be true.*)
and helper_drawreq curr_player curr_state prevpage s x y = 
  match get_draw_requested curr_state with 
  | true -> page_board 
              "You have already requested a draw in this round." 
              curr_player curr_state
  | false -> let draw_true = set_draw_requested true curr_state in
    page_draw prevpage curr_player draw_true s x y

(** [click_move] checks for click events in the choose piece page and handles 
    them according to the buttons that the user clicks *)
and click_move p curr_player curr_state x y = 
  let ev = wait_next_event [Button_down] in 
  match clicked "quit" ev, clicked "back" ev , clicked "board" ev, 
        clicked "draw" ev, clicked "concede" ev with 
  | true, _, _,_,_ -> (close_graph (); print_quit ())
  | _, true, _,_,_ -> page_restart "choosepiece" curr_player curr_state "" x y
  | _, _, true,_,_-> 
    begin match find_pos curr_player ev with 
      | (x, y) -> page_move_piece p curr_player curr_state
                    (string_of_int x ^ string_of_int y) end
  | _, _, _, true,_ -> 
    helper_drawreq curr_player curr_state "choosepiece" "" x y
  | _,_,_,_,true -> page_concede "choosepiece" curr_player curr_state "" x y
  | _ -> click_move p curr_player curr_state x y

(** [click_game] checks for click events in the board page and handles 
    them according to the buttons that the user clicks *)
and click_game curr_player curr_state s = 
  let ev = wait_next_event [Button_down] in 
  match clicked "quit" ev, clicked "back" ev , clicked "board" ev, 
        clicked "draw" ev, clicked "concede" ev with 
  | true, _, _,_,_-> (close_graph (); print_quit ())
  | _, true, _,_,_ -> page_restart "startgame" curr_player curr_state s 0 0 
  | _, _, true,_,_ -> 
    begin match find_pos curr_player ev with 
      | (x,y) -> page_choose_piece x y curr_player curr_state
    end
  | _, _, _, true,_ -> helper_drawreq curr_player curr_state "startgame" s 0 0 
  | _,_,_,_,true -> page_concede "startgame" curr_player curr_state s 0 0 
  | _ ->  click_game curr_player curr_state s

(** [helper_win curr_player] displays either the white win or black win page
    depending on who condedes or accepts the draw.*)
and helper_win curr_player curr_state= 
  match curr_player with 
  | White -> page_blackwin curr_state
  | Black -> page_whitewin curr_state

(** [helper_retartyes] displays either the starting page, concession winner 
    page, or the tie page depending on which button the user pressed.*)
and helper_restartyes curr_player page curr_state = 
  match page with 
  | "restart" -> start ()
  | "concede" -> helper_win curr_player curr_state
  | s -> page_tie ()

(** [helper_retartprev] displays the previous page that the [curr_player] was 
    on, before requesting a restart, draw, or concession*)
and helper_restartprev prevpage curr_player curr_state string x y = 
  match prevpage with 
  | "startgame" -> page_board string curr_player curr_state
  | s -> page_choose_piece x y curr_player curr_state

(** [click_restart] checks for click events in the restart page and handles 
    them according to the buttons that the user clicks *)
and click_restart prevpage curr_player curr_state string x y page = 
  let ev = wait_next_event [Button_down] in 
  match clicked "quit" ev, clicked "yes" ev, clicked "no" ev with 
  | true, _, _ -> (close_graph (); print_quit ())
  | _, true, _ -> helper_restartyes curr_player page curr_state
  | _, _, true ->  helper_restartprev prevpage curr_player curr_state string x y
  | _ -> click_restart prevpage curr_player curr_state string x y page

(** [click_promote] checks for click events in the promote page and handles 
    them according to the buttons that the user clicks *)
and click_promote p curr_player curr_state =
  let ev = wait_next_event [Button_down] in 
  match clicked "quit" ev, clicked "bishop" ev, clicked "knight" ev, 
        clicked "rook" ev, clicked "queen" ev with 
  | true,_,_,_,_ -> (close_graph (); print_quit ())
  | _,true,_,_,_ -> handle_promote Bishop curr_player curr_state p
  | _,_, true,_,_ -> handle_promote Knight curr_player curr_state p
  | _, _,_,true,_ -> handle_promote Rook curr_player curr_state p
  |_,_,_,_, true ->  handle_promote Queen curr_player curr_state p
  | _ -> click_promote p curr_player curr_state 

(** [click_end] checks for click events in the ending pages and handles 
    them according to the buttons that the user clicks *)
and click_end () = 
  let ev = wait_next_event [Button_down] in 
  match clicked "quit" ev, clicked "restart" ev with 
  | true, _ -> (close_graph (); print_quit ())
  | _, true -> start ()
  | _ -> click_end ()