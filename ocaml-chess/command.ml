type position_phrase = string list

type command = 
  | Move of position_phrase
  | Concede
  | Quit
  | Draw

exception Empty

exception Malformed

(** [check_if_int str] is true only when the first two characters of [str] are 
    characters 0 to 7 and false otherwise *)
let check_if_int str =
  str.[0] >= 'a' && str.[0] <= 'h' && 
  int_of_char str.[1] >= 49 && int_of_char str.[1] <= 56

(** [test_positio_syntax t] returns a list of an empty string if [t] does not 
    follow valid position_phrase syntax, otherwise it returns [t]. *)
let test_position_syntax t = 
  match t with 
  | a :: b :: [] when String.length a = 2  && String.length b = 2  -> 
    if check_if_int a && check_if_int b then t else [""]
  | _ -> [""]

let parse str =
  let str_list = String.split_on_char ' ' str 
  in
  let word_list = List.filter (fun x -> x <> "") str_list
  in 
  match word_list with
  | [] -> raise Empty
  | h :: [] when h = "quit" -> Quit
  | h :: [] when h = "concede" -> Concede
  | h :: [] when h = "draw" -> Draw
  | h :: t when h = "move" && List.length t = 2 -> begin
      match test_position_syntax t with
      | [""] -> raise Malformed
      | _ -> Move t
    end 
  | _ -> raise Malformed