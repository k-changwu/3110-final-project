open Yojson.Basic.Util
open Deck

(* Defines type for board *)
type card =
  | Reg_Card of Deck.t
  | Free_space

type chip =
  | Red
  | Blue
  | Free
  | None

type square = {
  row : int;
  col : int;
  mutable chip : chip;
  card : card;
  id : int;
}

type t = square list list

(* returns new board with no chips placed *)
let init : t =
  let file_init = "board.txt" in
  let str_init = file_init |> In_channel.open_text |> In_channel.input_all in
  let json = Yojson.Basic.from_string str_init in
  let rows =
    json |> Yojson.Basic.Util.to_list
    |> List.map (fun row ->
           row |> Yojson.Basic.Util.to_list
           |> List.map (fun square_json ->
                  let row =
                    square_json
                    |> Yojson.Basic.Util.member "row"
                    |> Yojson.Basic.Util.to_int
                  in
                  let col =
                    square_json
                    |> Yojson.Basic.Util.member "col"
                    |> Yojson.Basic.Util.to_int
                  in
                  let chip =
                    square_json
                    |> Yojson.Basic.Util.member "chip"
                    |> Yojson.Basic.Util.to_string
                    |> function
                    | "Red" -> Red
                    | "Blue" -> Blue
                    | "Free" -> Free
                    | _ -> None
                  in
                  let card =
                    square_json
                    |> Yojson.Basic.Util.member "card"
                    |> Yojson.Basic.Util.to_string
                    |> function
                    | "Free_space" -> Free_space
                    | _ -> Reg_Card { suit = Spades; rank = Two }
                    (* Replace with the appropriate suit and rank *)
                  in
                  let id =
                    square_json
                    |> Yojson.Basic.Util.member "id"
                    |> Yojson.Basic.Util.to_int
                  in
                  { row; col; chip; card; id }))
  in
  rows

(*prints the to_string representation of each card*)
let square_to_string square =
  match square.card with
  | Free_space -> "Free"
  | Reg_Card card -> Deck.to_string card ^ string_of_int square.id

(*prints the board*)
let print_board board =
  List.iter
    (fun row ->
      List.iter
        (fun square -> Printf.printf "%s\t" (square_to_string square))
        row;
      print_endline "")
    board

(* modifies [square] so chip [ch] is placed in it if it has card [c] and id
   [id]*)
let update_square ch c id square =
  if square.card = c && square.id = id then square.chip <- ch

(* modifies board [b] and places chip [ch] in the square with card [c] and id
   [id]. Requires: c is a valid card and id is a valid id *)
let place_chip (ch : chip) (c : card) (id : int) (b : t) : unit =
  List.iter (List.iter (update_square ch c id)) b

(* modifies board [b] and removes any chip in the square with card [c] and id
   [i] *)
let remove_chip (c : card) (id : int) (b : t) : unit =
  List.iter (List.iter (update_square None c id)) b

(* returns the chip in square with card [c] and id [i]. Returns None if its
   empty, Free if its the free space, or the color of the player who hold the
   space*)
let check_space (c : card) (id : int) (b : t) : chip =
  let rec find_chip s =
    match s with
    | [] -> None
    | h :: t -> if h.card = c && h.id = id then h.chip else find_chip t
  in
  find_chip (List.flatten b)

let check_card (r : int) (c : int) (b : t) : card = 
  (List.nth (List.nth b r)
   c).card
  
(*return a list of squares representing the specified column.*)
  let extract_col (b : t) col_index =
    List.map (fun row -> List.nth row col_index) b

  (*extracts the left-to-right diagonal from the board [b]*)
  let extract_diag1 (b : t) =
    List.mapi (fun i row -> List.nth row i) b
  
    (*extracts the right-to-left diagonal from the board [b]*)
  let extract_diag2 (b : t) =
    List.mapi (fun i row -> List.nth row (List.length row - 1 - i)) b

     (*checks if there is a winning sequence in a line*)
   let check_line_for_win line =
    let rec aux count = function
      | [] -> count >= 4
      | {chip = None; _} :: t -> aux 0 t
      | h :: ( {chip = ch; _} :: _ as t) when h.chip = ch -> aux (count + 1) t
      | _ :: t -> aux 1 t
    in
    aux 0 line

(* returns true if there is a win on the board*)
let is_win (b : t) : bool =
let row_win = List.exists check_line_for_win b in
let col_win = List.exists (fun i -> 
  check_line_for_win (extract_col b i)) (List.init (List.length (List.hd b)) (fun x -> x)) in
let diag1_win = check_line_for_win (extract_diag1 b) in
let diag2_win = check_line_for_win (extract_diag2 b) in
row_win || col_win || diag1_win || diag2_win




  
