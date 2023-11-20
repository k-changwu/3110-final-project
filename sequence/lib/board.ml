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
  let file_init = "data" ^ Filename.dir_sep ^ "board.txt" in
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

(* let check_card (r : int) (c : int) (b : t) : card = (List.nth (List.nth b r)
   c).card *)

(* returns true if there is a win on the board*)
(* let is_win (b : t) : bool = failwith "Unimplimented" *)
