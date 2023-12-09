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

let parse_card json_card =
  match json_card with
  | "Free_space" -> Free_space
  | str ->
      Scanf.sscanf str "Reg_Card { suit : %s@, rank : %s }"
        (fun suit_str rank_str ->
          let suit =
            match suit_str with
            | "Spades" -> Spades
            | "Diamonds" -> Diamonds
            | "Hearts" -> Hearts
            | "Clubs" -> Clubs
            | _ -> failwith ("Unknown suit: " ^ suit_str)
          in
          let rank =
            match rank_str with
            | "Two" -> Two
            | "Three" -> Three
            | "Four" -> Four
            | "Five" -> Five
            | "Six" -> Six
            | "Seven" -> Seven
            | "Eight" -> Eight
            | "Nine" -> Nine
            | "Ten" -> Ten
            | "Jack" -> Jack
            | "Queen" -> Queen
            | "King" -> King
            | "Ace" -> Ace
            | _ -> failwith ("Unknown rank: " ^ rank_str)
          in
          Reg_Card { suit; rank })

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
                    |> Yojson.Basic.Util.to_string |> parse_card
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
  let start =
    match square.chip with
    | Red -> "\027[31m "
    | Blue -> "\027[34m "
    | Free -> "\027[32m "
    | None -> "\027[39m "
  in
  match square.card with
  | Free_space -> "\027[32m Free"
  | Reg_Card card -> start ^ Deck.to_string card ^ string_of_int square.id

(*prints the board*)
let print_board board =
  List.iter
    (fun row ->
      List.iter
        (fun square -> Printf.printf "%s\t\027[39m" (square_to_string square))
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

let rec sublist_from_index i lst =
  if i <= 0 then lst
  else
    match lst with
    | [] -> []
    | _ :: tl -> sublist_from_index (i - 1) tl

let row_win b =
  let rec win_in_row chip count r =
    match r with
    | [] -> if count = 5 then true else false
    | square :: t ->
        if square.chip = None then win_in_row None 0 t
        else if chip = Free || chip = square.chip then
          if count = 4 then true else win_in_row square.chip (count + 1) t
        else if square.chip = Free then
          if count = 4 then true
          else if chip = None then win_in_row Free (count + 1) t
          else win_in_row chip (count + 1) t
        else win_in_row square.chip 1 t
  in
  List.fold_left ( || ) false (List.map (win_in_row None 0) b)

let col_win b =
  let transpose_square_list lst =
    List.map
      (fun i -> List.map (fun row -> List.nth row i) lst)
      (List.init (List.length (List.hd lst)) (fun x -> x))
  in
  row_win (transpose_square_list b)

let diag_win b =
  let rec check_diag chip count board =
    match board with
    | [] -> if count = 5 then true else false
    | [] :: t -> if count = 5 then true else false
    | h :: r -> (
        match h with
        | [] -> if count = 5 then true else false
        | square :: t ->
            if
              match r with
              | [] ->
                  if chip = Free || chip = square.chip || square.chip = Free
                  then if count = 4 then true else false
                  else false
              | hr2 :: tr2 ->
                  let col = List.length hr2 - List.length t in
                  (* end of row *)
                  if col >= List.length hr2 then
                    if count = 5 then true else false
                  else if square.chip = None then
                    (* square is empty, continue down the diag *)
                    check_diag None 0 (sublist_from_index col hr2 :: tr2)
                  else if chip = Free || chip = square.chip then
                    (* add one to count and continue down the diag *)
                    if count = 4 then true
                    else
                      check_diag square.chip (count + 1)
                        (sublist_from_index col hr2 :: tr2)
                  else if square.chip = Free then
                    if count = 4 then true
                    else if chip = None then
                      check_diag Free (count + 1)
                        (sublist_from_index col hr2 :: tr2)
                    else
                      check_diag chip (count + 1)
                        (sublist_from_index col hr2 :: tr2)
                  else
                    (* mismatch. set count back to 1 and keep going *)
                    check_diag square.chip 1 (sublist_from_index col hr2 :: tr2)
            then true
            else false)
  in
  let rec start_diag bd =
    let rec start_row b =
      match b with
      | [] -> false
      | [] :: _ -> false
      | (h1 :: t1) :: t ->
          if check_diag None 0 b then true else start_row (t1 :: t)
    in

    if start_row bd then true
    else
      let rec start_col b =
        match b with
        | [] -> false
        | h :: t -> (
            match h with
            | [] -> false
            | h1 :: t1 -> if check_diag None 0 b then true else start_col t)
      in
      start_col bd
  in
  start_diag b

let anti_diag_win b =
  let reverse_rows matrix = List.map List.rev matrix in
  diag_win (reverse_rows b)

(* returns true if there is a win on the board*)
let is_win (b : t) : bool =
  if row_win b then true
  else if col_win b then true
  else if diag_win b then true
  else if anti_diag_win b then true
  else false
