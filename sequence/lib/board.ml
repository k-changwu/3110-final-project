open Deck

(* Defines type for board *)
module Board = struct
  type card = Reg_Card of Deck.t | Free_space
  type chip = Red | Blue | Free | None

  type square = {
    row : int;
    col : int;
    mutable chip : chip;
    card : card;
    id : int option;
  }

  type t = square list list

  (* returns new board with no chips placed *)
  let init : t =
    [
      [
        { row = 0; col = 0; chip = Free; card = Free_space; id = Some 1 };
        {
          row = 0;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Ten };
          id = Some 1;
        };
        {
          row = 0;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Queen };
          id = Some 1;
        };
        {
          row = 0;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Spades; rank = King };
          id = Some 1;
        };
        {
          row = 0;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Ace };
          id = Some 1;
        };
        {
          row = 0;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Two };
          id = Some 0;
        };
        {
          row = 0;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Three };
          id = Some 0;
        };
        {
          row = 0;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Four };
          id = Some 0;
        };
        {
          row = 0;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Five };
          id = Some 0;
        };
        { row = 0; col = 9; chip = Free; card = Free_space; id = Some 1 };
      ];
    ]

  let square_to_string square =
    match square.card with
    | Free_space -> "Free"
    | Reg_Card card ->
      Deck.to_string card ^ string_of_int square.id

  (*prints the board*)
  let print_board board =
    List.iter (fun row -> 
      List.iter (fun square -> Printf.printf "%s\t" (square_to_string square)) row;
      print_endline ""
    ) board

  (* modifies board [b] and places chip [ch] in the spot (r, c) *)
  let place_chip (ch : chip) (r : int) (c : int) (b : t) : unit =
    (List.nth (List.nth b r) c).chip <- ch

  (* modifies board [b] and removes any chip in the spot (r, c) *)
  let remove_chip (r : int) (c : int) (b : t) : unit = failwith "Unimplimented"

  (* returns the chip in space (r,c). Returns None if its empty,
     Free if its the free space, or the color of the player who hold the space*)
  let check_space (r : int) (c : int) (b : t) : chip = failwith "Unimplimented"

  (* returns true if there is a win on the board*)
  let is_win (b : t) : bool = failwith "Unimplimented"

  (* prints ascii version of the board *)
  let print_board (b : t) : unit = failwith "Unimplimented"
end
