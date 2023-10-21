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
    id : int;
  }

  type t = square list list

  (* returns new board with no chips placed *)
  let init : t =
    [
      (* row 0 *)
      [
        { row = 0; col = 0; chip = Free; card = Free_space; id = 0 };
        {
          row = 0;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Ten };
          id = 1;
        };
        {
          row = 0;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Queen };
          id = 1;
        };
        {
          row = 0;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Spades; rank = King };
          id = 1;
        };
        {
          row = 0;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Ace };
          id = 1;
        };
        {
          row = 0;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Two };
          id = 0;
        };
        {
          row = 0;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Three };
          id = 0;
        };
        {
          row = 0;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Four };
          id = 0;
        };
        {
          row = 0;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Five };
          id = 0;
        };
        { row = 0; col = 9; chip = Free; card = Free_space; id = 0 };
      ];
      (* row 1 *)
      [
        {
          row = 1;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Nine };
          id = 1;
        };
        {
          row = 1;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Ten };
          id = 1;
        };
        {
          row = 1;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Nine };
          id = 1;
        };
        {
          row = 1;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Eight };
          id = 1;
        };
        {
          row = 1;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Seven };
          id = 1;
        };
        {
          row = 1;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Six };
          id = 1;
        };
        {
          row = 1;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Five };
          id = 1;
        };
        {
          row = 1;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Four };
          id = 1;
        };
        {
          row = 1;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Three };
          id = 1;
        };
        {
          row = 1;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Six };
          id = 0;
        };
      ];
      (* row 2 *)
      [
        {
          row = 2;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Eight };
          id = 1;
        };
        {
          row = 2;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Queen };
          id = 1;
        };
        {
          row = 2;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Seven };
          id = 1;
        };
        {
          row = 2;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Eight };
          id = 1;
        };
        {
          row = 2;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Nine };
          id = 1;
        };
        {
          row = 2;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Ten };
          id = 1;
        };
        {
          row = 2;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Queen };
          id = 1;
        };
        {
          row = 2;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = King };
          id = 1;
        };
        {
          row = 2;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Two };
          id = 1;
        };
        {
          row = 2;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Seven };
          id = 0;
        };
      ];
      (* row 3 *)
      [
        {
          row = 3;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Seven };
          id = 1;
        };
        {
          row = 3;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = King };
          id = 1;
        };
        {
          row = 3;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Six };
          id = 1;
        };
        {
          row = 3;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Two };
          id = 0;
        };
        {
          row = 3;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Ace };
          id = 0;
        };
        {
          row = 3;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = King };
          id = 0;
        };
        {
          row = 3;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Queen };
          id = 0;
        };
        {
          row = 3;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Ace };
          id = 1;
        };
        {
          row = 3;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Two };
          id = 0;
        };
        {
          row = 3;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Eight };
          id = 0;
        };
      ];
      (* row 4 *)
      [
        {
          row = 4;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Six };
          id = 1;
        };
        {
          row = 4;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Ace };
          id = 1;
        };
        {
          row = 4;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Five };
          id = 1;
        };
        {
          row = 4;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Three };
          id = 0;
        };
        {
          row = 4;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Four };
          id = 0;
        };
        {
          row = 4;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Three };
          id = 0;
        };
        {
          row = 4;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Ten };
          id = 0;
        };
        {
          row = 4;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Ace };
          id = 0;
        };
        {
          row = 4;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Three };
          id = 0;
        };
        {
          row = 4;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Nine };
          id = 0;
        };
      ];
      (* row 5 *)
      [
        {
          row = 5;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Five };
          id = 1;
        };
        {
          row = 5;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Two };
          id = 1;
        };
        {
          row = 5;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Four };
          id = 1;
        };
        {
          row = 5;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Four };
          id = 0;
        };
        {
          row = 5;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Five };
          id = 0;
        };
        {
          row = 5;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Two };
          id = 0;
        };
        {
          row = 5;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Nine };
          id = 0;
        };
        {
          row = 5;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = King };
          id = 0;
        };
        {
          row = 5;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Four };
          id = 0;
        };
        {
          row = 5;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Ten };
          id = 0;
        };
      ];
      (* row 6 *)
      [
        {
          row = 6;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Four };
          id = 1;
        };
        {
          row = 6;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Three };
          id = 1;
        };
        {
          row = 6;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Three };
          id = 1;
        };
        {
          row = 6;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Five };
          id = 0;
        };
        {
          row = 6;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Six };
          id = 0;
        };
        {
          row = 6;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Seven };
          id = 0;
        };
        {
          row = 6;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Eight };
          id = 0;
        };
        {
          row = 6;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Queen };
          id = 0;
        };
        {
          row = 6;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Five };
          id = 0;
        };
        {
          row = 6;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Queen };
          id = 0;
        };
      ];
      (* row 7 *)
      [
        {
          row = 7;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Three };
          id = 1;
        };
        {
          row = 7;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Four };
          id = 1;
        };
        {
          row = 7;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Two };
          id = 1;
        };
        {
          row = 7;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Six };
          id = 0;
        };
        {
          row = 7;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Seven };
          id = 0;
        };
        {
          row = 7;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Eight };
          id = 0;
        };
        {
          row = 7;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Nine };
          id = 0;
        };
        {
          row = 7;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Ten };
          id = 0;
        };
        {
          row = 7;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Six };
          id = 0;
        };
        {
          row = 7;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = King };
          id = 0;
        };
      ];
      (* row 7 *)
      [
        {
          row = 7;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Three };
          id = 1;
        };
        {
          row = 7;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Four };
          id = 1;
        };
        {
          row = 7;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Two };
          id = 1;
        };
        {
          row = 7;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Six };
          id = 0;
        };
        {
          row = 7;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Seven };
          id = 0;
        };
        {
          row = 7;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Eight };
          id = 0;
        };
        {
          row = 7;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Nine };
          id = 0;
        };
        {
          row = 7;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Ten };
          id = 0;
        };
        {
          row = 7;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Six };
          id = 0;
        };
        {
          row = 7;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = King };
          id = 0;
        };
      ];
      (* row 8 *)
      [
        {
          row = 8;
          col = 0;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Two };
          id = 1;
        };
        {
          row = 8;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Five };
          id = 1;
        };
        {
          row = 8;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Ace };
          id = 0;
        };
        {
          row = 8;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Spades; rank = King };
          id = 0;
        };
        {
          row = 8;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Queen };
          id = 0;
        };
        {
          row = 8;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Ten };
          id = 0;
        };
        {
          row = 8;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Nine };
          id = 0;
        };
        {
          row = 8;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Eight };
          id = 0;
        };
        {
          row = 8;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Spades; rank = Seven };
          id = 0;
        };
        {
          row = 8;
          col = 9;
          chip = None;
          card = Reg_Card { suit = Diamonds; rank = Ace };
          id = 0;
        };
      ];
      (* row 0 *)
      [
        { row = 9; col = 0; chip = Free; card = Free_space; id = 0 };
        {
          row = 9;
          col = 1;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Six };
          id = 1;
        };
        {
          row = 9;
          col = 2;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Seven };
          id = 1;
        };
        {
          row = 9;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Eight };
          id = 1;
        };
        {
          row = 9;
          col = 4;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Nine };
          id = 1;
        };
        {
          row = 9;
          col = 5;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Ten };
          id = 1;
        };
        {
          row = 9;
          col = 6;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Queen };
          id = 1;
        };
        {
          row = 9;
          col = 7;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = King };
          id = 1;
        };
        {
          row = 9;
          col = 8;
          chip = None;
          card = Reg_Card { suit = Clubs; rank = Ace };
          id = 1;
        };
        { row = 9; col = 9; chip = Free; card = Free_space; id = 0 };
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
  let remove_chip (r : int) (c : int) (b : t) : unit =
    (List.nth (List.nth b r) c).chip <- None

  (* returns the chip in space (r,c). Returns None if its empty,
     Free if its the free space, or the color of the player who hold the space*)
  let check_space (r : int) (c : int) (b : t) : chip =
    (List.nth (List.nth b r) c).chip

  let check_card (r : int) (c : int) (b : t) : card =
    (List.nth (List.nth b r) c).card

  (* returns true if there is a win on the board*)
  let is_win (b : t) : bool = failwith "Unimplimented"

  (* prints ascii version of the board *)
  let print_board (b : t) : unit = failwith "Unimplimented"
end
