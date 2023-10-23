(* Defines type for board *)
type card = Reg_Card of Deck.t | Free_space

  type chip = Red | Blue | Free | None

  type square = {
    row : int;
    col : int;
    mutable chip : chip;
    card : card;
    id : int;
  }

  (* Representation type*)
  type t = square list list

  (* returns new board with no chips placed *)
  val init : t

  val square_to_string: square -> string

  (* modifies board [b] and places chip [ch] in the spot (r, c) *)
  val place_chip : chip -> int -> int -> t -> unit

  (* modifies board [b] and removes any chip in the spot (r, c) *)
  val remove_chip : int -> int -> t -> unit

  (* returns the chip in space (r,c). Returns None if its empty,
     Free if its the free space, or the color of the player who hold the space*)
  val check_space : int -> int -> t -> chip

  (* returns the card in space (r,c). returns Free_space if its the free space, or the color of the player who hold the space*)
  val check_card : int -> int -> t -> card

  (* returns true if there is a win on the board*)
  (* val is_win : t -> bool *)

  (* prints ascii version of the board *)
  val print_board : t -> unit 
