(* Defines type for board *)
open Deck

type card = Deck.t | Free_space
type chip = Red | Blue | Free | None
type square = {row: int; col :int; mutable chip: Chip; card: Card}
type t

(* returns new board with no chips placed *)
let init : t = failwith "Unimplimented"

(* modifies board [b] and places chip [ch] in the spot (r, c) *)
let place_chip (ch : chip) (r : int) (c : int) (b : t) : unit =
  failwith "Unimplimented"

(* modifies board [b] and removes any chip in the spot (r, c) *)
let remove_chip (r : int) (c : int) (b : t) : unit = failwith "Unimplimented"

(* returns the chip in space (r,c). Returns None if its empty,
   Free if its the free space, or the color of the player who hold the space*)
let check_space (r : int) (c : int) (b : t) : chip = failwith "Unimplimented"

(* returns true if there is a win on the board*)
let is_win (b : t) : bool = failwith "Unimplimented"

(* prints ascii version of the board *)
let print_board (b : t) : unit = failwith "Unimplimented"
