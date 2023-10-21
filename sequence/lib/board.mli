(* Defines type for board *)
open Deck
type card : Deck.t | Free_space
type chip : Red | Blue | Free | None
type square : {row: int; col :int; mutable chip: Chip; card: Card}
type t : square list list

(* returns new board with no chips placed *)
val init : t

(* modifies board [b] and places chip [ch] in the spot (r, c) *)
val place_chip (ch : chip) (r : int) (c:int) (b: t) : unit

(* modifies board [b] and removes any chip in the spot (r, c) *)
val remove_chip (r : int) (c:int) (b: t) : unit

(* returns the chip in space (r,c). Returns None if its empty, 
   Free if its the free space, or the color of the player who hold the space*)
val check_space (r: int) (c: int) (b: t) : chip 

(* returns true if there is a win on the board*)
val is_win (b: t) : bool

(* prints ascii version of the board *)
val print_board (b:t) : unit

