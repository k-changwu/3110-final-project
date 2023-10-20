(* Define the type for a card. *)
type suit = Hearts | Diamonds | Clubs | Spades | OneEyed | TwoEyed
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type t = { suit : suit; rank : rank }

(* String representation of a card. *)
val to_string : t -> string

(* Create a full deck of cards, considering special Jacks. *)
val full_deck : unit -> t list

(* Shuffle a deck of cards. *)
val shuffle : t list -> t list
