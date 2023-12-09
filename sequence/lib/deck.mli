(* Define the type for a card. *)
type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades
  | OneEyed
  | TwoEyed

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type t = {
  suit : suit;
  rank : rank;
}

(* String representation of a card. *)
val to_string : t -> string

(* Get a list of unique "regular cards", not including special Jacks *)
val regular_cards : unit -> t list

(* Create a full deck of cards, considering special Jacks. *)
val full_deck : unit -> t list

(* Shuffle a deck of cards. *)
val shuffle : t list -> t list

(* Translate a string to a card *)
val card_of_string : string -> t

(* Translate a string to a rank *)
val rank_of_string : string -> rank

(* Translate a string to a suit *)
val suit_of_char : char -> suit
