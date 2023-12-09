type t

(* Create a new player with a specified ID. *)
val create : int -> Deck.t list -> t

(* Get the player's hand. *)
val get_hand : t -> Deck.t list

(* converts a player's hand to a string representation *)
val hand_to_string : Deck.t list -> string

(* Play a card from the player's hand such that the player's hand no longer has
   that card*)
val play_card : t -> Deck.t -> t option
val has_card : t -> Deck.t -> bool
val get_id : t -> int

(* Adds a card to a player's hand *)
val add_card : t -> Deck.t -> t
