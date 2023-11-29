type t

(* Create a new player with a specified ID. *)
val create : int -> t

(* Deal cards to a player's hand. *)
val deal_cards : t -> Deck.t list -> t

(* Get the player's hand. *)
val get_hand : t -> Deck.t list

(* Play a card from the player's hand. *)
val play_card : t -> Deck.t -> t

val has_card : player -> card -> bool 

val get_id : player -> int