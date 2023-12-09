(* game.mli *)

type t

(* Represents the result of a game, whether it's ongoing, won, or a draw. *)
type result =
  | Ongoing
  | Won
  | Draw

(* Starts a new game with two players. *)
val start : unit -> t

(* Gets the current player's ID. *)
val current_player_id : t -> int

(* Gets the current player *)
val current_player: t -> Player.t

(* Handles a player's turn with the given card and board position. Returns an
   updated game state. *)
val play_turn : t -> int -> int -> t

(* Checks the game state for a win or draw. *)
val check_game_over : t -> result

(* Returns the card that the current player can play. *)
val current_card : t -> Deck.t
