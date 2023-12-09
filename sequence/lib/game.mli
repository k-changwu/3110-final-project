(* game.mli *)

type t

(* Represents the result of a game, whether it's ongoing, won, or a draw. *)
type result =
  | Ongoing
  | Won
  | Draw

(* Starts a new game with two players. *)
val start : bool -> t

(* Gets the current player's ID. *)
val current_player_id : t -> int

(* Handles a player's turn with the given card and board position. Returns an
   updated game state. *)
val play_turn : t -> unit

(* Checks the game state for a win or draw. *)
val check_game_over : t -> result
