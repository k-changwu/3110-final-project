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

(*Applies the jack effect*)
val apply_jack_effect : Board.t -> int -> Deck.t -> unit

(*Applies a special effect based on a Jack card played during the game.*)
val get_card_square_and_id : unit -> (Deck.t * int) option

(* Requests or determines a square on the board for a player's action. *)
val ask_for_square : unit -> int

(* Asks a player to choose a card from their hand. *)
val ask_for_card: Player.t -> Deck.t

(*Determines if a given card from the deck is a special 
   Jack card (like One-Eyed or Two-Eyed Jacks)*)
val is_special_jack : Deck.t -> bool

(*Allows a player to draw a card from the deck in the current game state.*)
val draw_card : t -> Deck.t option

(*Advances the game to the next player's turn*)
val next_player: t -> unit

(*Returns the current active player based on the game state*)
val current_player: t -> Player.t

(*player playing a card*)
val play_card : t -> Player.t -> unit

(*player plays a turn*)
val play_turn: t -> unit





