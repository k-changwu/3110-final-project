(* game.ml *)
type t = {
  mutable current_player_id : int;
  players : Player.t array; (* array of players *)
  mutable deck : Deck.t list; (* remaining deck *)
  board : Board.t; (* game board *)
  mutable current_card : Deck.t; (* current card to play *)
}

type result =
  | Ongoing
  | Won of int
  | Draw

let start () =
  let deck_shuffled = Deck.full_deck () |> Deck.shuffle in
  {
    current_player_id = 1;
    players = [| Player.create 1; Player.create 2 |];
    (* initialize two players *)
    deck = deck_shuffled;
    board = Board.init;
    (* initialize a new game board *)
    current_card = List.hd deck_shuffled;
    (* current card to play *)
  }

let current_player game = game.current_player_id

let next_player game =
  game.current_player_id <- (if game.current_player_id = 1 then 2 else 1)

let draw_card game =
  match game.deck with
  | [] -> None (* deck is empty *)
  | h :: t ->
      game.deck <- t;
      game.current_card <- h;
      Some h

let play_turn = failwith "unimp"
let check_game_over = failwith "unimp"
let current_card = failwith "unimp"
