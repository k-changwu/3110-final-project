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
  | card :: remaining_deck ->
      (* Update the current player's hand *)
      let current_player =
        Array.get game.players (game.current_player_id - 1)
      in
      let updated_player = Player.add_card current_player card in
      Array.set game.players (game.current_player_id - 1) updated_player;

      (* Update the deck *)
      game.deck <- remaining_deck;

      (* Optionally, update the current card *)
      game.current_card <- card;

      Some card

(* Checks if the card is a special jack *)
let is_special_jack card =
  match card with
  | { Deck.rank = Jack; suit = _ } -> true
  | _ -> false

(* Apply special functionality for one-eyed or two-eyed jacks *)
let apply_jack_effect board card player_id =
  match card with
  | { Deck.rank = Jack; suit = _ } ->
      (* Implement jacks later *)
      ()
  | _ -> ()

let play_card game card id =
  let current_player = Array.get game.players (game.current_player_id - 1) in
  match Player.play_card current_player card with
  | Some updated_player ->
      Array.set game.players (game.current_player_id - 1) updated_player;
      if is_special_jack card then
        apply_jack_effect game.board card game.current_player_id
      else
        Board.place_chip
          (if game.current_player_id = 1 then Board.Red else Board.Blue)
          (Reg_Card card) 0 game.board;
      (* Assumes ID is 0 for regular cards *)
      Some game
  | None ->
      Printf.printf "You don't have that card. Please choose another.\n";
      None

let play_turn = failwith "unimp"
let check_game_over = failwith "unimp"
let current_card = failwith "unimp"
