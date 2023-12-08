(* game.ml *)
type t = {
  mutable current_player_id : int;
  players : Player.t array; (* array of players *)
  mutable deck : Deck.t list; (* remaining deck *)
  board : Board.t;
      (* game board *)
      (* current card to play *)
      (* mutable current_card : Deck.t; *)
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
    (* current_card = List.hd deck_shuffled; *)
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
      (* game.current_card <- card; *)
      Some card

(* Checks if the card is a special jack *)
let is_special_jack card =
  match card with
  | { Deck.rank = Jack; suit = _ } -> true
  | _ -> false

(* receive input from user *)
let rec get_card_square_and_id () =
  Printf.printf "Enter the card (format: Rank Suit, e.g., 'Ten Hearts'): ";
  let card_str = read_line () in
  let card =
    (* Convert card_str to a card, handling errors *)
    try Some (Deck.card_of_string card_str)
    with _ ->
      Printf.printf "Invalid card format.\n";
      None
  in
  Printf.printf "Enter the card ID (1 or 2): ";
  match (read_int_opt (), card) with
  | Some id, Some c when id = 1 || id = 2 -> Some (c, id)
  | _ ->
      Printf.printf "Invalid input. Try again.\n";
      get_card_square_and_id ()

(* Apply special functionality for one-eyed or two-eyed jacks *)
let apply_jack_effect board player_id card =
  match card.Deck.rank with
  | Jack -> begin
      match card.Deck.suit with
      | TwoEyed ->
          (* Two-eyed Jacks *)
          let rec place_chip_loop () =
            match get_card_square_and_id () with
            | Some (c, id) ->
                if Board.check_space (Reg_Card c) id board = Board.None then
                  Board.place_chip
                    (if player_id = 1 then Board.Red else Board.Blue)
                    (Reg_Card c) id board
                else (
                  Printf.printf "Space already occupied. Choose another.\n";
                  place_chip_loop ())
            | None -> ()
          in
          place_chip_loop ()
      | OneEyed ->
          (* One-eyed Jacks *)
          let rec remove_chip_loop () =
            match get_card_square_and_id () with
            | Some (c, id) ->
                if Board.check_space (Reg_Card c) id board <> Board.None then
                  Board.remove_chip (Reg_Card c) id board
                else (
                  Printf.printf
                    "No chip to remove at this space. Choose another.\n";
                  remove_chip_loop ())
            | None -> ()
          in
          remove_chip_loop ()
      | _ -> () (* shouldn't happen *)
    end
  | _ -> () (* shouldn't happen *)

let play_card game card id =
  let current_player = Array.get game.players (game.current_player_id - 1) in
  match Player.play_card current_player card with
  | Some updated_player ->
      Array.set game.players (game.current_player_id - 1) updated_player;
      if is_special_jack card then
        apply_jack_effect game.board game.current_player_id card
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
let check_game_over g = if Board.is_win g.board then Won 0 else Ongoing
let current_card = failwith "unimp"
