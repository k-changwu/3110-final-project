(* game.ml *)
type t = {
  mutable current_player_id : int;
  players : Player.t array; (* array of players *)
  mutable deck : Deck.t list; (* remaining deck *)
  board : Board.t; (* game board *)
}

type result =
  | Ongoing
  | Won
  | Draw

let draw_cards num_cards deck =
  let rec draw n acc_deck acc_drawn =
    if n <= 0 then (acc_deck, List.rev acc_drawn)
      (* Reverse to maintain original order *)
    else
      match acc_deck with
      | [] -> (acc_deck, List.rev acc_drawn) (* In case the deck runs out *)
      | card :: rest_deck -> draw (n - 1) rest_deck (card :: acc_drawn)
  in
  draw num_cards deck []

let start () =
  let deck_shuffled = Deck.full_deck () |> Deck.shuffle in
  let new_deck, drawn_cards_1 = draw_cards 7 deck_shuffled in
  let newest_deck, drawn_cards_2 = draw_cards 7 new_deck in
  {
    current_player_id = 1;
    players = [| Player.create 1 drawn_cards_1; Player.create 2 drawn_cards_2 |];
    (* initialize two players *)
    deck = newest_deck;
    board = Board.init;
    (* initialize a new game board *)
  }

let current_player_id game = game.current_player_id
let current_player game = Array.get game.players (game.current_player_id - 1)

let next_player game =
  game.current_player_id <- (if game.current_player_id = 1 then 2 else 1)

let draw_card game =
  match game.deck with
  | [] -> None (* deck is empty *)
  | card :: remaining_deck ->
      (* Update the current player's hand *)
      let current_player = current_player game in
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

let rec ask_for_card player =
  Printf.printf "Which card do you want to play: ";
  let card_str = read_line () in
  try
    let card = Deck.card_of_string card_str in
    if List.exists (fun c -> c = card) (Player.get_hand player) then card
    else (
      Printf.printf "That is not a valid card!\n";
      ask_for_card player)
  with Failure _ ->
    Printf.printf "Invalid card format!\n";
    ask_for_card player

let rec ask_for_square () =
  Printf.printf "Which square (1 or 0): ";
  match read_int_opt () with
  | Some square when square = 0 || square = 1 -> square
  | _ ->
      Printf.printf "Invalid input. Try again.\n";
      ask_for_square ()

let rec get_card_square_and_id () =
  Printf.printf "Enter the card (format: RankSuit, e.g., '10H'): ";
  let card_str = read_line () in
  try
    let card = Deck.card_of_string card_str in
    Printf.printf "Enter the card ID (1 or 2): ";
    match read_int_opt () with
    | Some id when id = 1 || id = 2 -> Some (card, id)
    | _ ->
        Printf.printf "Invalid card ID. Try again.\n";
        get_card_square_and_id ()
  with Failure _ ->
    Printf.printf "Invalid card format. Try again.\n";
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

let check_game_over g = if Board.is_win g.board then Won else Ongoing

let rec play_card game current_player =
  let card = ask_for_card current_player in
  let card_effect () =
    match card with
    | { suit = _; rank = Jack } ->
        apply_jack_effect game.board game.current_player_id card;
        ignore (Player.play_card current_player card)
    | _ ->
        let square = ask_for_square () in

        (* Check if the card can be played *)
        if Board.check_space (Board.Reg_Card card) square game.board = None then (
          (* Call Board.place_chip with the arguments card & square *)
          Board.place_chip
            (if game.current_player_id = 1 then Board.Red else Board.Blue)
            (Board.Reg_Card card) square game.board;
          Printf.printf "Placed %s Token on %s%d\n"
            (if game.current_player_id = 1 then "Red" else "Blue")
            (Deck.to_string card) square)
        else (
          Printf.printf
            "That move is not possible. Please choose another card or square.\n";
          play_card game current_player)
  in

  card_effect ();

  (* Update the player's hand *)
  match Player.play_card current_player card with
  | Some updated_player ->
      (* Update the array with the new player state *)
      Array.set game.players (game.current_player_id - 1) updated_player
  | None ->
      (* Handle the case where the card is not in the player's hand - shouldn't
         happen as ask_for_card forces card in hand *)
      Printf.printf "Card not in hand. Please choose another card.\n"

let play_turn game =
  (* Print the board *)
  Board.print_board game.board;

  let current_player = current_player game in
  Printf.printf "Player %d's turn.\n" game.current_player_id;
  Printf.printf "Your Hand: %s\n"
    (Player.hand_to_string (Player.get_hand current_player));

  play_card game current_player;
  (* Check if the game is over *)
  (if check_game_over game = Won then (
     Board.print_board game.board;
     Printf.printf "Player %d wins!\n" game.current_player_id)
   else
     (* Draw a card for the player *)
     match draw_card game with
     | Some card -> Printf.printf "You drew %s\n" (Deck.to_string card)
     | None -> Printf.printf "No more cards to draw. The game ends in a draw \n");

  (* Change players *)
  game.current_player_id <- (if game.current_player_id = 1 then 2 else 1)
