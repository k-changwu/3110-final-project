open Sequence.Game
open Sequence.Player
open Sequence.Board
open Sequence.Deck

let instructions () =
  print_endline "Sequence is a two player game.";
  print_endline
    "A sequence is A connected series of five of the same colored chip either \
     up or down, across or diagonally.";
  print_endline "The first player to make ONE sequence wins the game.\n";
  print_endline
    "Players alternate turns. Players will take turns selecting a card of \
     their choice from their hand and claiming a spot on the game board \
     corresponding to that card. The card is discarded and the player will \
     have a new card as well as all their unplayed cards in their hand for \
     next turn";
  print_endline
    "Each card has two corresponding spots on the game board.  A player can \
     play on either one of the card spaces as long as it is not already \
     covered by another marker chip. Once a marker chip has been played, it \
     cannot be removed by an opponent except when using a one-eyed Jack as \
     explained below.\n";
  print_endline "The Jacks";
  print_endline
    "Two eyed Jacks are wild cards that can be used to claim any unclaimed spot";
  print_endline
    "One eyed Jacks are anti-wild. They can be used to free any claimed spot \
     on the board.\n"

let rec game_loop game =
  match Sequence.Game.check_game_over game with
  | Ongoing ->
      Sequence.Game.play_turn game;
      game_loop game
  | _ -> Printf.printf "Game over!\n"

let () =
  print_endline "\nWelcome to Sequence!\n";
  instructions ();
  print_endline "With that out of the way, let's begin the game!";
  let g = Sequence.Game.start () in
  game_loop g
