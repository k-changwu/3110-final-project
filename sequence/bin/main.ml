open Sequence.Game
open Sequence.Player
open Sequence.Board
open Sequence.Deck

let () =
  print_endline "\nWelcome to Sequence!\n";
  print_endline
    "Sequence is a two player game of connect 5. Looks up the rules online to \
     find out more\n";
  let g = Sequence.Game.start () in
  print_endline
    ("\n It's player "
    ^ string_of_int (Sequence.Game.current_player g)
    ^ "'s turn!")
