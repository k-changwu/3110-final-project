open OUnit2
open Sequence
open Player
open Deck
open Game
open Board

(* Test Plan Tested Player, Deck, and Board module functions by OUnit test suite
   here Our test cases were developed with both the black box and glass box
   approach, some methods were tested by testing every input possible (eg.
   card_of_string) while others were created using black box testing (eg.
   place_card) Game module was tested manually by play This testing approach
   demonstrates the correctness of the system because we created test cases for
   all the functions that game calls/builds off of. Since these fundamental
   functions passed, we know that our game functions work. *)

let p1 =
  Player.create 1
    [ { suit = Clubs; rank = Three }; { suit = Spades; rank = Five } ]

let p2 = Player.create 2 []

let player_tests =
  [
    ( "create  non empty hand" >:: fun _ ->
      assert_equal 1 (get_id p1);
      assert_equal
        [ { suit = Clubs; rank = Three }; { suit = Spades; rank = Five } ]
        (get_hand p1) );
    ( "create empty hand" >:: fun _ ->
      assert_equal 2 (get_id p2);
      assert_equal [] (get_hand p2) );
    ( "get_hand test" >:: fun _ ->
      let cards_to_deal =
        [ { suit = Clubs; rank = Three }; { suit = Spades; rank = Five } ]
      in
      let player = Player.create 3 cards_to_deal in
      let new_hand = Player.get_hand player in
      assert_equal cards_to_deal new_hand );
    ( "get_hand test" >:: fun _ ->
      let cards_to_deal =
        [ { suit = Hearts; rank = Five }; { suit = Diamonds; rank = Ten }; { suit = Clubs; rank = Queen } ]
      in
      let player = Player.create 4 cards_to_deal in
      let new_hand = Player.get_hand player in
      assert_equal cards_to_deal new_hand );
    ( "get_hand test" >:: fun _ ->
      let cards_to_deal =
        [ { suit = Hearts; rank = King}; { suit = Diamonds; rank = Ace }; { suit = Clubs; rank = Queen } ]
      in
      let player = Player.create 8 cards_to_deal in
      let new_hand = Player.get_hand player in
      assert_equal cards_to_deal new_hand );
    ( "has_card test card exists" >:: fun _ ->
      let initial_hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      let player = Player.create 1 initial_hand in
      let target_card = { suit = Diamonds; rank = Jack } in
      assert_equal true (Player.has_card player target_card) );
    ( "has_card test card does not exist" >:: fun _ ->
      let initial_hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      let player = Player.create 1 initial_hand in
      let target_card = { suit = Spades; rank = Seven } in
      assert_equal false (Player.has_card player target_card) );
    ( "play_card test valid card" >:: fun _ ->
      let initial_hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      let player = Player.create 1 initial_hand in
      let card_to_play = { suit = Hearts; rank = Two } in
      match Player.play_card player card_to_play with
      | Some player_after ->
          let new_hand = Player.get_hand player_after in
          assert_equal [ { suit = Diamonds; rank = Jack } ] new_hand
      | None -> assert_failure "Failed to play the card" );
    ( "play_card test valid card" >:: fun _ ->
      let initial_hand =
        [ { suit = Clubs; rank = King }; { suit = Diamonds; rank = Queen }; { suit = Hearts; rank = Two } ]
      in
      let player = Player.create 5 initial_hand in
      let card_to_play = { suit = Diamonds; rank = Queen } in
      match Player.play_card player card_to_play with
      | Some player_after ->
          let new_hand = Player.get_hand player_after in
          assert_equal [ { suit = Clubs; rank = King }; { suit = Hearts; rank = Two } ] new_hand
      | None -> assert_failure "Failed to play the card" );
    ( "play_card test invalid card" >:: fun _ ->
      let initial_hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      let player = Player.create 1 initial_hand in
      let card_to_play = { suit = Clubs; rank = Seven } in
      match Player.play_card player card_to_play with
      | None -> ()
      | Some _ -> assert_failure "Expected None for an invalid card" );
    ( "hand_to_string empty hand" >:: fun _ ->
      let empty_hand = [] in
      assert_equal "[]" (Player.hand_to_string empty_hand) );
    ( "hand_to_string non-empty hand" >:: fun _ ->
      let hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      assert_equal "[2 H, J D]" (Player.hand_to_string hand) );
    ( "hand_to_string non-empty hand" >:: fun _ ->
      let hand =
        [ { suit = Clubs; rank = King }; { suit = Diamonds; rank = Queen }; { suit = Hearts; rank = Two } ]
      in
      assert_equal "[K C, Q D, 2 H]" (Player.hand_to_string hand) );
   ( "hand_to_string non-empty hand" >:: fun _ ->
      let hand =
        [ { suit = Clubs; rank = King }; { suit = Diamonds; rank = Queen }; { suit = Hearts; rank = Two }; { suit = Clubs; rank = Seven } ]
      in
      assert_equal "[K C, Q D, 2 H, 7 C]" (Player.hand_to_string hand) );
  ]

let deck_tests =
  [
    ( "to_string tests" >:: fun _ ->
      assert_equal "A C" (to_string { suit = Clubs; rank = Ace });
      assert_equal "K D" (to_string { suit = Diamonds; rank = King });
      assert_equal "2 H" (to_string { suit = Hearts; rank = Two });
      assert_equal "10 D" (to_string { suit = Diamonds; rank = Ten });
      assert_equal "J S" (to_string { suit = Spades; rank = Jack }) );
    ( "card_of_string tests" >:: fun _ ->
      assert_equal "A C" (to_string (card_of_string "AC"));
      assert_equal "K D" (to_string (card_of_string "KD"));
      assert_equal "2 H" (to_string (card_of_string "2H"));
      assert_equal "10 D" (to_string (card_of_string "10D"));
      assert_equal "J S" (to_string (card_of_string "JS")) );
    ( "card_of_string tests" >:: fun _ ->
      assert_equal "A C" (to_string (card_of_string "A C"));
      assert_equal "K D" (to_string (card_of_string "K D"));
      assert_equal "2 H" (to_string (card_of_string "2 H"));
      assert_equal "10 D" (to_string (card_of_string "10 D"));
      assert_equal "J S" (to_string (card_of_string "J S")) );
    ( "full_deck tests" >:: fun _ ->
      let deck = Deck.full_deck () in
      let expected_size = 52 * 2 in
      assert_equal expected_size (List.length deck));
     ( "full_deck tests" >:: fun _ ->
      let deck = Deck.full_deck () in
      let count_jacks =
        List.filter
          (fun card ->
            card.rank = Jack && (card.suit = OneEyed || card.suit = TwoEyed))
          deck
        |> List.length
      in
      assert_equal 8 count_jacks );
    ( "shuffle test" >:: fun _ ->
      let sorted_deck = Deck.full_deck () in
      let shuffled_deck = Deck.shuffle sorted_deck in
      assert_bool "Shuffle should change the order of the deck"
        (sorted_deck <> shuffled_deck) );
    ( "rank_of_string test" >:: fun _ ->
      assert_equal Two (rank_of_string "2");
      assert_equal Three (rank_of_string "3");
      assert_equal Four (rank_of_string "4");
      assert_equal Five (rank_of_string "5");
      assert_equal Six (rank_of_string "6");
      assert_equal Seven (rank_of_string "7");
      assert_equal Eight (rank_of_string "8");
      assert_equal Nine (rank_of_string "9");
      assert_equal Ten (rank_of_string "10");
      assert_equal Jack (rank_of_string "J");
      assert_equal Jack (rank_of_string "j");
      assert_equal Queen (rank_of_string "Q");
      assert_equal Queen (rank_of_string "q");
      assert_equal King (rank_of_string "K");
      assert_equal King (rank_of_string "k");
      assert_equal Ace (rank_of_string "A");
      assert_equal Ace (rank_of_string "a");
      assert_raises (Failure "Invalid rank") (fun () -> rank_of_string "G") );
    ( "suit_of_char test" >:: fun _ ->
      assert_equal Hearts (suit_of_char 'H');
      assert_equal Hearts (suit_of_char 'h');
      assert_equal Diamonds (suit_of_char 'D');
      assert_equal Diamonds (suit_of_char 'd');
      assert_equal Clubs (suit_of_char 'C');
      assert_equal Clubs (suit_of_char 'c');
      assert_equal Spades (suit_of_char 'S');
      assert_equal Spades (suit_of_char 's');
      assert_raises (Failure "Invalid suit") (fun () -> suit_of_char 'a') );
  ]

let board_tests =
  [
    ( "square_to_string free space" >:: fun _ ->
      let free = { row = 1; col = 2; chip = None; card = Free_space; id = 3 } in
      assert_equal "\027[32m Free" (Board.square_to_string free) );
    ( "square_to_string regular card" >:: fun _ ->
      let reg =
        {
          row = 2;
          col = 3;
          chip = None;
          card = Reg_Card { suit = Hearts; rank = Two };
          id = 5;
        }
      in
      assert_equal "\027[39m 2 H5" (Board.square_to_string reg) );
    ( "place_chip red chip" >:: fun _ ->
      let board = Board.init in
      let card_to_place = Reg_Card { suit = Hearts; rank = Two } in
      let id_to_place = 1 in
      place_chip Red card_to_place id_to_place board;
      let chip_in_square = check_space card_to_place id_to_place board in
      assert_equal Red chip_in_square );
    ( "place_chip blue chip" >:: fun _ ->
      let board = Board.init in
      let card_to_place = Reg_Card { suit = Spades; rank = King } in
      let id_to_place = 1 in
      place_chip Blue card_to_place id_to_place board;
      let chip_in_square = check_space card_to_place id_to_place board in
      assert_equal Blue chip_in_square );
    ( "place_chip free chip" >:: fun _ ->
      let board = Board.init in
      let card_to_place = Reg_Card { suit = Diamonds; rank = Queen } in
      let id_to_place = 1 in
      place_chip Free card_to_place id_to_place board;
      let chip_in_square = check_space card_to_place id_to_place board in
      assert_equal Free chip_in_square );
    ( "remove_chip " >:: fun _ ->
      let board = Board.init in
      let card_to_place = Reg_Card { suit = Diamonds; rank = Queen } in
      let id_to_place = 1 in
      place_chip Free card_to_place id_to_place board;
      remove_chip card_to_place id_to_place board;
      let chip_in_square = check_space card_to_place id_to_place board in
      assert_equal None chip_in_square );
    ( "remove_chip " >:: fun _ ->
      let board = Board.init in
      let card_to_place = Reg_Card { suit = Spades; rank = Six } in
      let id_to_place = 1 in
      place_chip Free card_to_place id_to_place board;
      remove_chip card_to_place id_to_place board;
      let chip_in_square = check_space card_to_place id_to_place board in
      assert_equal None chip_in_square );
    ( "remove_chip " >:: fun _ ->
      let board = Board.init in
      let card_to_place = Reg_Card { suit = Hearts; rank = Seven } in
      let id_to_place = 1 in
      place_chip Free card_to_place id_to_place board;
      remove_chip card_to_place id_to_place board;
      let chip_in_square = check_space card_to_place id_to_place board in
      assert_equal None chip_in_square );
    ( "remove_chip " >:: fun _ ->
      let board = Board.init in
      let card_to_place = Reg_Card { suit = Diamonds; rank = Ten } in
      let id_to_place = 1 in
      place_chip Free card_to_place id_to_place board;
      remove_chip card_to_place id_to_place board;
      let chip_in_square = check_space card_to_place id_to_place board in
      assert_equal None chip_in_square );
  ]

let board_tests_2 =
  [
    ( "is_vertical_win" >:: fun _ ->
      let board =
        [
          [
            {
              row = 0;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Two };
              id = 0;
            };
            {
              row = 0;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Three };
              id = 1;
            };
          ];
          [
            {
              row = 1;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Four };
              id = 5;
            };
            {
              row = 1;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Five };
              id = 6;
            };
          ];
          [
            {
              row = 2;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Six };
              id = 10;
            };
            {
              row = 2;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Seven };
              id = 11;
            };
          ];
          [
            {
              row = 3;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Eight };
              id = 15;
            };
            {
              row = 3;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Nine };
              id = 16;
            };
          ];
          [
            {
              row = 4;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Ten };
              id = 20;
            };
            {
              row = 4;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Jack };
              id = 21;
            };
          ];
        ]
      in
      assert_bool "vertical_win" (Board.is_win board) );
    ( "no win horizontal_board_with_free_space" >:: fun _ ->
      let board =
        [
          [
            { row = 0; col = 0; chip = Free; card = Free_space; id = 0 };
            {
              row = 0;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Ten };
              id = 1;
            };
          ];
        ]
      in
      assert_bool "no win" (not (Board.is_win board)) );
    ( "diagonal_win" >:: fun _ ->
      let board =
        [
          [
            {
              row = 0;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Two };
              id = 0;
            };
            {
              row = 0;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Three };
              id = 1;
            };
            {
              row = 0;
              col = 2;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Four };
              id = 2;
            };
            {
              row = 0;
              col = 3;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Five };
              id = 3;
            };
            {
              row = 0;
              col = 4;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Six };
              id = 4;
            };
          ];
          [
            {
              row = 1;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Seven };
              id = 5;
            };
            {
              row = 1;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Eight };
              id = 6;
            };
            {
              row = 1;
              col = 2;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Nine };
              id = 7;
            };
            {
              row = 1;
              col = 3;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Ten };
              id = 8;
            };
            {
              row = 1;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Jack };
              id = 9;
            };
          ];
          [
            {
              row = 2;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Queen };
              id = 10;
            };
            {
              row = 2;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = King };
              id = 11;
            };
            {
              row = 2;
              col = 2;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Ace };
              id = 12;
            };
            {
              row = 2;
              col = 3;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Two };
              id = 13;
            };
            {
              row = 2;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Three };
              id = 14;
            };
          ];
          [
            {
              row = 3;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Four };
              id = 15;
            };
            {
              row = 3;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Five };
              id = 16;
            };
            {
              row = 3;
              col = 2;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Six };
              id = 17;
            };
            {
              row = 3;
              col = 3;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Seven };
              id = 18;
            };
            {
              row = 3;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Eight };
              id = 19;
            };
          ];
          [
            {
              row = 4;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Nine };
              id = 20;
            };
            {
              row = 4;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Ten };
              id = 21;
            };
            {
              row = 4;
              col = 2;
              chip = Red;
              card = Reg_Card { suit = Spades; rank = Jack };
              id = 22;
            };
            {
              row = 4;
              col = 3;
              chip = None;
              card = Reg_Card { suit = Spades; rank = Queen };
              id = 23;
            };
            {
              row = 4;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Spades; rank = King };
              id = 24;
            };
          ];
        ]
      in
      assert_bool "win" (Board.is_win board) );
    ( "horizontal 4-in a row no win" >:: fun _ ->
      let board =
        [
          [
            {
              row = 0;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Hearts; rank = Two };
              id = 0;
            };
            {
              row = 0;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Hearts; rank = Three };
              id = 1;
            };
            {
              row = 0;
              col = 2;
              chip = Red;
              card = Reg_Card { suit = Hearts; rank = Four };
              id = 2;
            };
            {
              row = 0;
              col = 3;
              chip = Red;
              card = Reg_Card { suit = Hearts; rank = Five };
              id = 3;
            };
            { row = 0; col = 4; chip = None; card = Free_space; id = 4 };
          ];
        ]
      in
      assert_bool "horizontal_win" (not (Board.is_win board)) );
    ( "vertical_win" >:: fun _ ->
      let board =
        [
          [
            {
              row = 0;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Two };
              id = 0;
            };
            {
              row = 0;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Three };
              id = 1;
            };
          ];
          [
            {
              row = 1;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Four };
              id = 5;
            };
            {
              row = 1;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Five };
              id = 6;
            };
          ];
          [
            {
              row = 2;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Six };
              id = 10;
            };
            {
              row = 2;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Seven };
              id = 11;
            };
          ];
          [
            {
              row = 3;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Eight };
              id = 15;
            };
            {
              row = 3;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Nine };
              id = 16;
            };
          ];
          [
            {
              row = 4;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Ten };
              id = 20;
            };
            {
              row = 4;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Jack };
              id = 21;
            };
          ];
        ]
      in
      assert_bool "vertical_win" (Board.is_win board) );
    ( "no win with_free_space" >:: fun _ ->
      let board =
        [
          [
            {
              row = 0;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Two };
              id = 0;
            };
            { row = 0; col = 1; chip = None; card = Free_space; id = 1 };
          ];
          [
            {
              row = 1;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Four };
              id = 5;
            };
            {
              row = 1;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Five };
              id = 6;
            };
          ];
          [
            {
              row = 2;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Six };
              id = 10;
            };
            {
              row = 2;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Seven };
              id = 11;
            };
          ];
        ]
      in
      assert_bool "no win" (not (Board.is_win board)) );
    ( "vertical_win_with_free_space" >:: fun _ ->
      let board =
        [
          [
            {
              row = 0;
              col = 0;
              chip = Blue;
              card = Reg_Card { suit = Diamonds; rank = Two };
              id = 0;
            };
            {
              row = 0;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Three };
              id = 1;
            };
          ];
          [
            {
              row = 1;
              col = 0;
              chip = Blue;
              card = Reg_Card { suit = Diamonds; rank = Four };
              id = 5;
            };
            {
              row = 1;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Five };
              id = 6;
            };
          ];
          [
            {
              row = 2;
              col = 0;
              chip = Blue;
              card = Reg_Card { suit = Diamonds; rank = Six };
              id = 10;
            };
            {
              row = 2;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Seven };
              id = 11;
            };
          ];
          [
            {
              row = 3;
              col = 0;
              chip = Blue;
              card = Reg_Card { suit = Diamonds; rank = Eight };
              id = 15;
            };
            {
              row = 3;
              col = 1;
              chip = Blue;
              card = Reg_Card { suit = Diamonds; rank = Nine };
              id = 16;
            };
          ];
          [
            { row = 4; col = 0; chip = Blue; card = Free_space; id = 1 };
            { row = 4; col = 1; chip = None; card = Free_space; id = 21 };
          ];
        ]
      in
      assert_bool "vertical_win" (Board.is_win board) );
    ( "is_win test - diagonal win with strategic Blue placement" >:: fun _ ->
      let board =
        [
          [
            {
              row = 0;
              col = 0;
              chip = Red;
              card = Reg_Card { suit = Hearts; rank = Two };
              id = 0;
            };
            {
              row = 0;
              col = 1;
              chip = Blue;
              card = Reg_Card { suit = Hearts; rank = Three };
              id = 1;
            };
            {
              row = 0;
              col = 2;
              chip = None;
              card = Reg_Card { suit = Hearts; rank = Four };
              id = 2;
            };
            {
              row = 0;
              col = 3;
              chip = Blue;
              card = Reg_Card { suit = Hearts; rank = Five };
              id = 3;
            };
            {
              row = 0;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Hearts; rank = Six };
              id = 4;
            };
          ];
          [
            {
              row = 1;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Hearts; rank = Seven };
              id = 5;
            };
            {
              row = 1;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Hearts; rank = Eight };
              id = 6;
            };
            {
              row = 1;
              col = 2;
              chip = Blue;
              card = Reg_Card { suit = Hearts; rank = Nine };
              id = 7;
            };
            {
              row = 1;
              col = 3;
              chip = None;
              card = Reg_Card { suit = Hearts; rank = Ten };
              id = 8;
            };
            {
              row = 1;
              col = 4;
              chip = Blue;
              card = Reg_Card { suit = Hearts; rank = Jack };
              id = 9;
            };
          ];
          [
            {
              row = 2;
              col = 0;
              chip = Blue;
              card = Reg_Card { suit = Diamonds; rank = Two };
              id = 10;
            };
            {
              row = 2;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Three };
              id = 11;
            };
            {
              row = 2;
              col = 2;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Four };
              id = 12;
            };
            {
              row = 2;
              col = 3;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Five };
              id = 13;
            };
            {
              row = 2;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Six };
              id = 14;
            };
          ];
          [
            {
              row = 3;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Seven };
              id = 15;
            };
            {
              row = 3;
              col = 1;
              chip = Blue;
              card = Reg_Card { suit = Diamonds; rank = Eight };
              id = 16;
            };
            {
              row = 3;
              col = 2;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Nine };
              id = 17;
            };
            {
              row = 3;
              col = 3;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Ten };
              id = 18;
            };
            {
              row = 3;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Jack };
              id = 19;
            };
          ];
          [
            {
              row = 4;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Clubs; rank = Two };
              id = 20;
            };
            {
              row = 4;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Clubs; rank = Three };
              id = 21;
            };
            {
              row = 4;
              col = 2;
              chip = Blue;
              card = Reg_Card { suit = Clubs; rank = Four };
              id = 22;
            };
            {
              row = 4;
              col = 3;
              chip = None;
              card = Reg_Card { suit = Clubs; rank = Five };
              id = 23;
            };
            {
              row = 4;
              col = 4;
              chip = Red;
              card = Reg_Card { suit = Clubs; rank = Six };
              id = 24;
            };
          ];
        ]
      in
      assert_bool "win red" (Board.is_win board) );
  ]

let game_test =
  [
    ( "apply_jack_effect test - jack effects" >:: fun _ ->
      let board =
        [
          [
            {
              row = 0;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Hearts; rank = Two };
              id = 0;
            };
            {
              row = 0;
              col = 1;
              chip = None;
              card = Reg_Card { suit = Hearts; rank = Three };
              id = 1;
            };
            {
              row = 0;
              col = 2;
              chip = None;
              card = Reg_Card { suit = Hearts; rank = Four };
              id = 2;
            };
            {
              row = 0;
              col = 3;
              chip = Blue;
              card = Reg_Card { suit = Hearts; rank = Five };
              id = 3;
            };
            {
              row = 0;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Hearts; rank = Six };
              id = 4;
            };
          ];
          [
            {
              row = 1;
              col = 0;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Two };
              id = 5;
            };
            {
              row = 1;
              col = 1;
              chip = Red;
              card = Reg_Card { suit = Diamonds; rank = Three };
              id = 6;
            };
            {
              row = 1;
              col = 2;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Four };
              id = 7;
            };
            {
              row = 1;
              col = 3;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Five };
              id = 8;
            };
            {
              row = 1;
              col = 4;
              chip = None;
              card = Reg_Card { suit = Diamonds; rank = Six };
              id = 9;
            };
          ];
        ]
      in

      let two_eyed_jack = { Deck.suit = TwoEyed; rank = Jack } in
      apply_jack_effect board 1 two_eyed_jack;
      let one_eyed_jack = { Deck.suit = OneEyed; rank = Jack } in
      apply_jack_effect board 2 one_eyed_jack;

      assert_equal
        (Board.check_space
           (Reg_Card { Deck.suit = Diamonds; rank = Four })
           7 board)
        Board.Red;
      assert_equal
        (Board.check_space
           (Reg_Card { Deck.suit = Diamonds; rank = Three })
           6 board)
        Board.None );
  ]

let suite =
  "test suite for Sequence"
  >::: List.flatten [ player_tests; deck_tests; board_tests; board_tests_2 ]

let () = run_test_tt_main suite
