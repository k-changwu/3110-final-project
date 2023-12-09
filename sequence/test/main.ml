open OUnit2
open Sequence
open Player
open Deck
open Board

(* let pp_string s = "\"" ^ s ^ "\""

   let pp_list pp_elt lst = let pp_elts lst = let rec loop n acc = function | []
   -> acc | [ h ] -> acc ^ pp_elt h | h1 :: (h2 :: t as t') -> if n = 100 then
   acc ^ "..." (* stop printing long list *) else loop (n + 1) (acc ^ pp_elt h1
   ^ "; ") t' in loop 0 "" lst in "[" ^ pp_elts lst ^ "]" *)

let p1 = Player.create 1
let p2 = Player.create 2

let player_tests =
  [
    ("create test" >:: fun _ -> assert_equal 1 (get_id p1));
    ( "deal_card test empty initial hand" >:: fun _ ->
      let cards_to_deal = Deck.full_deck () in
      let player_after = Player.deal_cards p1 cards_to_deal in
      let new_hand = Player.get_hand player_after in
      assert_equal cards_to_deal new_hand );
    ( "get_hand test" >:: fun _ ->
      let cards_to_deal =
        [ { suit = Clubs; rank = Three }; { suit = Spades; rank = Five } ]
      in
      let player_after = Player.deal_cards p2 cards_to_deal in
      let new_hand = Player.get_hand player_after in
      assert_equal cards_to_deal new_hand );
    ( "has_card test card exists" >:: fun _ ->
      let initial_hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      let initial_player = Player.create 1 in
      let player = Player.deal_cards initial_player initial_hand in
      let target_card = { suit = Diamonds; rank = Jack } in
      assert_equal true (Player.has_card player target_card) );
    ( "has_card test card does not exist" >:: fun _ ->
      let initial_hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      let initial_player = Player.create 1 in
      let player = Player.deal_cards initial_player initial_hand in
      let target_card = { suit = Spades; rank = Seven } in
      assert_equal false (Player.has_card player target_card) );
    ( "play_card test valid card" >:: fun _ ->
      let initial_hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      let initial_player = Player.create 1 in
      let player = Player.deal_cards initial_player initial_hand in
      let card_to_play = { suit = Hearts; rank = Two } in
      match Player.play_card player card_to_play with
      | Some player_after ->
          let new_hand = Player.get_hand player_after in
          assert_equal [ { suit = Diamonds; rank = Jack } ] new_hand
      | None -> assert_failure "Failed to play the card" );
    ( "play_card test invalid card" >:: fun _ ->
      let initial_hand =
        [ { suit = Hearts; rank = Two }; { suit = Diamonds; rank = Jack } ]
      in
      let initial_player = Player.create 1 in
      let player = Player.deal_cards initial_player initial_hand in
      let card_to_play = { suit = Clubs; rank = Seven } in
      match Player.play_card player card_to_play with
      | None ->
          (* This is the expected behavior for an invalid card *)
          ()
      | Some _ -> assert_failure "Expected None for an invalid card" );
    ( "hand_to_string empty hand" >:: fun _ -> 
      let empty_hand = [] in 
      assert_equal "[]" (Player.hand_to_string empty_hand));
    ( "hand_to_string non-empty hand" >:: fun _ -> 
      let hand = [ 
        { suit = Hearts; rank = Two }; 
        { suit = Diamonds; rank = Jack}
      ] in 
      assert_equal "[2 H, J D]" (Player.hand_to_string hand));
  ]

let deck_tests =
  [
    ( "to_string tests" >:: fun _ ->
      assert_equal "A C" (to_string { suit = Clubs; rank = Ace });
      assert_equal "K D" (to_string { suit = Diamonds; rank = King });
      assert_equal "2 H" (to_string { suit = Hearts; rank = Two });
      assert_equal "10 D" (to_string { suit = Diamonds; rank = Ten });
      assert_equal "J S" (to_string { suit = Spades; rank = Jack }));
    ( "card_of_string tests" >:: fun _ ->
      assert_equal "A C" (to_string (card_of_string "AC"));
      assert_equal "K D" (to_string (card_of_string "KD"));
      assert_equal "2 H" (to_string (card_of_string "2H"));
      assert_equal "10 D" (to_string (card_of_string "10D"));
      assert_equal "J S" (to_string (card_of_string "JS")));
    ( "full_deck tests" >:: fun _ -> 
        let deck = Deck.full_deck () in 
        let expected_size = 52 * 2 in 
        let count_jacks = List.filter (fun card -> 
          card.rank = Jack && (card.suit = OneEyed || card.suit = TwoEyed)) deck |> List.length in 
        assert_equal expected_size (List.length deck); 
        assert_equal 8 count_jacks);
    ( "shuffle test" >:: fun _ -> 
      let sorted_deck = Deck.full_deck () in 
      let shuffled_deck = Deck.shuffle sorted_deck in 
      assert_bool "Shuffle should change the order of the deck" 
      (sorted_deck <> shuffled_deck));
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
        assert_raises (Failure "Invalid rank") (fun () -> rank_of_string "G"));
    ( "suit_of_char test" >:: fun _ -> 
      assert_equal Hearts (suit_of_char 'H'); 
      assert_equal Hearts (suit_of_char 'h'); 
      assert_equal Diamonds (suit_of_char 'D'); 
      assert_equal Diamonds (suit_of_char 'd'); 
      assert_equal Clubs (suit_of_char 'C'); 
      assert_equal Clubs (suit_of_char 'c'); 
      assert_equal Spades (suit_of_char 'S'); 
      assert_equal Spades (suit_of_char 's'); 
      assert_raises (Failure "Invalid suit") (fun () -> suit_of_char 'a'));
  ]

let board_tests = [
  ( "square_to_string free space" >:: fun _ -> 
    let free = { row = 1; col = 2; chip = None; card = Free_space; id = 3 } in 
    assert_equal "\027[32m Free" (Board.square_to_string free));
  ( "square_to_string regular card" >:: fun _ -> 
    let reg = { row = 2; col = 3; chip = None; 
    card = Reg_Card { suit = Hearts; rank = Two}; id = 5} in 
    assert_equal "\027[39m 2 H5" (Board.square_to_string reg));
  ( "place_chip tests" >:: fun _ -> 
    let board = Board.init in
    let card_to_place = Reg_Card { suit = Hearts; rank = Two } in
    let id_to_place = 1 in
    place_chip Red card_to_place id_to_place board;
    let chip_in_square = check_space card_to_place id_to_place board in
    assert_equal Red chip_in_square
    );
  
  

]

let suite =
  "test suite for Sequence" >::: List.flatten [ player_tests; deck_tests; board_tests]

let () = run_test_tt_main suite
