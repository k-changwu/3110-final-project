open OUnit2
open Sequence
open Player
open Deck

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
      let initial_hand = Player.get_hand p1 in
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
  ]

let deck_tests =
  [
    ( "to_string_tests" >:: fun _ ->
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
    "";
  ]

let suite =
  "test suite for Sequence" >::: List.flatten [ player_tests; deck_tests ]

let () = run_test_tt_main suite
