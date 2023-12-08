open Board
open Deck

type t = {
  id : int;
  hand : Deck.t list;
}

type color =
  | Red
  | Blue

type currTurn =
  | True
  | False

(* create takes an integer id and returns a new player with the given ID and an
   empty hand of cards. This function is used to initialize a new player when
   the game starts. *)
let create i = { id = i; hand = [] }

(* deal_cards is used to give a list of cards to a player. It takes a player and
   a list of cards, and it returns a new player that has the same ID but with
   the hand replaced by the new list of cards. *)
let deal_cards player cards = { player with hand = cards }

(* get_hand function retrieves the list of cards (hand) from a player. *)
let get_hand player = player.hand

(* has_card function checks if the card a player wants to play exists in their
   deck *)
let has_card player target_card =
  List.exists (fun card -> card = target_card) player.hand

(* play_card function simulates a player playing a card from their hand. It
   takes a player and the card to be played and returns a new player with the
   same ID and a hand that no longer contains the played card. *)
let play_card player card =
  if has_card player card then
    Some { player with hand = List.filter (fun c -> c <> card) player.hand }
  else None

(* get_id function returns the player's id *)
let get_id player = player.id

(* add_card adds a card to a player's hand *)
let add_card player card = { player with hand = card :: player.hand }
