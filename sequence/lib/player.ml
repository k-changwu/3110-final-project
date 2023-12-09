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
let create i cards = { id = i; hand = cards }

(* get_hand function retrieves the list of cards (hand) from a player. *)
let get_hand player = player.hand

(* hand_to_string converts a hand of cards to its string representation *)
let hand_to_string hand =
  let card_strings = List.map Deck.to_string hand in
  "[" ^ String.concat ", " card_strings ^ "]"

(* has_card function checks if the card a player wants to play exists in their
   deck *)
let has_card player target_card =
  List.exists (fun card -> card = target_card) player.hand

let rec filter c = function
  | [] -> []
  | h :: t -> if h = c then t else h :: filter c t

(* play_card function simulates a player playing a card from their hand. It
   takes a player and the card to be played and returns a new player with the
   same ID and a hand that no longer contains the played card. *)
let play_card player card =
  if has_card player card then
    Some { player with hand = filter card player.hand }
  else None

(* get_id function returns the player's id *)
let get_id player = player.id

(* add_card adds a card to a player's hand *)
let add_card player card = { player with hand = card :: player.hand }
