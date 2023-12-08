type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades
  | OneEyed
  | TwoEyed

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type t = {
  suit : suit;
  rank : rank;
}

let to_string card =
  let suit =
    match card.suit with
    | Hearts -> "H"
    | Diamonds -> "D"
    | Clubs -> "C"
    | Spades -> "S"
    | OneEyed -> "One-Eyed"
    | TwoEyed -> "Two-Eyed"
  in
  let rank =
    match card.rank with
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | Jack -> "J"
    | Queen -> "Q"
    | King -> "K"
    | Ace -> "A"
  in
  rank ^ " " ^ suit

(* Create an unshuffled full deck of cards, considering special Jacks. *)
let full_deck () =
  let suits = [ Hearts; Diamonds; Clubs; Spades ] in
  let special_jacks =
    [ { suit = OneEyed; rank = Jack }; { suit = TwoEyed; rank = Jack } ]
  in
  let ranks =
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Queen; King; Ace ]
  in
  let regular_cards =
    List.flatten
      (List.map (fun suit -> List.map (fun rank -> { suit; rank }) ranks) suits)
  in
  special_jacks @ regular_cards (* Append special Jacks to the deck *)

let shuffle deck =
  let nd = List.map (fun c -> (Random.bits (), c)) deck in
  let sond = List.sort compare nd in
  List.map snd sond

let rank_of_string str =
  match str with
  | "Two" -> Two
  | "Three" -> Three
  | "Four" -> Four
  | "Five" -> Five
  | "Six" -> Six
  | "Seven" -> Seven
  | "Eight" -> Eight
  | "Nine" -> Nine
  | "Ten" -> Ten
  | "Jack" -> Jack
  | "Queen" -> Queen
  | "King" -> King
  | "Ace" -> Ace
  | _ -> failwith "Invalid rank"

let suit_of_string str =
  match str with
  | "Hearts" -> Hearts
  | "Diamonds" -> Diamonds
  | "Clubs" -> Clubs
  | "Spades" -> Spades
  | _ -> failwith "Invalid suit"

let card_of_string str =
  try
    let parts = String.split_on_char ' ' str in
    match parts with
    | [ rank_str; suit_str ] ->
        let rank = rank_of_string rank_str in
        let suit = suit_of_string suit_str in
        { suit; rank }
    | _ -> failwith "Invalid card format"
  with Failure msg -> failwith ("card_of_string: " ^ msg)
