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

(* Create an unshuffled full deck of cards (2 regular decks of cards)
   considering special Jacks. *)
let full_deck () =
  let suits = [ Hearts; Diamonds; Clubs; Spades ] in
  (* 4 special_jacks per 52 card deck *)
  let special_jacks =
    [
      { suit = OneEyed; rank = Jack };
      { suit = OneEyed; rank = Jack };
      { suit = TwoEyed; rank = Jack };
      { suit = TwoEyed; rank = Jack };
    ]
  in
  let ranks =
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Queen; King; Ace ]
  in
  let regular_cards =
    List.flatten
      (List.map (fun suit -> List.map (fun rank -> { suit; rank }) ranks) suits)
  in
  let full_single_deck = special_jacks @ regular_cards in
  full_single_deck @ full_single_deck

let shuffle deck =
  let nd = List.map (fun c -> (Random.bits (), c)) deck in
  let sond = List.sort compare nd in
  List.map snd sond

let rank_of_string s =
  match s with
  | "2" -> Two
  | "3" -> Three
  | "4" -> Four
  | "5" -> Five
  | "6" -> Six
  | "7" -> Seven
  | "8" -> Eight
  | "9" -> Nine
  | "10" -> Ten
  | "J" -> Jack
  | "Q" -> Queen
  | "K" -> King
  | "A" -> Ace
  | _ -> failwith "Invalid rank"

let suit_of_char c =
  match c with
  | 'H' -> Hearts
  | 'D' -> Diamonds
  | 'C' -> Clubs
  | 'S' -> Spades
  | _ -> failwith "Invalid suit"

let card_of_string str =
  let len = String.length str in
  if len = 3 || len = 2 then
    let rank_str =
      if len = 3 then String.sub str 0 2 else String.make 1 (String.get str 0)
    in
    let suit_char = String.get str (len - 1) in
    try
      let rank = rank_of_string rank_str in
      let suit = suit_of_char suit_char in
      { suit; rank }
    with Failure msg -> failwith ("card_of_string: " ^ msg)
  else failwith "Invalid card format"
