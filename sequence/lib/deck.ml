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

let regular_cards () =
  let suits = [ Hearts; Diamonds; Clubs; Spades ] in
  let ranks =
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Queen; King; Ace ]
  in
  List.flatten
    (List.map (fun suit -> List.map (fun rank -> { suit; rank }) ranks) suits)

(* Create an unshuffled full deck of cards (2 regular decks of cards)
   considering special Jacks. *)
let full_deck () =
  (* 4 special_jacks per 52 card deck *)
  let special_jacks =
    [
      { suit = OneEyed; rank = Jack };
      { suit = OneEyed; rank = Jack };
      { suit = TwoEyed; rank = Jack };
      { suit = TwoEyed; rank = Jack };
    ]
  in
  let regular_cards = regular_cards () in
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
  | "j" -> Jack
  | "Q" -> Queen
  | "q" -> Queen
  | "K" -> King
  | "k" -> King
  | "A" -> Ace
  | "a" -> Ace
  | _ -> failwith "Invalid rank"

let suit_of_char c =
  match c with
  | 'H' -> Hearts
  | 'h' -> Hearts
  | 'D' -> Diamonds
  | 'd' -> Diamonds
  | 'C' -> Clubs
  | 'c' -> Clubs
  | 'S' -> Spades
  | 's' -> Spades
  | _ -> failwith "Invalid suit"

let card_of_string str =
  if str = "J One-Eyed" || str = "JOneEyed" then { suit = OneEyed; rank = Jack }
  else if str = "J Two-Eyed" || str = "JTwoEyed" then
    { suit = TwoEyed; rank = Jack }
  else if String.contains str ' ' then
    let parts = String.split_on_char ' ' str in
    match parts with
    | [ rank_str; suit_str ] ->
        let rank = rank_of_string rank_str in
        let suit = suit_of_char (String.get suit_str 0) in
        { suit; rank }
    | _ -> failwith "Invalid card format"
  else
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
