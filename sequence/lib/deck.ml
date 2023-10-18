type suit = Hearts | Diamonds | Clubs | Spades | OneEyed | TwoEyed
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
type t = { suit : suit; rank : rank }

let to_string card =
  let suit = match card.suit with
    | Hearts -> "Hearts"
    | Diamonds -> "Diamonds"
    | Clubs -> "Clubs"
    | Spades -> "Spades"
    | OneEyed -> "One-Eyed"
    | TwoEyed -> "Two-Eyed"
  in
  let rank = match card.rank with
    | Two -> "2" | Three -> "3" | Four -> "4" | Five -> "5"
    | Six -> "6" | Seven -> "7" | Eight -> "8" | Nine -> "9"
    | Ten -> "10" | Jack -> "Jack" | Queen -> "Queen"
    | King -> "King" | Ace -> "Ace"
  in
  rank ^ " of " ^ suit

