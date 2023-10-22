open Board 
module type Player = sig
    type hand = card list
    type color =  Red | Blue 
    type currTurn = True | False 

    (* player plays a card from hand and calls Board place_chip function *)
    val play_card : card -> unit 

end 