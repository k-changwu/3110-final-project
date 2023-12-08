open OUnit2
open Sequence
open Player

let pp_string s = "\"" ^ s ^ "\""
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let p1 = Player.create 1
(* let p2 = Player.create 2 *)

let player_tests = [
  "create test" >:: (fun _ ->
    assert_equal 1 (get_id p1)
  )


]


let suite =
  "test suite for Sequence"
  >::: List.flatten [player_tests]

let () = run_test_tt_main suite