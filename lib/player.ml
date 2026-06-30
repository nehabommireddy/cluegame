open Cards
open Clue_sheet

(* Type to represent a player *)
type t = {
  name : string;
  hand : Cards.t list;
  clue_sheet : Clue_sheet.t;
  x : int;
  y : int;
}

let create_player name hand x y =
  let clue_sheet = Clue_sheet.initial_sheet hand in
  { name; hand; clue_sheet; x; y }

let get_name player = player.name
let get_hand player = player.hand
let get_clue_sheet player = player.clue_sheet
let get_x player = player.x
let get_y player = player.y
let set_position new_x new_y player = { player with x = new_x; y = new_y }

let reveal_card_if_has player cards =
  let rec find_card cards_to_check =
    match cards_to_check with
    | [] -> None
    | card :: rest ->
        if List.exists (fun c -> c = card) player.hand then Some card
        else find_card rest
  in
  find_card cards

let update_clue_sheet player card =
  let new_clue_sheet = Clue_sheet.update_sheet player.clue_sheet card in
  { player with clue_sheet = new_clue_sheet }

let bold str = "\027[1m" ^ str ^ "\027[0m"

let center_text text width =
  let len = String.length text in
  if len >= width then
    String.sub text 0 (width - 1) ^ "…" (* truncate and add ellipsis *)
  else
    let total_padding = width - len in
    let left_padding = total_padding / 2 in
    let right_padding = total_padding - left_padding in
    String.make left_padding ' ' ^ text ^ String.make right_padding ' '

let display_player_info player =
  let hand = List.map Cards.card_to_string (get_hand player) in
  let sheet = get_clue_sheet player in

  let extract_cards_with_status all_cards known_list =
    List.fold_right2
      (fun card known (found, not_found) ->
        if known then (Cards.display_name_of_card card :: found, not_found)
        else (found, Cards.card_to_string card :: not_found))
      all_cards known_list ([], [])
  in

  let suspects_found, suspects_not_found =
    extract_cards_with_status Cards.suspects
      (Clue_sheet.suspects_of_sheet sheet)
  in
  let weapons_found, weapons_not_found =
    extract_cards_with_status Cards.weapons (Clue_sheet.weapons_of_sheet sheet)
  in
  let locations_found, locations_not_found =
    extract_cards_with_status Cards.locations
      (Clue_sheet.locations_of_sheet sheet)
  in

  let found = suspects_found @ weapons_found @ locations_found in
  let remaining =
    suspects_not_found @ weapons_not_found @ locations_not_found
  in

  let remaining_filtered =
    List.filter (fun card -> not (List.mem card hand)) remaining
  in

  let max_len =
    max (List.length hand)
      (max (List.length remaining_filtered) (List.length found))
  in

  let col_width = 25 in
  let pad s =
    if String.length s >= col_width then String.sub s 0 (col_width - 1) ^ "…"
    else Printf.sprintf "%-*s" col_width s
  in

  print_endline
    (bold
       (Printf.sprintf "\n%s | %s | %s"
          (center_text "YOUR CARDS" col_width)
          (center_text "CARDS FOUND" col_width)
          (center_text "REMAINING OPTIONS" col_width)));

  for i = 0 to max_len - 1 do
    let left = if i < List.length hand then List.nth hand i else "" in
    let middle =
      if i < List.length remaining_filtered then List.nth remaining_filtered i
      else ""
    in
    let right = if i < List.length found then List.nth found i else "" in
    Printf.printf "%s | %s | %s\n" (pad left) (pad middle) (pad right)
  done

let can_disprove_guess player cards =
  List.exists (fun card -> List.exists (fun c -> c = card) player.hand) cards
