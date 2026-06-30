type card =
  | Suspect of string
  | Weapon of string
  | Location of string

type t = card

let card_to_string = function
  | Suspect name -> "Suspect: " ^ name
  | Weapon name -> "Weapon: " ^ name
  | Location name -> "Location: " ^ name

let suspect_to_string = function
  | Suspect name -> name
  | _ -> failwith "Expected a Suspect card!"

let weapon_to_string weapon =
  match weapon with
  | Weapon name -> name
  | _ -> failwith "Expected a Weapon card!"

let location_to_string location =
  match location with
  | Location name -> name
  | _ -> failwith "Expected a Location card!"

let rec cards_list_to_string cards =
  match cards with
  | [] -> ""
  | [ h ] -> card_to_string h
  | h :: t -> card_to_string h ^ ", " ^ cards_list_to_string t

let get_card_names cards =
  List.map
    (function
      | Suspect name -> name
      | Weapon name -> name
      | Location name -> name)
    cards

let suspects =
  [
    Suspect "Mrs. Peacock";
    Suspect "Mrs. White";
    Suspect "Mr. Green";
    Suspect "Prof. Plum";
    Suspect "Col. Mustard";
    Suspect "Miss Scarlett";
  ]

let weapons =
  [
    Weapon "Candlestick";
    Weapon "Revolver";
    Weapon "Lead Pipe";
    Weapon "Wrench";
    Weapon "Dagger";
    Weapon "Rope";
  ]

let locations =
  [
    Location "Study";
    Location "Library";
    Location "Kitchen";
    Location "Hall";
    Location "Ballroom";
    Location "Billiard Room";
    Location "Dining Room";
    Location "Conservatory";
    Location "Lounge";
  ]

let all_cards =
  [
    Suspect "Mrs. Peacock";
    Suspect "Mrs. White";
    Suspect "Mr. Green";
    Suspect "Prof. Plum";
    Suspect "Col. Mustard";
    Suspect "Miss Scarlett";
    Weapon "Candlestick";
    Weapon "Revolver";
    Weapon "Lead Pipe";
    Weapon "Wrench";
    Weapon "Dagger";
    Weapon "Rope";
    Location "Study";
    Location "Library";
    Location "Kitchen";
    Location "Hall";
    Location "Ballroom";
    Location "Billiard Room";
    Location "Dining Room";
    Location "Conservatory";
    Location "Lounge";
  ]

let string_to_card card_str =
  match
    List.find_opt
      (fun card ->
        match card with
        | Suspect name -> name = card_str
        | _ -> false)
      suspects
  with
  | Some card -> card
  | None -> (
      match
        List.find_opt
          (fun card ->
            match card with
            | Weapon name -> name = card_str
            | _ -> false)
          weapons
      with
      | Some card -> card
      | None -> (
          match
            List.find_opt
              (fun card ->
                match card with
                | Location name -> name = card_str
                | _ -> false)
              locations
          with
          | Some card -> card
          | None -> failwith ("Invalid card: " ^ card_str)))

let display_name_of_card = function
  | Suspect "Mrs. Peacock" -> "Mrs. Peacock  🔵"
  | Suspect "Mrs. White" -> "Mrs. White    ⚪"
  | Suspect "Mr. Green" -> "Mr. Green     🟢"
  | Suspect "Prof. Plum" -> "Prof. Plum    🟣"
  | Suspect "Col. Mustard" -> "Col. Mustard  🟡"
  | Suspect "Miss Scarlett" -> "Miss Scarlett 🔴"
  | Weapon "Candlestick" -> "Candlestick   🕯️ "
  | Weapon "Revolver" -> "Revolver      🔫"
  | Weapon "Lead Pipe" -> "Lead Pipe     🪈"
  | Weapon "Wrench" -> "Wrench        🔧"
  | Weapon "Dagger" -> "Dagger        🗡️ "
  | Weapon "Rope" -> "Rope          🪢"
  | Location "Study" -> "Study         🔑"
  | Location "Library" -> "Library       🔑"
  | Location "Kitchen" -> "Kitchen       🔑"
  | Location "Hall" -> "Hall          🔑"
  | Location "Ballroom" -> "Ballroom      🔑"
  | Location "Billiard Room" -> "Billiard Room 🔑"
  | Location "Dining Room" -> "Dining Room   🔑"
  | Location "Conservatory" -> "Conservatory  🔑"
  | Location "Lounge" -> "Lounge        🔑"
  | _ -> failwith "Unknown card"
