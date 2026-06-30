open Cards

(* Type to represent a clue sheet *)
type t = {
  suspect_list : bool list;
  weapon_list : bool list;
  location_list : bool list;
}

(* Helper function to create a list of booleans representing the initial state
   of a category *)
let create_bool_list category_list =
  let rec aux category_list bool_list =
    match category_list with
    | [] -> bool_list
    | _ :: t -> aux t (true :: bool_list)
  in
  aux category_list []
[@@coverage off]

(* Helper function to classify cards *)
let classify_card c =
  let name = card_to_string c in
  if String.sub name 0 8 = "Suspect:" then "Suspect"
  else if String.sub name 0 7 = "Weapon:" then "Weapon"
  else "Location"
[@@coverage off]

let initial_sheet player_cards =
  let all_suspects = suspects in
  let all_weapons = weapons in
  let all_locations = locations in

  let suspect_list =
    List.map
      (fun s ->
        not
          (List.exists
             (fun c ->
               classify_card c = "Suspect"
               && card_to_string c = card_to_string s)
             player_cards))
      all_suspects
  in
  let weapon_list =
    List.map
      (fun w ->
        not
          (List.exists
             (fun c ->
               classify_card c = "Weapon" && card_to_string c = card_to_string w)
             player_cards))
      all_weapons
  in
  let location_list =
    List.map
      (fun l ->
        not
          (List.exists
             (fun c ->
               classify_card c = "Location"
               && card_to_string c = card_to_string l)
             player_cards))
      all_locations
  in

  { suspect_list; weapon_list; location_list }

let display_bool_list bool_list cards =
  List.iter2
    (fun b c ->
      if b then print_endline (card_to_string c ^ " - Not crossed off")
      else print_endline (card_to_string c ^ " - Crossed off"))
    bool_list cards

let display_clue_sheet sheet =
  print_endline "Suspects:";
  display_bool_list sheet.suspect_list suspects;

  print_endline "\nWeapons:";
  display_bool_list sheet.weapon_list weapons;

  print_endline "\nLocations:";
  display_bool_list sheet.location_list locations

let update_sheet sheet card =
  match classify_card card with
  | "Suspect" ->
      let suspect_names = List.map card_to_string suspects in
      let index =
        List.find_index (fun n -> n = card_to_string card) suspect_names
      in
      let new_suspect_list =
        match index with
        | Some i ->
            List.mapi (fun j b -> if j = i then false else b) sheet.suspect_list
        | None -> sheet.suspect_list
      in
      { sheet with suspect_list = new_suspect_list }
  | "Weapon" ->
      let weapon_names = List.map card_to_string weapons in
      let index =
        List.find_index (fun n -> n = card_to_string card) weapon_names
      in
      let new_weapon_list =
        match index with
        | Some i ->
            List.mapi (fun j b -> if j = i then false else b) sheet.weapon_list
        | None -> sheet.weapon_list
      in
      { sheet with weapon_list = new_weapon_list }
  | "Location" ->
      let location_names = List.map card_to_string locations in
      let index =
        List.find_index (fun n -> n = card_to_string card) location_names
      in
      let new_location_list =
        match index with
        | Some i ->
            List.mapi
              (fun j b -> if j = i then false else b)
              sheet.location_list
        | None -> sheet.location_list
      in
      { sheet with location_list = new_location_list }
  | _ -> sheet

let suspects_of_sheet (s : t) = s.suspect_list
let weapons_of_sheet (s : t) = s.weapon_list
let locations_of_sheet (s : t) = s.location_list
