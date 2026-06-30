open Graphics

let make_rooms =
  [
    "Study";
    "Library";
    "Kitchen";
    "Hall";
    "Ballroom";
    "Billiard Room";
    "Dining Room";
    "Conservatory";
    "Lounge";
  ]

let make_suspects =
  [
    "Mrs. Peacock";
    "Mrs. White";
    "Mr. Green";
    "Prof. Plum";
    "Col. Mustard";
    "Miss Scarlett";
  ]

let make_weapons =
  [ "Candlestick"; "Revolver"; "Lead Pipe"; "Wrench"; "Dagger"; "Rope" ]

let choose_solution =
  Random.self_init ();
  let index = Random.int (List.length Cards.suspects) in
  let chosen_suspect = List.nth Cards.suspects index in
  let index = Random.int (List.length Cards.weapons) in
  let chosen_weapon = List.nth Cards.weapons index in
  let index = Random.int (List.length Cards.locations) in
  let chosen_location = List.nth Cards.locations index in
  (chosen_suspect, chosen_weapon, chosen_location)

let solution_to_strings (suspect, weapon, location) =
  let chosen_suspect = Cards.suspect_to_string suspect in
  let chosen_weapon = Cards.weapon_to_string weapon in
  let chosen_location = Cards.location_to_string location in
  (chosen_suspect, chosen_weapon, chosen_location)

let rec remove_card lst card = List.filter (fun x -> x <> card) lst

let list_of_cards_without_solution
    (chosen_suspect, chosen_weapon, chosen_location) =
  remove_card Cards.suspects chosen_suspect
  @ remove_card Cards.weapons chosen_weapon
  @ remove_card Cards.locations chosen_location

let shuffle_list lst =
  (*Random.self_init ();*)
  let n = List.length lst in
  let array = Array.of_list lst in
  for i = 0 to n - 2 do
    let j = Random.int (n - i) + i in
    let temp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- temp
  done;
  Array.to_list array

let get_six_cards shuffled_list i j =
  let rec sublist index acc = function
    | [] -> List.rev acc
    | h :: t when index >= i && index <= j -> sublist (index + 1) (h :: acc) t
    | h :: t when index < i -> sublist (index + 1) acc t
    | _ -> List.rev acc
  in
  sublist 0 [] shuffled_list

let rec game_over guess (chosen_suspect, chosen_weapon, chosen_location) =
  guess = (chosen_suspect, chosen_weapon, chosen_location)

let roll_dice () = Random.int 6 + 1

let is_valid_move players x y =
  (* Board boundaries: 0 <= x < 25, 0 <= y < 25 *)
  if x < 0 || x >= 25 || y < 0 || y >= 25 then false
  else
    not
      (List.exists (fun p -> Player.get_x p = x && Player.get_y p = y) players)

type room = {
  name : string;
  x1 : int;
  y1 : int;
  x2 : int;
  y2 : int;
  move_outside_x : int;
  move_outside_y : int;
}

let rooms =
  [
    {
      name = "Kitchen";
      x1 = 0;
      y1 = 20;
      x2 = 4;
      y2 = 24;
      move_outside_x = 2;
      move_outside_y = 19;
    };
    {
      name = "Ballroom";
      x1 = 10;
      y1 = 20;
      x2 = 15;
      y2 = 24;
      move_outside_x = 12;
      move_outside_y = 19;
    };
    {
      name = "Conservatory";
      x1 = 20;
      y1 = 20;
      x2 = 24;
      y2 = 24;
      move_outside_x = 22;
      move_outside_y = 19;
    };
    {
      name = "Dining Room";
      x1 = 0;
      y1 = 9;
      x2 = 5;
      y2 = 14;
      move_outside_x = 6;
      move_outside_y = 12;
    };
    {
      name = "Billiard Room";
      x1 = 20;
      y1 = 14;
      x2 = 24;
      y2 = 17;
      move_outside_x = 19;
      move_outside_y = 16;
    };
    {
      name = "Library";
      x1 = 20;
      y1 = 8;
      x2 = 24;
      y2 = 11;
      move_outside_x = 19;
      move_outside_y = 9;
    };
    {
      name = "Lounge";
      x1 = 0;
      y1 = 0;
      x2 = 4;
      y2 = 4;
      move_outside_x = 5;
      move_outside_y = 2;
    };
    {
      name = "Hall";
      x1 = 10;
      y1 = 0;
      x2 = 15;
      y2 = 5;
      move_outside_x = 9;
      move_outside_y = 3;
    };
    {
      name = "Study";
      x1 = 20;
      y1 = 0;
      x2 = 24;
      y2 = 4;
      move_outside_x = 19;
      move_outside_y = 2;
    };
  ]

let room_at_position x y =
  List.find_opt
    (fun r -> x >= r.x1 && x <= r.x2 && y >= r.y1 && y <= r.y2)
    rooms

let room_to_string room_in = room_in.name

let move_outside_room (player : Player.t) =
  match room_at_position (Player.get_x player) (Player.get_y player) with
  | Some room ->
      Player.set_position room.move_outside_x room.move_outside_y player
  | None -> player

let ansi_color_of_room name =
  match name with
  | "Kitchen" -> "31" (* Red *)
  | "Ballroom" -> "32" (* Green *)
  | "Conservatory" -> "34" (* Blue *)
  | "Dining Room" -> "36" (* Cyan *)
  | "Lounge" -> "93" (* Yellow *)
  | "Hall" -> "35" (* Magenta *)
  | "Study" -> "95" (* Magenta *)
  | "Library" -> "33" (* Yellow *)
  | "Billiard Room" -> "37" (* White *)
  | _ -> "0" (* Default *)
[@@coverage off]

let ansi_color_of_room_color name =
  match name with
  | "Kitchen" -> rgb 210 100 100
  | "Ballroom" -> rgb 140 170 90
  | "Conservatory" -> rgb 100 120 200
  | "Dining Room" -> rgb 140 200 190
  | "Lounge" -> rgb 218 165 32
  | "Hall" -> rgb 170 130 200
  | "Study" -> rgb 220 150 160
  | "Library" -> rgb 180 130 90
  | "Billiard Room" -> rgb 128 128 128
  | _ -> black
[@@coverage off]

type portal = {
  start : int * int;
  end_point : int * int;
}

let portals =
  [
    { start = (3, 3); end_point = (21, 21) };
    (*Lounge to Conservatory*)
    { start = (21, 21); end_point = (3, 3) };
    (*Conservatory to Lounge*)
    { start = (3, 21); end_point = (21, 3) };
    (*Study to Kitchen*)
    { start = (21, 3); end_point = (3, 21) };
    (*Kitchen to Study*)
  ]

let check_portal (x, y) =
  match List.find_opt (fun p -> p.start = (x, y)) portals with
  | Some p -> Some p.end_point
  | None -> None

let move_player_and_check_portal new_x new_y player =
  let player = Player.set_position new_x new_y player in
  match check_portal (new_x, new_y) with
  | Some (px, py) -> Player.set_position px py player
  | None -> player
