open OUnit2
open Final_project.Cards
open Final_project.Clue_sheet
open Final_project.Player
open Final_project.Game

(* =========================== *)
(*    Clue_sheet.ml Tests      *)
(* =========================== *)

let test_initial_sheet _ =
  let sheet = initial_sheet [] in
  assert_bool "Initial sheet creation should not throw an exception" true;
  assert_bool "All suspects should be true"
    (List.for_all (fun x -> x) (suspects_of_sheet sheet));
  assert_bool "Suspect list length matches"
    (List.length (suspects_of_sheet sheet) = List.length suspects)

let test_update_sheet _ =
  let sheet = initial_sheet [] in
  let mrs_peacock = List.hd suspects in
  let updated_sheet = update_sheet sheet mrs_peacock in
  assert_bool "Updating the sheet should not throw an exception" true;
  assert_bool "Mrs. Peacock should be crossed off"
    (not (List.hd (suspects_of_sheet updated_sheet)));
  assert_bool "Weapons remain unchanged"
    (weapons_of_sheet updated_sheet = weapons_of_sheet sheet);
  assert_bool "Locations remain unchanged"
    (locations_of_sheet updated_sheet = locations_of_sheet sheet)

let test_initial_sheet_with_cards _ =
  let test_hand = [ List.hd suspects; List.hd weapons; List.hd locations ] in
  let sheet = initial_sheet test_hand in
  assert_bool "First suspect should be crossed off"
    (not (List.hd (suspects_of_sheet sheet)));
  assert_bool "First weapon should be crossed off"
    (not (List.hd (weapons_of_sheet sheet)));
  assert_bool "First location should be crossed off"
    (not (List.hd (locations_of_sheet sheet)))

let test_update_multiple_times _ =
  let sheet = initial_sheet [] in
  let suspect = List.hd suspects in
  let sheet' = update_sheet sheet suspect in
  let weapon = List.nth weapons 1 in
  let sheet'' = update_sheet sheet' weapon in
  let location = List.hd locations in
  let sheet''' = update_sheet sheet'' location in
  assert_bool "First suspect crossed"
    (not (List.hd (suspects_of_sheet sheet''')));

  assert_bool "Second weapon crossed"
    (not (List.nth (weapons_of_sheet sheet''') 1));

  assert_bool "First location crossed"
    (not (List.hd (locations_of_sheet sheet''')));
  assert_bool "Second suspect remains" (List.nth (suspects_of_sheet sheet''') 1);

  assert_bool "First weapon remains" (List.hd (weapons_of_sheet sheet'''));

  assert_bool "Second location remains"
    (List.nth (locations_of_sheet sheet''') 1)

let test_display_clue_sheet _ =
  let sheet = initial_sheet [] in
  display_clue_sheet sheet;
  assert_bool "Display function should not throw an exception" true

(* =========================== *)
(*    Player.ml Tests          *)
(* =========================== *)

let random_card cards =
  let index = Random.int (List.length cards) in
  List.nth cards index

let test_create_player_empty_hand _ =
  let player = create_player "Test Player" [] 0 0 in
  assert_equal [] (get_hand player)

let test_create_player_with_cards _ =
  let suspect = random_card suspects in
  let weapon = random_card weapons in
  let location = random_card locations in
  let cards = [ suspect; weapon; location ] in
  let player = create_player "Test Player" cards 0 0 in
  assert_equal cards (get_hand player)

let test_get_hand _ =
  let suspect = random_card suspects in
  let weapon = random_card weapons in
  let location = random_card locations in
  let cards = [ suspect; weapon; location ] in
  let player = create_player "Test Player" cards 0 0 in
  assert_equal cards (get_hand player)

let test_get_clue_sheet _ =
  let player = create_player "Test Player" [] 0 0 in
  let _ = get_clue_sheet player in
  assert_bool "Getting clue sheet should not throw an exception" true

let test_reveal_card_if_has _ =
  let suspect = random_card suspects in
  let weapon = random_card weapons in
  let location = random_card locations in
  let cards = [ suspect; weapon; location ] in
  let player = create_player "Test Player" cards 0 0 in
  let revealed_card = reveal_card_if_has player cards in
  assert_equal (Some suspect) revealed_card

let test_update_clue_sheet _ =
  let player = create_player "Test Player" [] 0 0 in
  let card = random_card suspects in
  let _ = update_clue_sheet player card in
  assert_bool "Updating clue sheet should not throw an exception" true

let test_can_disprove_guess _ =
  let suspect = random_card suspects in
  let weapon = random_card weapons in
  let location = random_card locations in
  let cards = [ suspect; weapon; location ] in
  let player = create_player "Test Player" cards 0 0 in
  assert_bool "Player can disprove guess" (can_disprove_guess player cards)

let test_display_player_info _ =
  let hand = [ string_to_card "Prof. Plum" ] in
  let player = create_player "Test Player" hand 0 0 in

  let player = update_clue_sheet player (string_to_card "Kitchen") in

  display_player_info player;

  assert_bool "Displaying player info should not throw an exception" true

let test_get_name _ =
  let player = create_player "Test Player" [] 0 0 in
  assert_equal "Test Player" (get_name player)

let test_get_position _ =
  let player = create_player "Test Player" [] 5 7 in
  assert_equal 5 (get_x player);
  assert_equal 7 (get_y player)

let test_set_position _ =
  let player = create_player "Test Player" [] 0 0 in
  let moved_player = set_position 3 4 player in
  assert_equal 3 (get_x moved_player);
  assert_equal 4 (get_y moved_player)

let test_reveal_card_if_has_none _ =
  let player = create_player "Test Player" [] 0 0 in
  let cards = [ List.hd suspects ] in
  assert_equal None (reveal_card_if_has player cards)

let test_can_disprove_guess_false _ =
  let player = create_player "Test Player" [] 0 0 in
  let cards = [ List.hd suspects ] in
  assert_bool "can_disprove_guess should be false"
    (not (can_disprove_guess player cards))

(* =========================== *)
(*    Cards.ml Tests           *)
(* =========================== *)

let string_list_printer lst = "[" ^ String.concat "; " lst ^ "]"

let test_card_to_string _ =
  assert_equal
    ~printer:(fun x -> x)
    "Suspect: Mrs. Peacock"
    (card_to_string (List.hd suspects))

let test_suspect_to_string _ =
  assert_equal
    ~printer:(fun x -> x)
    "Mrs. Peacock"
    (suspect_to_string (List.hd suspects))

let test_suspect_to_string_fail _ =
  assert_raises (Failure "Expected a Suspect card!") (fun () ->
      suspect_to_string (List.hd locations))

let test_weapon_to_string _ =
  assert_equal
    ~printer:(fun x -> x)
    "Candlestick"
    (weapon_to_string (List.hd weapons))

let test_weapon_to_string_fail _ =
  assert_raises (Failure "Expected a Weapon card!") (fun () ->
      weapon_to_string (List.hd locations))

let test_location_to_string _ =
  assert_equal
    ~printer:(fun x -> x)
    "Study"
    (location_to_string (List.hd locations))

let test_location_to_string_fail _ =
  assert_raises (Failure "Expected a Location card!") (fun () ->
      location_to_string (List.hd suspects))

let test_cards_list_to_string _ =
  assert_equal
    ~printer:(fun x -> x)
    "Suspect: Mrs. Peacock, Suspect: Mrs. White, Suspect: Mr. Green, Suspect: \
     Prof. Plum, Suspect: Col. Mustard, Suspect: Miss Scarlett"
    (cards_list_to_string suspects)

let test_cards_list_to_string_empty _ =
  assert_equal ~printer:(fun x -> x) "" (cards_list_to_string [])

let test_get_card_names _ =
  assert_equal ~printer:string_list_printer
    [
      "Mrs. Peacock";
      "Mrs. White";
      "Mr. Green";
      "Prof. Plum";
      "Col. Mustard";
      "Miss Scarlett";
    ]
    (get_card_names suspects)

let test_get_card_names_locations _ =
  assert_equal ~printer:string_list_printer
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
    (get_card_names locations)

let test_string_to_card_suspect _ =
  assert_equal (List.hd suspects) (string_to_card "Mrs. Peacock")

let test_string_to_card_weapon _ =
  assert_equal (List.hd weapons) (string_to_card "Candlestick")

let test_string_to_card_location _ =
  assert_equal (List.hd locations) (string_to_card "Study")

let test_string_to_card_invalid _ =
  assert_raises (Failure "Invalid card: Mr. Boddy") (fun () ->
      string_to_card "Mr. Boddy")

(* =========================== *)
(*    Game.ml Tests            *)
(* =========================== *)

let test_make_rooms _ =
  let expected =
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
  in
  assert_equal ~printer:string_list_printer expected make_rooms

let test_choose_solution _ =
  let suspect, weapon, location = choose_solution in
  assert_bool "Chosen suspect should be in suspects list"
    (List.mem suspect suspects);
  assert_bool "Chosen weapon should be in weapons list"
    (List.mem weapon weapons);
  assert_bool "Chosen location should be in locations list"
    (List.mem location locations)

let test_solution_to_strings _ =
  let example_solution =
    (List.hd suspects, List.hd weapons, List.hd locations)
  in
  let suspect_str, weapon_str, location_str =
    solution_to_strings example_solution
  in
  assert_equal ~printer:(fun x -> x) "Mrs. Peacock" suspect_str;
  assert_equal ~printer:(fun x -> x) "Candlestick" weapon_str;
  assert_equal ~printer:(fun x -> x) "Study" location_str

let test_remove_card _ =
  let original_list = [ "A"; "B"; "C"; "D" ] in
  let new_list = remove_card original_list "B" in
  assert_bool "Removed card should not be in the list"
    (not (List.mem "B" new_list))

let test_list_of_cards_without_solution _ =
  let suspect, weapon, location = choose_solution in
  let new_list = list_of_cards_without_solution (suspect, weapon, location) in
  assert_bool "Chosen suspect should not be in the list"
    (not (List.mem suspect new_list));
  assert_bool "Chosen weapon should not be in the list"
    (not (List.mem weapon new_list));
  assert_bool "Chosen location should not be in the list"
    (not (List.mem location new_list))

let test_shuffle_list _ =
  Random.init 42;
  let lst = [ 1; 2; 3; 4; 5 ] in
  let shuffled1 = shuffle_list lst in
  let shuffled2 = shuffle_list lst in
  assert_bool "Shuffled lists should be different" (shuffled1 <> shuffled2)

let test_get_six_cards _ =
  let lst = all_cards in
  let player_1_card_names = get_card_names (get_six_cards lst 0 5) in
  let player_2_card_names = get_card_names (get_six_cards lst 6 11) in
  assert_equal ~printer:string_list_printer
    [
      "Mrs. Peacock";
      "Mrs. White";
      "Mr. Green";
      "Prof. Plum";
      "Col. Mustard";
      "Miss Scarlett";
    ]
    player_1_card_names;
  assert_equal ~printer:string_list_printer
    [ "Candlestick"; "Revolver"; "Lead Pipe"; "Wrench"; "Dagger"; "Rope" ]
    player_2_card_names

let test_game_over _ =
  let correct_solution =
    (List.hd suspects, List.hd weapons, List.hd locations)
  in
  let correct_guess = (List.hd suspects, List.hd weapons, List.hd locations) in
  let incorrect_guess =
    ( List.hd (List.rev suspects),
      List.hd (List.rev weapons),
      List.hd (List.rev locations) )
  in

  assert_bool "Game should be over when the correct guess is made"
    (game_over correct_guess correct_solution);

  assert_bool "Game should not be over when the guess is incorrect"
    (not (game_over incorrect_guess correct_solution))

let test_choose_solution_validity _ =
  for _ = 1 to 100 do
    let s, w, l = choose_solution in
    let suspects_names = get_card_names suspects in
    let weapons_names = get_card_names weapons in
    let locations_names = get_card_names locations in

    assert_bool "Valid suspect" (List.mem (suspect_to_string s) suspects_names);
    assert_bool "Valid weapon" (List.mem (weapon_to_string w) weapons_names);
    assert_bool "Valid location"
      (List.mem (location_to_string l) locations_names)
  done

let test_list_of_cards_without_solution_length _ =
  let solution = (List.hd suspects, List.hd weapons, List.hd locations) in
  let filtered = list_of_cards_without_solution solution in
  let all_names = get_card_names all_cards in
  let filtered_names = get_card_names filtered in
  assert_equal (List.length all_names - 3) (List.length filtered_names)

let test_game_over_partial_matches _ =
  let solution = (List.hd suspects, List.hd weapons, List.hd locations) in
  let guess = (List.nth suspects 1, List.hd weapons, List.hd locations) in
  assert_bool "Partial match fails" (not (game_over guess solution))

let test_make_rooms_contents _ =
  let rooms = make_rooms in
  assert_equal 9 (List.length rooms);
  assert_bool "Contains Study" (List.mem "Study" rooms);
  assert_bool "Contains Lounge" (List.mem "Lounge" rooms)

let test_roll_dice_range _ =
  for _ = 1 to 1000 do
    let roll = roll_dice () in
    assert_bool "Dice roll within valid range" (roll >= 1 && roll <= 6)
  done

let test_is_valid_move_boundaries _ =
  assert_bool "Minimum X valid" (is_valid_move [] 0 12);
  assert_bool "Maximum X valid" (is_valid_move [] 24 12);
  assert_bool "Minimum Y valid" (is_valid_move [] 12 0);
  assert_bool "Maximum Y valid" (is_valid_move [] 12 24);
  assert_bool "X too low invalid" (not (is_valid_move [] (-1) 12));
  assert_bool "X too high invalid" (not (is_valid_move [] 25 12));
  assert_bool "Y too low invalid" (not (is_valid_move [] 12 (-1)));
  assert_bool "Y too high invalid" (not (is_valid_move [] 12 25))

let test_is_valid_move_collisions _ =
  let player = create_player "Test" [] 5 5 in
  assert_bool "Occupied position invalid" (not (is_valid_move [ player ] 5 5));
  assert_bool "Unoccupied position valid" (is_valid_move [ player ] 5 6)

let test_room_at_position _ =
  assert_equal (Some (List.hd rooms)) (room_at_position 0 20);
  assert_equal (Some (List.hd rooms)) (room_at_position 4 24);
  assert_equal None (room_at_position 5 20);

  let ballroom = List.nth rooms 1 in
  assert_equal (Some ballroom) (room_at_position 10 20);
  assert_equal (Some ballroom) (room_at_position 15 24);
  assert_equal None (room_at_position 16 20);

  assert_equal None (room_at_position 12 12)

let test_room_to_string _ =
  let kitchen = List.hd rooms in
  assert_equal "Kitchen" (room_to_string kitchen)

let test_check_portal _ =
  assert_equal (check_portal (3, 3)) (Some (21, 21));
  assert_equal (check_portal (2, 2)) None

let test_move_player_with_portal _ =
  let p = create_player "Test Player" [] 0 0 in
  let p_portal = move_player_and_check_portal 3 3 p in
  assert_equal (get_x p_portal) 21;
  assert_equal (get_y p_portal) 21;
  let p_portal = move_player_and_check_portal 21 21 p in
  assert_equal (get_x p_portal) 3;
  assert_equal (get_y p_portal) 3;
  let p_portal = move_player_and_check_portal 3 21 p in
  assert_equal (get_x p_portal) 21;
  assert_equal (get_y p_portal) 3;
  let p_portal = move_player_and_check_portal 21 3 p in
  assert_equal (get_x p_portal) 3;
  assert_equal (get_y p_portal) 21

let test_move_player_no_portal _ =
  let p = create_player "Test Player" [] 0 0 in
  let p_portal = move_player_and_check_portal 2 2 p in
  assert_equal (get_x p_portal) 2;
  assert_equal (get_y p_portal) 2

(* =========================== *)
(*         Test Suite          *)
(* =========================== *)

let suite =
  "Clue Tests"
  >::: [
         (* Clue_sheet.ml *)
         "initial_sheet" >:: test_initial_sheet;
         "update_sheet" >:: test_update_sheet;
         "display_clue_sheet" >:: test_display_clue_sheet;
         "test_initial_sheet_with_cards" >:: test_initial_sheet_with_cards;
         "test_update_multiple_times" >:: test_update_multiple_times;
         (* Player.ml *)
         "test_create_player_empty_hand" >:: test_create_player_empty_hand;
         "test_create_player_with_cards" >:: test_create_player_with_cards;
         "test_get_hand" >:: test_get_hand;
         "test_get_clue_sheet" >:: test_get_clue_sheet;
         "test_reveal_card_if_has" >:: test_reveal_card_if_has;
         "test_update_clue_sheet" >:: test_update_clue_sheet;
         "test_can_disprove_guess" >:: test_can_disprove_guess;
         "test_display_player_info" >:: test_display_player_info;
         "test_get_name" >:: test_get_name;
         "test_get_position" >:: test_get_position;
         "test_set_position" >:: test_set_position;
         "test_reveal_card_if_has_none" >:: test_reveal_card_if_has_none;
         "test_can_disprove_guess_false" >:: test_can_disprove_guess_false;
         (* Cards.ml *)
         "test_card_to_string" >:: test_card_to_string;
         "test_suspect_to_string" >:: test_suspect_to_string;
         "test_suspect_to_string_fail" >:: test_suspect_to_string_fail;
         "test_weapon_to_string" >:: test_weapon_to_string;
         "test_weapon_to_string_fail" >:: test_weapon_to_string_fail;
         "test_location_to_string" >:: test_location_to_string;
         "test_location_to_string_fail" >:: test_location_to_string_fail;
         "test_cards_list_to_string" >:: test_cards_list_to_string;
         "test_cards_list_to_string_empty" >:: test_cards_list_to_string_empty;
         "test_get_card_names" >:: test_get_card_names;
         "test_get_card_names_locations" >:: test_get_card_names_locations;
         "test_string_to_card_suspect" >:: test_string_to_card_suspect;
         "test_string_to_card_weapon" >:: test_string_to_card_weapon;
         "test_string_to_card_location" >:: test_string_to_card_location;
         "test_string_to_card_invalid" >:: test_string_to_card_invalid;
         (* Game.ml *)
         "test_make_rooms" >:: test_make_rooms;
         "test_choose_solution" >:: test_choose_solution;
         "test_solution_to_strings" >:: test_solution_to_strings;
         "test_remove_card" >:: test_remove_card;
         "test_list_of_cards_without_solution"
         >:: test_list_of_cards_without_solution;
         "test_shuffle_list" >:: test_shuffle_list;
         "test_get_six_cards" >:: test_get_six_cards;
         "test_game_over" >:: test_game_over;
         "test_make_rooms_contents" >:: test_make_rooms_contents;
         "test_choose_solution_validity" >:: test_choose_solution_validity;
         "test_list_of_cards_without_solution_length"
         >:: test_list_of_cards_without_solution_length;
         "test_game_over_partial_matches" >:: test_game_over_partial_matches;
         "test_solution_to_strings" >:: test_solution_to_strings;
         "test_roll_dice_range" >:: test_roll_dice_range;
         "test_is_valid_move_boundaries" >:: test_is_valid_move_boundaries;
         "test_is_valid_move_collisions" >:: test_is_valid_move_collisions;
         "test_room_at_position" >:: test_room_at_position;
         "test_room_to_string" >:: test_room_to_string;
         "test_check_portal" >:: test_check_portal;
         "test_move_player_with_portal" >:: test_move_player_with_portal;
         "test_move_player_no_portal" >:: test_move_player_no_portal;
       ]

let () = run_test_tt_main suite
