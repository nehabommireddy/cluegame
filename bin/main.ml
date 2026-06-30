open Final_project
open Game
open Player
open Clue_sheet
open Cards
open Graphics

(** [get_valid_room ()] prompts the user to enter a room name, checks if the
    input is a valid room, and returns the room as a string. If the input is
    invalid, it prompts again recursively. *)
let rec get_valid_room () =
  print_endline "\nWhere did they do it (Pick a number)\n";
  List.iteri
    (fun i card ->
      Printf.printf "%d. %s\n" (i + 1) (Cards.display_name_of_card card))
    Cards.locations;
  match int_of_string_opt (read_line ()) with
  | Some n when n >= 1 && n <= List.length Cards.locations ->
      Cards.location_to_string (List.nth Cards.locations (n - 1))
  | _ ->
      print_endline "Invalid input. Please enter a valid number.";
      get_valid_room ()

(** [get_valid_suspect ()] prompts the user to enter a suspect name, checks if
    the input is a valid suspect, and returns the suspect as a string. If the
    input is invalid, it prompts again recursively. *)

let rec get_valid_suspect () =
  print_endline "\nWho did it? (Pick a number)\n";
  List.iteri
    (fun i card ->
      Printf.printf "%d. %s\n" (i + 1) (Cards.display_name_of_card card))
    Cards.suspects;
  match int_of_string_opt (read_line ()) with
  | Some n when n >= 1 && n <= List.length Cards.suspects ->
      Cards.suspect_to_string (List.nth Cards.suspects (n - 1))
  | _ ->
      print_endline "Invalid input. Please enter a valid number.";
      get_valid_suspect ()

(** [get_valid_weapon ()] prompts the user to enter a weapon name, checks if the
    input is a valid weapon, and returns the weapon as a string. If the input is
    invalid, it prompts again recursively. *)
let rec get_valid_weapon () =
  print_endline "\nWith what object? (Pick a number)\n";
  List.iteri
    (fun i card ->
      Printf.printf "%d. %s\n" (i + 1) (Cards.display_name_of_card card))
    Cards.weapons;
  match int_of_string_opt (read_line ()) with
  | Some n when n >= 1 && n <= List.length Cards.weapons ->
      Cards.weapon_to_string (List.nth Cards.weapons (n - 1))
  | _ ->
      print_endline "Invalid input. Please enter a valid number.";
      get_valid_weapon ()

(** [starting_position name] returns the starting coordinates (x, y) for a
    player given name. Default (12,12)*)

let starting_position name =
  match name with
  | "Miss Scarlett" -> (12, 9) (* bottom middle *)
  | "Col. Mustard" -> (12, 17) (* top middle *)
  | "Mrs. White" -> (17, 17) (* top right *)
  | "Mr. Green" -> (8, 9) (* bottom left *)
  | "Mrs. Peacock" -> (8, 17) (* top left *)
  | "Prof. Plum" -> (17, 9) (* bottom right *)
  | _ -> (12, 12)

(** [play_turn player other_player solution extra_pile room_player_in] manages a
    single player's turn. The player can attempt to solve the case or make a
    guess. If guessing, the function reveals a matching card or allows picking
    from extra pile. Returns the updated player. *)
let rec play_turn player other_player solution extra_pile room_player_in =
  Unix.sleep 1;
  print_endline
    ("\nYou think the crime was committed in the " ^ room_player_in ^ "!");
  Unix.sleep 2;
  print_endline "\nNow guess the person and weapon!";
  Unix.sleep 2;
  let suspect = get_valid_suspect () in
  Unix.sleep 1;
  let weapon = get_valid_weapon () in

  let matched_card =
    reveal_card_if_has other_player
      [
        string_to_card suspect;
        string_to_card weapon;
        string_to_card room_player_in;
      ]
  in
  match matched_card with
  | Some card ->
      Unix.sleep 1;
      let bold str = "\027[1m" ^ str ^ "\027[0m" in
      print_endline
        ("\nOther player has: "
        ^ bold (card_to_string card)
        ^ ". It is now revealed and taken off your CLUE sheet.");
      let player_updated = update_clue_sheet player card in
      display_player_info player_updated;
      player_updated
  | None ->
      Unix.sleep 1;
      print_endline
        "\nOther player does not have any relevant cards. Pick a card 1-6.";
      let card_ind = read_int () in
      let picked_card = List.nth extra_pile (card_ind - 1) in
      let bold str = "\027[1m" ^ str ^ "\027[0m" in
      print_endline ("\nYou picked card " ^ bold (card_to_string picked_card));
      Unix.sleep 1;
      let updated_player = update_clue_sheet player picked_card in
      display_player_info updated_player;
      updated_player

(** The size of the board (number of grid squares). *)
let board_size = 25

(** The pixel size of each grid cell. *)
let cell_size = 30

let offset_x = 25
let offset_y = 25
let to_screen_x x = offset_x + x
let to_screen_y y = offset_y + y

(** [draw_grid] Draws the main grid of the board. *)
let draw_grid () =
  set_color (rgb 101 67 33);
  for i = 0 to board_size do
    moveto (to_screen_x 0) (to_screen_y (i * cell_size));
    lineto (to_screen_x (board_size * cell_size)) (to_screen_y (i * cell_size));
    moveto (to_screen_x (i * cell_size)) (to_screen_y 0);
    lineto (to_screen_x (i * cell_size)) (to_screen_y (board_size * cell_size))
  done

(** [draw_cell x y color] draws a single colored cell at grid (x, y). *)
let draw_cell x y color =
  set_color color;
  fill_rect
    (to_screen_x (x * cell_size) + 1)
    (to_screen_y (y * cell_size) + 1)
    (cell_size - 2) (cell_size - 2)

(** [label_cell x y text] writes text label inside grid cell at (x, y). *)
let label_cell x y label =
  set_color black;
  moveto (to_screen_x (x * cell_size) + 5) (to_screen_y (y * cell_size) + 10);
  draw_string label

(** [label_room name x y w h] labels a room at grid rectangle starting at (x, y)
    with width w and height h. *)
let label_room name x y w h =
  set_color black;
  set_font "-*-courier-bold-r-*-*-20-*-*-*-*-*-*-*";
  let box_px = to_screen_x (x * cell_size) in
  let box_py = to_screen_y (y * cell_size) in
  let box_w = w * cell_size in
  let box_h = h * cell_size in
  let tw, th = text_size name in
  let tx = box_px + ((box_w - tw) / 2) in
  let ty = box_py + ((box_h + th) / 2) in
  moveto tx ty;
  draw_string name

(** [draw_horizontal_line] draws a horizontal line with thickness. *)
let draw_horizontal_line x1 y1 x2 y2 thickness =
  for i = -thickness / 2 to thickness / 2 do
    moveto (to_screen_x x1) (to_screen_y (y1 + i));
    lineto (to_screen_x x2) (to_screen_y (y2 + i))
  done

(** [draw_vertical_line] draws a vertical line with thickness. *)
let draw_vertical_line x1 y1 x2 y2 thickness =
  for i = -thickness / 2 to thickness / 2 do
    moveto (to_screen_x (x1 + i)) (to_screen_y y1);
    lineto (to_screen_x (x2 + i)) (to_screen_y y2)
  done

(** [darken_color color factor] returns a darker version of color. *)
let darken_color color factor =
  let r = (color lsr 16) land 0xFF in
  let g = (color lsr 8) land 0xFF in
  let b = color land 0xFF in
  let new_r = max 0 (r * factor / 100) in
  let new_g = max 0 (g * factor / 100) in
  let new_b = max 0 (b * factor / 100) in
  rgb new_r new_g new_b

(** [draw_room cells color] draws all the cells of the room and outlines it with
    a dark thick border. *)

let draw_room cells color =
  List.iter (fun (x, y) -> draw_cell x y color) cells;

  let xs, ys = List.split cells in
  let min_x = List.fold_left min max_int xs in
  let max_x = List.fold_left max min_int xs in
  let min_y = List.fold_left min max_int ys in
  let max_y = List.fold_left max min_int ys in

  let border_color = darken_color color 60 in
  set_color border_color;
  let x1 = min_x * cell_size in
  let y1 = min_y * cell_size in
  let x2 = (max_x + 1) * cell_size in
  let y2 = (max_y + 1) * cell_size in
  let thickness = 4 in
  draw_horizontal_line x1 y2 x2 y2 thickness;
  draw_horizontal_line x1 y1 x2 y1 thickness;
  draw_vertical_line x1 y1 x1 y2 thickness;
  draw_vertical_line x2 y1 x2 y2 thickness

let kitchen =
  let start_x = 0 in
  let start_y = 20 in
  let size = 5 in
  List.init size (fun dx ->
      List.init size (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let ballroom =
  let start_x = 10 in
  let start_y = 20 in
  let width = 6 in
  let height = 5 in
  List.init width (fun dx ->
      List.init height (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let conservatory =
  let start_x = 20 in
  let start_y = 20 in
  let size = 5 in
  List.init size (fun dx ->
      List.init size (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let dining_room =
  let start_x = 0 in
  let start_y = 9 in
  let width = 6 in
  let height = 6 in
  List.init width (fun dx ->
      List.init height (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let lounge =
  let start_x = 0 in
  let start_y = 0 in
  let size = 5 in
  List.init size (fun dx ->
      List.init size (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let hall =
  let start_x = 10 in
  let start_y = 0 in
  let width = 6 in
  let height = 6 in
  List.init width (fun dx ->
      List.init height (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let study =
  let start_x = 20 in
  let start_y = 0 in
  let size = 5 in
  List.init size (fun dx ->
      List.init size (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let billiard_room =
  let start_x = 20 in
  let start_y = 14 in
  let width = 5 in
  let height = 4 in
  List.init width (fun dx ->
      List.init height (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let library =
  let start_x = 20 in
  let start_y = 8 in
  let width = 5 in
  let height = 4 in
  List.init width (fun dx ->
      List.init height (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let clue_square =
  let start_x = 8 in
  let start_y = 9 in
  let width = 10 in
  let height = 9 in
  List.init width (fun dx ->
      List.init height (fun dy -> (start_x + dx, start_y + dy)))
  |> List.flatten

let draw_title () =
  let offset_x = 25 in
  let offset_y = 25 in

  let text1 = "CAML" in
  let text2 = "CLUE" in
  let textq = "?" in

  let start_x = offset_x + (8 * cell_size) in
  let start_y = offset_y + (9 * cell_size) in
  let width = 10 * cell_size in
  let height = 9 * cell_size in
  let center_y = start_y + (height / 2) in

  let line_spacing = 50 in

  set_font "-*-times-bold-r-*-*-120-*-*-*-*-*-*-*";
  let twq, _ = text_size textq in
  let center_xq = start_x + ((width - twq) / 2) in
  set_color (rgb 245 245 220);
  moveto center_xq (center_y - 30);
  draw_string textq;

  set_font "-*-times-bold-r-*-*-40-*-*-*-*-*-*-*";
  let tw2, _ = text_size text2 in
  let center_x2 = start_x + ((width - tw2) / 2) in
  moveto center_x2 (center_y - line_spacing - 20);
  draw_string text2

(** [draw_board_and_players players current_player] draws the entire board, all
    rooms, and all players. The players are distinct by their colors *)
let draw_board_and_players players current_player =
  let offset_x = 25 in
  let offset_y = 25 in

  set_color (rgb 250 240 200);
  fill_rect offset_x offset_y (25 * cell_size) (25 * cell_size);

  set_color (rgb 101 67 33);
  set_line_width 15;
  draw_rect offset_x offset_y (25 * cell_size) (25 * cell_size);
  set_line_width 1;

  draw_grid ();
  let crimson_red = rgb 210 100 100 in
  draw_room kitchen crimson_red;
  set_font "-*-courier-bold-r-*-*-20-*-*-*-*-*-*-*";
  label_room "Kitchen" 0 20 5 4;
  let olive_green = rgb 140 170 90 in
  draw_room ballroom olive_green;
  label_room "Ballroom" 10 20 6 4;
  let navy_blue = rgb 100 120 200 in
  draw_room conservatory navy_blue;
  label_room "Conservatory" 20 20 5 4;
  let pale_teal = rgb 140 200 190 in
  draw_room dining_room pale_teal;
  set_font "-*-courier-bold-r-*-*-25-*-*-*-*-*-*-*";
  label_room "Dining" 0 10 6 4;
  label_room "Room" 0 9 6 4;
  let mustard_yellow = rgb 218 165 32 in
  draw_room lounge mustard_yellow;
  set_font "-*-courier-bold-r-*-*-20-*-*-*-*-*-*-*";
  label_room "Lounge" 0 0 5 4;
  let plum_purple = rgb 170 130 200 in
  draw_room hall plum_purple;
  set_font "-*-courier-bold-r-*-*-25-*-*-*-*-*-*-*";
  label_room "Hall" 10 0 6 6;
  let muted_pink = rgb 220 150 160 in
  draw_room study muted_pink;
  set_font "-*-courier-bold-r-*-*-20-*-*-*-*-*-*-*";
  label_room "Study" 20 0 5 4;
  let chocolate_brown = Graphics.rgb 180 130 90 in
  draw_room library chocolate_brown;
  set_font "-*-courier-bold-r-*-*-20-*-*-*-*-*-*-*";
  label_room "Library" 20 8 5 3;
  let gray = Graphics.rgb 128 128 128 in
  draw_room billiard_room gray;
  set_font "-*-courier-bold-r-*-*-18-*-*-*-*-*-*-*";
  label_room "Billiard Room" 20 14 5 3;
  draw_room clue_square (rgb 101 67 33);
  draw_title ();
  List.iter
    (fun portal ->
      let px, py = portal.start in
      match portal.end_point with
      | ex, ey ->
          let room_name =
            match (ex, ey) with
            | 21, 21 -> "Conservatory"
            | 3, 3 -> "Lounge"
            | 21, 3 -> "Study"
            | 3, 21 -> "Kitchen"
            | _ -> "Kitchen"
          in
          let room_color = ansi_color_of_room_color room_name in
          draw_cell px py room_color)
    portals;

  let color_of p =
    match Player.get_name p with
    | "Mr. Green" -> Graphics.rgb 0 204 102
    | "Mrs. White" -> Graphics.rgb 255 255 255
    | "Miss Scarlett" -> Graphics.rgb 255 51 51
    | "Col. Mustard" -> Graphics.rgb 255 204 51
    | "Mrs. Peacock" -> Graphics.rgb 0 122 204
    | "Prof. Plum" -> Graphics.rgb 153 102 204
    | _ -> black
  in
  List.iter
    (fun p ->
      let c = color_of p in
      draw_cell (Player.get_x p) (Player.get_y p) c)
    players;

  synchronize ()

(** [bold str] returns the string [str] formatted in bold for terminal output.
*)
let bold str = "\027[1m" ^ str ^ "\027[0m"

(** [prompt_stay_in_room current other rest] checks if [current] player is in a
    room. If so, prompts them whether to stay or leave. If they leave, moves
    them outside the room and redraws the board. Returns the updated player and
    a bool indicating if they stayed. *)
let prompt_stay_in_room current other rest =
  let in_room =
    room_at_position (Player.get_x current) (Player.get_y current)
  in
  match in_room with
  | Some room ->
      let room_name = room_to_string room in
      let color_code = ansi_color_of_room room_name in
      Printf.printf "\027[%sm%s\n\027[0m" color_code
        (bold
           ("\nYou are currently in the " ^ room_name ^ ". Stay here? (Y/N)"));
      let choice = read_line () in
      if choice = "Y" || choice = "y" then (current, true)
      else
        let moved_out = move_outside_room current in
        draw_board_and_players (moved_out :: other :: rest) moved_out;
        (moved_out, false)
  | None -> (current, false)

(** [handle_movement player players dice_roll] allows the current player to move
    up to [dice_roll] steps using WASD keys. Player can press 'q' to stop early.
*)
let rec handle_movement player players dice_roll =
  let rec move steps_remaining curr_player =
    if steps_remaining <= 0 then begin
      let win_w, win_h = (size_x (), size_y ()) in
      set_color (rgb 245 245 220);
      fill_rect 0 0 win_w 30;
      synchronize ();
      curr_player
    end
    else begin
      let win_w, win_h = (size_x (), size_y ()) in
      set_color (rgb 245 245 220);
      fill_rect 0 0 win_w 30;

      set_color black;
      set_font "-*-times-bold-r-*-*-18-*-*-*-*-*-*-*";
      let msg =
        Printf.sprintf "Moves remaining: %d (Press 'q' to stop early)"
          steps_remaining
      in
      let tw, th = text_size msg in
      moveto ((win_w - tw) / 2) 5;
      draw_string msg;
      synchronize ();

      let ev = Graphics.wait_next_event [ Graphics.Key_pressed ] in
      let dx, dy =
        match ev.key with
        | 'w' | 'W' -> (0, 1)
        | 's' | 'S' -> (0, -1)
        | 'a' | 'A' -> (-1, 0)
        | 'd' | 'D' -> (1, 0)
        | 'q' ->
            set_color (rgb 160 82 45);
            fill_rect 0 0 win_w 30;
            synchronize ();
            (0, 0)
        | _ -> (0, 0)
      in
      if dx = 0 && dy = 0 then move 0 curr_player
      else
        let new_x = Player.get_x curr_player + dx in
        let new_y = Player.get_y curr_player + dy in
        if Game.is_valid_move players new_x new_y then begin
          let updated =
            Game.move_player_and_check_portal new_x new_y curr_player
          in
          let others =
            List.filter
              (fun p -> Player.get_name p <> Player.get_name curr_player)
              players
          in
          let new_players = updated :: others in
          draw_board_and_players new_players updated;
          move (steps_remaining - 1) updated
        end
        else move steps_remaining curr_player
    end
  in
  move dice_roll player

(** [prompt_solve_case moved solution] asks if player [moved] wants to attempt
    to solve the case. If they do and guess correctly, they win. If they guess
    wrong, they lose and the solution is shown. *)
let rec prompt_solve_case moved solution =
  print_endline
    "\nDo you want to attempt to solve the case? (If wrong, you lose!) (Y/N)";
  let solve_response = read_line () in
  match String.lowercase_ascii solve_response with
  | "y" -> begin
      let suspect = get_valid_suspect () in
      let weapon = get_valid_weapon () in
      let room = get_valid_room () in
      let guess = (suspect, weapon, room) in
      if game_over guess solution then begin
        print_endline "\n🎉 Congratulations! You solved the case!";
        exit 0
      end
      else begin
        let correct_suspect, correct_weapon, correct_room = solution in
        print_endline "\n❌ Wrong guess! The other player wins!";
        print_endline "\nThe correct solution was:";
        let suspect_card = Cards.string_to_card correct_suspect in
        let weapon_card = Cards.string_to_card correct_weapon in
        let room_card = Cards.string_to_card correct_room in
        Printf.printf "%s\n" (Cards.display_name_of_card suspect_card);
        Printf.printf "%s\n" (Cards.display_name_of_card weapon_card);
        Printf.printf "%s\n\n" (Cards.display_name_of_card room_card);
        exit 0
      end
    end
  | "n" -> ()
  | _ ->
      print_endline "Invalid input. Please enter Y or N.";
      prompt_solve_case moved solution

(** [prompt_room_guess moved other solution extra_pile] checks if [moved] player
    is in a room after moving, and prompts them if they want to make a guess. If
    yes, calls [play_turn] with the current room and returns updated player. *)
let rec prompt_room_guess moved other solution extra_pile =
  match room_at_position (Player.get_x moved) (Player.get_y moved) with
  | Some current_room -> begin
      let room_name = room_to_string current_room in
      let color_code = ansi_color_of_room room_name in
      print_endline
        ("\027[" ^ color_code ^ "m"
        ^ bold
            ("\nYou are now in the " ^ room_name
           ^ ". Would you like to make a guess? (Y/N)\027[0m"));
      let choice = read_line () in
      match String.lowercase_ascii choice with
      | "y" -> play_turn moved other solution extra_pile room_name
      | "n" -> moved
      | _ -> begin
          print_endline "Invalid input. Please enter Y or N.";
          prompt_room_guess moved other solution extra_pile
        end
    end
  | None -> moved

(** [game_loop players solution extra_pile] is the main game loop. Alternates
    turns between players, handles movement, solving, and guessing. *)
let rec game_loop players solution extra_pile =
  match players with
  | [] | [ _ ] -> game_loop players solution extra_pile
  | current :: other :: rest ->
      Unix.sleep 2;
      print_endline
        "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n";
      print_endline (bold ("\n>>> " ^ Player.get_name current ^ "'s turn!"));
      Unix.sleep 2;
      display_player_info current;

      let current, stayed_in_room = prompt_stay_in_room current other rest in

      let moved =
        if not stayed_in_room then begin
          print_endline
            "\n\
             Press Enter and double click on the board to roll dice, then \
             begin moving.\n\
             Use W-A-S-D keys to navigate.";
          ignore (read_line ());
          ignore (Graphics.wait_next_event [ Graphics.Button_down ]);

          let dice1 = Game.roll_dice () in
          let dice2 = Game.roll_dice () in
          let dice_total = dice1 + dice2 in
          Printf.printf "🎲 The dice is rolling a..... %d\n%!" dice_total;

          draw_board_and_players (current :: other :: rest) current;
          handle_movement current (current :: other :: rest) dice_total
        end
        else current
      in

      Unix.sleep 2;
      prompt_solve_case moved solution;
      let moved_after_guess =
        prompt_room_guess moved other solution extra_pile
      in
      game_loop ((other :: rest) @ [ moved_after_guess ]) solution extra_pile

let character_info =
  [
    ("Miss Scarlett", Graphics.red);
    ("Col. Mustard", Graphics.yellow);
    ("Mrs. White", Graphics.white);
    ("Mr. Green", Graphics.green);
    ("Mrs. Peacock", Graphics.blue);
    ("Prof. Plum", Graphics.cyan);
  ]

let get_character_info name = List.assoc name character_info

let rec prompt_character used =
  let available =
    List.filter
      (fun c -> not (List.mem (Cards.suspect_to_string c) used))
      Cards.suspects
  in

  print_endline "\nChoose your character! (Pick a number)";
  List.iteri
    (fun i c -> Printf.printf "%d) %s\n" (i + 1) (Cards.display_name_of_card c))
    available;

  print_string "> ";
  let input = read_line () in
  match int_of_string_opt input with
  | Some n when n >= 1 && n <= List.length available ->
      let chosen_card = List.nth available (n - 1) in
      Cards.suspect_to_string chosen_card
  | _ ->
      print_endline "Invalid selection. Try again.";
      prompt_character used

(** [print_rules ()] prints the rules of the Caml Clue game to the terminal. *)
let print_rules () =
  print_endline "\n--- CAML CLUE RULES ---";
  print_endline "1. The goal is to solve the mystery by correctly guessing the";
  print_endline "   suspect, weapon, and room.";
  print_endline
    "2. At the beginning of your turn, you may make a final accusation.";
  print_endline "   - If you are correct, you win the game!";
  print_endline "   - If you are wrong, you lose and are out of the game.";
  print_endline "3. If you choose not to make a final accusation, you can:";
  print_endline "   - Roll the dice and move around the board.";
  print_endline "   - If you enter a room, you may make a suggestion involving:";
  print_endline "     the room you're in, a suspect, and a weapon.";
  print_endline "4. The other player will try to disprove your suggestion by:";
  print_endline "   - Showing you one of their cards, if possible.";
  print_endline
    "   - If the other player can't disprove it, a card from the extra pile";
  print_endline "     may be revealed to you instead.";
  print_endline "5. Use the information you gather to eliminate possibilities.";
  print_endline
    "6. Each room contains a different colored square called a portal.";
  print_endline
    "   - If you land on a portal, you will be transported to the room";
  print_endline "     that matches the portal's color.";
  print_endline "------------------------\n"

(** [prompt_for_rules ()] prompts the player to either view the game rules or
    proceed directly to starting the game.*)
let rec prompt_for_rules () =
  print_endline
    "\nType \'rules\' to see the game rules or press Enter to start:";
  match read_line () with
  | "rules" -> print_rules ()
  | "" -> ()
  | _ ->
      print_endline "Invalid input. Please type \"rules\" or press Enter.";
      prompt_for_rules ()

(** [main ()] initializes the game, sets up the board, deals cards, opens the
    graphics window, draws the initial board, and starts the game loop. *)
let main () =
  let bold str = "\027[1m" ^ str ^ "\027[0m" in
  print_endline (bold "\n\n\n=============================");
  print_endline (bold "     WELCOME TO CAML CLUE!   ");
  print_endline (bold "=============================\n\n");
  Unix.sleep 2;
  prompt_for_rules ();
  let solution = choose_solution in
  let shuffled_deck = shuffle_list (list_of_cards_without_solution solution) in
  let player1_hand = get_six_cards shuffled_deck 0 5 in
  let player2_hand = get_six_cards shuffled_deck 6 11 in
  let extra_pile = get_six_cards shuffled_deck 12 17 in
  print_endline (bold "      PLAYER 1\n");
  let char1 = prompt_character [] in
  let color1 = get_character_info char1 in
  let x1, y1 = starting_position char1 in
  let player1 = create_player char1 player1_hand x1 y1 in

  print_endline (bold "\n      PLAYER 2\n");
  let char2 = prompt_character [ char1 ] in
  let color2 = get_character_info char2 in
  let x2, y2 = starting_position char2 in
  let player2 = create_player char2 player2_hand x2 y2 in

  open_graph " 800x800";
  set_window_title "Clue OCaml Board";
  auto_synchronize false;
  draw_board_and_players [ player1; player2 ] player1;
  Graphics.synchronize ();
  game_loop [ player1; player2 ] (solution_to_strings solution) extra_pile

let () = main ()
