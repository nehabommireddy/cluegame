open Graphics

type room
(** Abstract type representing a game room *)

val make_rooms : string list
(* [make_rooms] returns a list of all of the rooms in Clue as a string. *)

val make_suspects : string list
(* [make_suspects] returns a list of all of the suspects in Clue as a string. *)

val make_weapons : string list
(* [make_weapons] eturns a list of all of the weapons in Clue as a string. *)

val choose_solution : Cards.t * Cards.t * Cards.t
(* [choose_solution] choose a random solution consisting of a suspect, weapon,
   and location. Returns a tuple of Cards.t representing the chosen cards. *)

val solution_to_strings :
  Cards.t * Cards.t * Cards.t -> string * string * string
(* [solution_to_strings] convert the chosen solution (a tuple of Cards.t) into a
   tuple of strings representing the suspect, weapon, and location as
   strings. *)

val remove_card : 'a list -> 'a -> 'a list
(* [remove_card] returns specified list with the specified card removed. *)

val list_of_cards_without_solution : Cards.t * Cards.t * Cards.t -> Cards.t list
(* [list_of_cards_without_solution] given the tuple with the 3 solution cards,
   returns the card list without these 3 cards. *)

val shuffle_list : 'a list -> 'a list
(* [shuffle_list] randomly shuffles given list. *)

val get_six_cards : 'a list -> int -> int -> 'a list
(* [get_six_cards] given a shuffled list, gets the cards from i to j. *)

val game_over : 'a * 'b * 'c -> 'a * 'b * 'c -> bool
(* [game_over] compares given tuple to the solution tuple. *)

val ansi_color_of_room : string -> string
(** [ansi_color_of_room name] returns the ANSI color code string corresponding
    to the given room name. *)

(* [roll_dice] generates a random integer between 1-6 inclusive *)
val roll_dice : unit -> int

(* [is_valid_move players x y] checks if moving to (x,y) is allowed *)
val is_valid_move : Player.t list -> int -> int -> bool

(* List of all rooms in the game *)
val rooms : room list

(* [room_at_position x y] returns Some room if (x,y) is inside a room, None if
   in a hallway/clue square*)
val room_at_position : color -> color -> room option

val move_outside_room : Player.t -> Player.t
(** [move_outside_room player] moves player to designated exit position if
    currently in a room, returns unchanged player otherwise *)

val room_to_string : room -> string
(** [room_to_string room] returns room's display name as string *)

type portal = {
  start : int * int;
  end_point : int * int;
}
(** Abstract type representing a room with coordinates*)

val portals : portal list
(** Abstract type representing a game room *)

val check_portal : color * color -> (color * color) option
(*[check_portal] checks whether a set of coordinates is a portal*)

val move_player_and_check_portal : color -> color -> Player.t -> Player.t
(*[move_player_and_check_portal] checks whether a player is in a portal and
  moves then accordingly*)

val ansi_color_of_room_color : string -> color
(*[ansi_color_of_room_color] returns the ANSI color corresponsing to the given
  room name*)
