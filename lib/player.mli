(* Abstract type to represent a player *)
type t

(* [create_player] creates a new player given a name and a hand of cards *)
val create_player : string -> Cards.t list -> int -> int -> t

(* [get_hand] returns the player's hand *)
val get_hand : t -> Cards.t list

(* [get_clue_sheet] returns the player's clue sheet *)
val get_clue_sheet : t -> Clue_sheet.t

(* [reveal_card_if_has] given a list of cards, return Some card if the player
   has one of them, otherwise None *)
val reveal_card_if_has : t -> Cards.t list -> Cards.t option

(* [update_clue_sheet] cross off a card in the player's clue sheet *)
val update_clue_sheet : t -> Cards.t -> t

(* [display_player_info] displays the player's information *)
val display_player_info : t -> unit

(* [can_disprove_guess] returns true if the player has any card in the guess *)
val can_disprove_guess : t -> Cards.t list -> bool

(* [get_name] returns the player's name *)
val get_name : t -> string

(* [get_x] returns the player's x coordinate *)
val get_x : t -> int

(* [get_y] returns the player's y coordinate *)
val get_y : t -> int

(* [set_position] sets the player's x y coordinates to a provided x y
   position *)
val set_position : int -> int -> t -> t
