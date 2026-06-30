(* Abstract type to represent a clue sheet *)
type t

(* [initial_sheet] creates an initial clue sheet based on the player's cards *)
val initial_sheet : Cards.t list -> t

(* [display_clue_sheet] displays the current state of the clue sheet *)
val display_clue_sheet : t -> unit

(* [display_bool_list] displays a boolean list on given cards*)
val display_bool_list : bool list -> Cards.t list -> unit

(* [update_sheet] updates the clue sheet by crossing off the specified item *)
val update_sheet : t -> Cards.t -> t

(* [suspects_of_sheet sheet] returns the suspect elimination status list.*)
val suspects_of_sheet : t -> bool list

(* [weapons_of_sheet sheet] returns the weapon elimination status list.*)
val weapons_of_sheet : t -> bool list

(* [locations_of_sheet sheet] returns the locations elimination status list.*)
val locations_of_sheet : t -> bool list
