type t
(*Abstract type to represent a card*)

val card_to_string : t -> string
(* [card_to_string] converts card into a string *)

val suspect_to_string : t -> string
(* [suspect_to_string] converts suspect card into a string*)

val weapon_to_string : t -> string
(* [weapon_to_string] converts weapon card into a string*)

val location_to_string : t -> string
(* [location_to_string] onverts location card into a string*)

val cards_list_to_string : t list -> string
(* [cards_list_to_string] converts card list into a string*)

val get_card_names : t list -> string list
(* [get_card_names] converts card list into a string list*)

val suspects : t list
(* List of suspect cards *)

val weapons : t list
(* List of weapon cards *)

val locations : t list
(* List of location cards *)

val all_cards : t list
(* List of suspect, weapon, and location cards *)

val string_to_card : string -> t
(* [string_to_card] converts a string to its card form*)

val display_name_of_card : t -> string
(* [display_name_of_card] displays the name of the card*)
