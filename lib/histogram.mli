(** Braille histogram generation module *)

val make_braille_char_dual : float -> float -> int
(** Create a braille character with dual columns *)

val make_braille_char : float -> int
(** Create a braille character with single column (left only) *)

val generate_braille_histogram : float array -> int -> int -> string
(** Generate a braille histogram from float data *)

