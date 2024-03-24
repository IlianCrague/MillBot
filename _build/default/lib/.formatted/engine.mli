type color = Black | White

type direction = H | V | DR | DL

type coordinates = int * int

type square = Empty | Path of direction | Wall | Color of color

type board

type phase = Placing | Moving | Flying

type direction_deplacement = Up | Down | Right | Left | Up_right | Up_left | Down_right | Down_left

type player = { phase: phase; color: color; piece_placed: int; nb_pieces_on_board: int; bag: coordinates list }

type action =
    | Placing of coordinates
    | Moving of coordinates * direction_deplacement
    | Flying of coordinates * coordinates
    | Remove of coordinates

type game_update

type player_strategie = {
    strategie_play: game_update -> player -> action;
    strategie_remove: game_update -> player -> action;
  }

type got_mill = board * bool

type template = Three_mens_morris | Six_mens_morris | Nine_mens_morris | Twelve_mens_morris

type end_game = { game: game_update; winner: player; loser: player }

val affiche_tour_info : color -> unit

val print_move : direction_deplacement -> unit

val pretty_print_list_direction : direction_deplacement list -> unit

val print_cord : int * int -> unit

val print_square : square -> unit

val pretty_print_board : board -> unit

val pretty_print_phase : phase -> unit

val get_square : board -> int * int -> square option

val check_mill_from_position : board -> coordinates -> color -> bool

val max_piece_from_template : template -> int

val init_board_with_template : template -> board

(** This function initialize a new game_update
    * If players are not correctly initialized, the function will raise an exception
   *)
val init_game_update : template -> game_update

val get_board : game_update -> board

val get_is_mill : game_update -> bool

val get_board_is_changed : game_update -> bool

val get_player_1 : game_update -> player

val get_player_2 : game_update -> player

val get_max_pieces : game_update -> int

val board_length : board -> int

val board_flatten : board -> square list

val equals_board : board -> board -> bool

val board_map_all : (square -> square) -> board -> board

val for_all_board : (square -> bool) -> board -> bool

val fill_all_node : template -> color -> board

val nb_to_get_mill : int

val get_player : game_update -> color -> player

val reverse_color : color -> color

val get_opponent : game_update -> color -> player

val not_updated_game : game_update -> game_update

val coordinates_from_directions : direction_deplacement -> coordinates -> coordinates

val path_to_have_from_direction : direction_deplacement -> square

val node_from_direction : board -> coordinates -> direction_deplacement -> coordinates option

val eliminate_piece : game_update -> coordinates -> color -> game_update

val move_to_coordinates : game_update -> coordinates -> coordinates -> color -> game_update

val possible_moves_directions : game_update -> coordinates -> color -> direction_deplacement list

val apply : game_update -> player -> action -> game_update

val cant_move_piece_on_board : player -> game_update -> bool

val lost : game_update -> player -> bool

val update_player_phase : player -> int -> player

val update_phase : game_update -> game_update

val traductor : coordinates -> int -> coordinates

val get_all_free_positions : game_update -> coordinates list
