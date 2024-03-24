val equals_coordinate : Mill.Engine.coordinates -> Mill.Engine.coordinates -> bool

val equals_list_coordinate : Mill.Engine.coordinates list -> Mill.Engine.coordinates list -> bool

val equals_player : Mill.Engine.player -> Mill.Engine.player -> bool

val equals_game_update : Mill.Engine.game_update -> Mill.Engine.game_update -> bool

val equals_end_game : Mill.Engine.end_game -> Mill.Engine.end_game -> bool

val test_complete_board : Mill.Engine.game_update -> bool

val generate_color : Mill.Engine.color QCheck.Gen.t

val arbitrary_color : Mill.Engine.color QCheck.arbitrary

val generate_templates : Mill.Engine.template QCheck.Gen.t

val arbitrary_templates : Mill.Engine.template QCheck.arbitrary

val player_random_dumb : (int -> int) -> Mill.Engine.player_strategie

val player_invalid_pos : Mill.Engine.player_strategie

val generate_coordinates : (int * int) QCheck.Gen.t

val generate_direction : Mill.Engine.direction QCheck.Gen.t

val fill_template_with_colors : Mill.Engine.template -> Mill.Engine.board

val triple_gen_template_coordinates_color : (Mill.Engine.template * (int * int) * Mill.Engine.color) QCheck.Gen.t

val arbitrary_triple_template_coordinates_color :
  (Mill.Engine.template * (int * int) * Mill.Engine.color) QCheck.arbitrary

val phase_gen : Mill.Engine.phase QCheck.Gen.t

val player_gen : Mill.Engine.player QCheck.Gen.t

val gen_valid_coords_nine : (int * int) QCheck.Gen.t

val gen_invalid_coords_nine : (int * int) QCheck.Gen.t
