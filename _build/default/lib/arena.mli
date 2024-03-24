exception Not_Allowed of string

exception Invalid_Strategy of string

val init_player_with_strategie :
  (Engine.game_update -> Engine.player -> Engine.action) ->
  (Engine.game_update -> Engine.player -> Engine.action) ->
  Engine.player_strategie

val arena : Engine.player_strategie -> Engine.player_strategie -> Engine.template -> Engine.end_game

val player_random : (int -> int) -> Engine.player_strategie

val player_human : Engine.player_strategie
