open Mill.Engine
open QCheck
(*
let reverse_traductor (i, j) =
    match (i, j) with
    | 0, 0 -> (0, 0)
    | 0, 6 -> (0, 1)
    | 0, 12 -> (0, 2)
    | 2, 2 -> (1, 0)
    | 2, 6 -> (1, 1)
    | 2, 10 -> (1, 2)
    | 4, 4 -> (2, 0)
    | 4, 6 -> (2, 1)
    | 4, 8 -> (2, 2)
    | 6, 0 -> (3, 0)
    | 6, 2 -> (3, 1)
    | 6, 4 -> (3, 2)
    | 6, 8 -> (3, 3)
    | 6, 10 -> (3, 4)
    | 6, 12 -> (3, 5)
    | 8, 4 -> (4, 0)
    | 8, 6 -> (4, 1)
    | 8, 8 -> (4, 2)
    | 10, 2 -> (5, 0)
    | 10, 6 -> (5, 1)
    | 10, 10 -> (5, 2)
    | 12, 0 -> (6, 0)
    | 12, 6 -> (6, 1)
    | 12, 12 -> (6, 2)
    | _ -> (i, j)
    *)

let equals_coordinate (c1 : coordinates) (c2 : coordinates) : bool = fst c1 = fst c2 && snd c1 = snd c2

let rec equals_list_coordinate (l1 : coordinates list) (l2 : coordinates list) : bool =
    match (l1, l2) with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | x :: xs, y :: ys -> equals_coordinate x y && equals_list_coordinate xs ys

let equals_player (p1 : player) (p2 : player) : bool =
    p1.color = p2.color
    && equals_list_coordinate p1.bag p2.bag
    && p1.piece_placed = p2.piece_placed
    && p1.nb_pieces_on_board = p2.nb_pieces_on_board

let equals_game_update (g1 : game_update) (g2 : game_update) : bool =
    equals_board (get_board g1) (get_board g2)
    && get_is_mill g1 = get_is_mill g2
    && equals_player (get_player_1 g1) (get_player_1 g2)
    && equals_player (get_player_2 g1) (get_player_2 g2)
    && get_board_is_changed g1 = get_board_is_changed g2
    && get_max_pieces g1 = get_max_pieces g2

let equals_end_game (end_game1 : end_game) (end_game2 : end_game) : bool =
    equals_game_update end_game1.game end_game2.game
    && equals_player end_game1.loser end_game2.loser
    && equals_player end_game1.winner end_game2.winner

let test_complete_board (game_update : game_update) : bool =
    let liste = get_all_free_positions game_update in
    List.length liste = 0

let generate_color =
    let open QCheck in
    Gen.oneof [Gen.return Black; Gen.return White]

let arbitrary_color =
    let open QCheck in
    make generate_color

let generate_templates =
    let open QCheck in
    Gen.oneof
      [
        Gen.return Six_mens_morris;
        Gen.return Three_mens_morris;
        Gen.return Nine_mens_morris;
        Gen.return Twelve_mens_morris;
      ]

let arbitrary_templates = QCheck.make generate_templates

let player_random_dumb (random : int -> int) : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            (* We also allow the bot to go outside the board by 1 square (to make him very dumb)*)
            let i = random (board_length (get_board game_update) + 2) - 1 in
            let j = random (board_length (get_board game_update) + 2) - 1 in
            Placing (i, j)
        | Moving ->
            let i = random (List.length player.bag + 2) - 1 in
            let coord = List.nth player.bag i in
            let possible_move = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
            let j = random (List.length possible_move + 2) - 1 in
            let dir = List.nth possible_move j in
            Moving (coord, dir)
        | Flying ->
            (* We also allow the bot to go outside the board by 1 square (to make him very dumb)*)
            let i = random (board_length (get_board game_update) + 2) - 1 in
            let j = random (board_length (get_board game_update) + 2) - 1 in
            let coord_arrive = (i, j) in
            let i = random (List.length player.bag) in
            let depart = List.nth player.bag i in
            Flying (depart, coord_arrive)
    in
    (* The removing strategy is here *)
    let strategie_remove (game_update : game_update) (player : player) : action =
        let i = random (List.length (get_opponent game_update player.color).bag) in
        Remove (List.nth (get_opponent game_update player.color).bag i)
    in
    { strategie_play; strategie_remove }

let player_invalid_pos : player_strategie =
    (* The placing/moving strategy is here *)
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing ->
            (* We also allow the bot to go outside the board by 1 square (to make him very dumb)*)
            let i = -1 in
            let j = -1 in
            Placing (i, j)
        | Moving ->
            let i = Random.int (List.length player.bag + 2) - 1 in
            let coord = List.nth player.bag i in
            let possible_move = [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left] in
            let j = Random.int (List.length possible_move + 2) - 1 in
            let dir = List.nth possible_move j in
            Moving (coord, dir)
        | Flying ->
            (* We also allow the bot to go outside the board by 1 square (to make him very dumb)*)
            let i = Random.int (board_length (get_board game_update) + 2) - 1 in
            let j = Random.int (board_length (get_board game_update) + 2) - 1 in
            let coord_arrive = (i, j) in
            let i = Random.int (List.length player.bag) in
            let depart = List.nth player.bag i in
            Flying (depart, coord_arrive)
    in
    (* The removing strategy is here *)
    let strategie_remove (game_update : game_update) (player : player) : action =
        let i = Random.int (List.length (get_opponent game_update player.color).bag) in
        Remove (List.nth (get_opponent game_update player.color).bag i)
    in
    { strategie_play; strategie_remove }

let generate_coordinates =
    let open QCheck in
    Gen.pair Gen.int Gen.int

let generate_direction = Gen.oneof [Gen.return H; Gen.return V; Gen.return DR; Gen.return DL]

let fill_template_with_colors (template : template) : board =
    let fill_row_template (square : square) : square =
        match square with
        | Color _ -> square
        | _ -> Color (QCheck.Gen.generate1 generate_color)
    in
    board_map_all fill_row_template (init_board_with_template template)

let triple_gen_template_coordinates_color =
    QCheck.Gen.(
      pair generate_templates (pair generate_coordinates generate_color)
      |> map (fun (template, (coordinates, color)) -> (template, coordinates, color)))

let arbitrary_triple_template_coordinates_color = QCheck.make triple_gen_template_coordinates_color

let phase_gen : phase QCheck.Gen.t =
    let open QCheck in
    Gen.oneof [Gen.return (Placing : phase); Gen.return (Moving : phase); Gen.return (Flying : phase)]

let player_gen : player Gen.t =
    let open Gen in
    let gen_bag = list (pair int int) in
    let* phase = phase_gen in
    let* color = generate_color in
    let* piece_placed = small_nat in
    let* nb_pieces_on_board = small_nat in
    let* bag = gen_bag in
    return { phase; color; piece_placed; nb_pieces_on_board; bag }

let gen_valid_coords_nine =
    let size = max_piece_from_template Nine_mens_morris in
    let gen_coord = Gen.int_range 0 (size - 1) in
    Gen.pair gen_coord gen_coord

let gen_invalid_coords_nine =
    let gen_coord = Gen.oneof [Gen.int_range (-100) (-10); Gen.int_range 100 1000] in
    Gen.pair gen_coord gen_coord
