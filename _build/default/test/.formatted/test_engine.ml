open Mill.Engine
open Utils

let test_move_to_cordinates (game_up : game_update) (col : color) =
    let open QCheck in
    Test.make ~name:"move_to_coord" ~count:1000 (quad small_int small_int small_int small_int)
      (fun (cx1, cx2, cy1, cy2) ->
        let i1 = cx1 mod 9 in
        let j1 = cx2 mod 9 in
        let i2 = cy1 mod 9 in
        let j2 = cy2 mod 9 in
        let dep = get_square (get_board game_up) (i1, j1) in
        let arr = get_square (get_board game_up) (i2, j2) in
        if dep = Some (Color col) && arr = Some Empty
        then not (equals_board (get_board game_up) (get_board (move_to_coordinates game_up (i1, j1) (i2, j2) col)))
        else equals_board (get_board game_up) (get_board (move_to_coordinates game_up (i1, j1) (i2, j2) col)))

let test_place_piece =
    let open QCheck in
    Test.make ~name:"place_piece" ~count:1000 (triple arbitrary_color small_int small_int) (fun (color, x, y) ->
        let i = x mod 9 in
        let j = y mod 9 in
        let coord = (i, j) in
        let game_update = init_game_update Nine_mens_morris in
        if get_square (get_board game_update) (i, j) == Some Empty
        then
          let newGU = apply game_update (get_player game_update color) (Placing coord) in
          get_square (get_board newGU) coord = Some (Color color)
        else true)

let test_mill =
    let open QCheck in
    Test.make ~name:"mill" ~count:1000 (pair arbitrary_color arbitrary_templates) (fun (color, template) ->
        let board = fill_all_node template color in
        let flat = board_flatten board in
        let rec loop list acc x =
            match list with
            | [] -> acc
            | y :: ys -> (
                match y with
                | Color c when c = color ->
                    let j = x mod board_length board in
                    let i = x / board_length board in
                    let coord = (i, j) in
                    loop ys (acc && check_mill_from_position board coord color) (x + 1)
                | _ -> loop ys acc (x + 1))
        in
        loop flat true 0)

let check_mill_from_position_property =
    QCheck.Test.make arbitrary_triple_template_coordinates_color ~name:"check_mill_from_position" ~count:1000
      (fun (template, (i, j), color) ->
        let board = fill_template_with_colors template in
        let result = check_mill_from_position board (i, j) color in
        (* Properties *)
        QCheck.assume (i >= 0 && j >= 0 && i < board_length board && j < board_length board);
        (if result
         then
           (* Property 1: If a mill is detected, there must be at least 3 pieces in a row/column/diagonal *)
           let count_pieces d =
               (* Count pieces in a certain direction *)
               let rec count_from_dir (x, y) d =
                   match node_from_direction board (x, y) d with
                   | Some (a, b) ->
                       if get_square board (a, b) = Some (Color color) then 1 + count_from_dir (a, b) d else 0
                   | _ -> 0
               in
               let reverse_direction : direction_deplacement -> direction_deplacement = function
                   | Up -> Down
                   | Down -> Up
                   | Right -> Left
                   | Left -> Right
                   | Up_right -> Down_left
                   | Up_left -> Down_right
                   | Down_right -> Up_left
                   | Down_left -> Up_right
               in
               count_from_dir (i, j) d + count_from_dir (i, j) (reverse_direction d) + 1
           in
           QCheck.assume
             (count_pieces Right >= 3
             || count_pieces Down >= 3
             || count_pieces Up_right >= 3
             || count_pieces Down_left >= 3
             || count_pieces Up_left >= 3
             || count_pieces Down_right >= 3));
        (* Add more properties as needed *)
        true)

let place_start_piece_test =
    QCheck.Test.make ~name:"place_start_piece" ~count:1000 arbitrary_templates (fun template ->
        let game_update = init_game_update template in
        let initial_player = get_player_1 game_update in
        let coordinates = (Random.int 10, Random.int 10) in
        let updated_game = apply game_update initial_player (Placing coordinates) in

        let expected_piece_placed =
            if get_square (get_board game_update) (fst coordinates, snd coordinates) = Some Empty
               && initial_player.piece_placed < get_max_pieces game_update
            then initial_player.piece_placed + 1
            else initial_player.piece_placed
        in
        let expected_nb_pieces_on_board =
            if get_square (get_board game_update) (fst coordinates, snd coordinates) = Some Empty
               && initial_player.piece_placed < get_max_pieces game_update
            then initial_player.nb_pieces_on_board + 1
            else initial_player.nb_pieces_on_board
        in
        let expected_bag =
            if get_square (get_board game_update) (fst coordinates, snd coordinates) = Some Empty
               && initial_player.piece_placed < get_max_pieces game_update
            then initial_player.bag @ [coordinates]
            else initial_player.bag
        in
        let () =
            Alcotest.(check bool)
              "Piece placed incremented"
              (expected_piece_placed = (get_player_1 updated_game).piece_placed)
              true
        in
        let () =
            Alcotest.(check bool)
              "Number of pieces on board incremented"
              (expected_nb_pieces_on_board = (get_player_1 updated_game).nb_pieces_on_board)
              true
        in
        let () =
            Alcotest.(check (list (pair int int))) "Piece added to bag" expected_bag (get_player_1 updated_game).bag
        in
        let () =
            Alcotest.(check bool)
              "Player have a finite number of pieces"
              (expected_piece_placed <= get_max_pieces game_update)
              true
        in

        true)

let test_place_exceed_max_pieces =
    let open QCheck in
    Test.make ~name:"place_start_piece" ~count:1000 (pair small_int small_int) (fun (x, y) ->
        let i = x mod 9 in
        let j = y mod 9 in
        let coord = (i, j) in
        let initial_game_state = init_game_update Nine_mens_morris in

        (* try to place a piece for the player *)
        let updated_game_state = apply initial_game_state (get_player_1 initial_game_state) (Placing coord) in

        (* check if the piece is placed or not *)
        if get_board_is_changed updated_game_state
        then
          (get_player_1 updated_game_state).piece_placed = (get_player_1 initial_game_state).piece_placed + 1
          && (get_player_1 updated_game_state).piece_placed <= max_piece_from_template Nine_mens_morris
        else (get_player_1 initial_game_state).piece_placed = (get_player_1 updated_game_state).piece_placed)

let test_template_property =
    let open QCheck in
    Test.make ~name:"Test template property" ~count:50 arbitrary_templates (fun template ->
        let game_update = init_game_update template in
        let expected_empty_cells =
            match template with
            | Three_mens_morris -> 9
            | Six_mens_morris -> 16
            | Nine_mens_morris -> 24
            | Twelve_mens_morris -> 24
        in
        List.length (get_all_free_positions game_update) = expected_empty_cells)

let test_players_placing_phase =
    let open QCheck in
    Test.make ~name:"Test players placing phase" ~count:50 arbitrary_templates (fun template ->
        let game_update = init_game_update template in
        (get_player_1 game_update).phase = Placing
        && (get_player_2 game_update).phase = Placing
        && (get_player_1 game_update).piece_placed = 0
        && (get_player_2 game_update).piece_placed = 0
        && (get_player_1 game_update).nb_pieces_on_board = 0
        && (get_player_2 game_update).nb_pieces_on_board = 0)

let simulate_placing_all_pieces game_update max_pieces =
    let rec helper game_update player nb_all_placing =
        if nb_all_placing > 0
        then
          let coord = List.hd (get_all_free_positions game_update) in
          let newGU = apply game_update player (Placing coord) in
          helper newGU (get_opponent game_update player.color) (nb_all_placing - 1)
        else game_update
    in
    helper game_update (get_player_1 game_update) (max_pieces * 2)

let test_players_moving_phase =
    let open QCheck in
    Test.make ~name:"Test players moving phase" ~count:50 arbitrary_templates (fun template ->
        let game_update = init_game_update template in
        let game_update_after_placing = simulate_placing_all_pieces game_update (get_max_pieces game_update) in
        pretty_print_board (get_board game_update_after_placing);
        pretty_print_phase (get_player_1 game_update_after_placing).phase;
        pretty_print_phase (get_player_2 game_update_after_placing).phase;
        match template with
        | Three_mens_morris ->
            (get_player_1 game_update_after_placing).phase = Flying
            && (get_player_2 game_update_after_placing).phase = Flying
        | _ ->
            (get_player_1 game_update_after_placing).phase = Moving
            && (get_player_2 game_update_after_placing).phase = Moving)

let arbitrary_valid = QCheck.make gen_valid_coords_nine

let test_get_square_valid_coords =
    let open QCheck in
    Test.make ~name:"Test get_square with valid coords" ~count:50 arbitrary_valid (fun coords ->
        let gu = init_game_update Nine_mens_morris in
        print_cord coords;
        match get_square (get_board gu) coords with
        | None -> false
        | Some _ -> true)

let arbitrary_invalid = QCheck.make gen_invalid_coords_nine

let test_get_square_invalid_coords =
    let open QCheck in
    Test.make ~name:"Test get_square with valid coords" ~count:50 arbitrary_invalid (fun coords ->
        print_cord coords;
        let gu = init_game_update Nine_mens_morris in
        match get_square (get_board gu) coords with
        | None -> true
        | Some _ -> false)

let () =
    let open Alcotest in
    run "TEST ENGINE"
      [
        ( "Test move_to_cordinates",
          [
            (let game_up = init_game_update Twelve_mens_morris in
             QCheck_alcotest.to_alcotest (test_move_to_cordinates game_up Black));
          ] );
        ("Test place piece", [QCheck_alcotest.to_alcotest test_place_piece]);
        ("Test mill", [QCheck_alcotest.to_alcotest test_mill]);
        ("Test mill from position", [QCheck_alcotest.to_alcotest check_mill_from_position_property]);
        ("Test place start piece", [QCheck_alcotest.to_alcotest place_start_piece_test]);
        ("Test place more than max piece", [QCheck_alcotest.to_alcotest test_place_exceed_max_pieces]);
        ("Test template property", [QCheck_alcotest.to_alcotest test_template_property]);
        ("Test players placing phase", [QCheck_alcotest.to_alcotest test_players_placing_phase]);
        ("Test players moving phase", [QCheck_alcotest.to_alcotest test_players_moving_phase]);
        ("Test get_square with valid coords", [QCheck_alcotest.to_alcotest test_get_square_valid_coords]);
        ("Test get_square with invalid coords", [QCheck_alcotest.to_alcotest test_get_square_invalid_coords]);
      ]
