open Engine

let my_player (random : int -> int) =
    let place_versatile free_pos list_priority =
        let rec get_v pos list_priority =
            match list_priority with
            | [] -> false
            | p :: tl -> p = pos || get_v pos tl
        in
        List.find_opt (fun pos -> get_v pos list_priority) free_pos
    in
    (*sometimes the check_mill_from_position function doesn't work well in these coordinates*)
    let check_mill game_update coord color =
        let square_color board color coord =
            match get_square board coord with
            | Some (Color c) -> c = color
            | _ -> false
        in
        let board = get_board game_update in
        (square_color board color (2, 2) && square_color board color (6, 2) && square_color board color (10, 2))
        || (square_color board color (6, 0) && square_color board color (6, 2) && square_color board color (6, 4))
        || check_mill_from_position (get_board game_update) coord color
    in
    let place_next_opponent_move_mill free_pos game_update player =
        let rec is_move_mill game_update (free_pos : coordinates list) =
            match free_pos with
            | [] -> None
            | (i, j) :: tl ->
                let new_game_up = apply game_update (get_opponent game_update player.color) (Placing (i, j)) in
                if check_mill new_game_up (i, j) (reverse_color player.color)
                then Some (i, j)
                else is_move_mill game_update tl
        in
        is_move_mill game_update free_pos
    in

    let inter l1 l2 = List.fold_left (fun acc x -> if List.exists (fun y -> y = x) l1 then x :: acc else acc) [] l2 in

    let place game_update player =
        let free_pos = get_all_free_positions game_update in
        let choose_coord () =
            match place_versatile free_pos [(2, 6); (6, 2); (6, 10); (10, 6); (6, 0); (0, 6); (6, 12); (12, 6)] with
            | Some (i, j) -> (i, j)
            | None -> List.nth free_pos (Random.int (List.length free_pos))
        in
        let mill =
            ( place_next_opponent_move_mill free_pos game_update player,
              place_next_opponent_move_mill free_pos game_update (get_opponent game_update player.color) )
        in
        match mill with
        | _, Some (i, j) -> (i, j)
        | Some (i, j), _ -> (i, j)
        | _ -> choose_coord ()
    in
    let rec all_possible_moves game_update player coords =
        let rec aux (i, j) list_dir =
            match list_dir with
            | [] -> []
            | dir :: tl -> (
                match node_from_direction (get_board game_update) (i, j) dir with
                | None -> aux (i, j) tl
                | Some d -> ((i, j), d, dir) :: aux (i, j) tl)
        in
        match coords with
        | [] -> []
        | (i, j) :: tl ->
            aux (i, j) (possible_moves_directions game_update (i, j) player.color)
            @ all_possible_moves game_update player tl
    in

    let move_to_mill all_pos player game_update =
        List.find_opt
          (fun (dep, arr, dir) ->
            let new_game_up = apply game_update player (Moving (dep, dir)) in
            check_mill_from_position (get_board new_game_up) arr player.color)
          all_pos
    in

    let move_to_avoid_mill_from_opp game_update player all_pos_o all_pos_p =
        match move_to_mill all_pos_o (get_opponent game_update player.color) game_update with
        | None -> None
        | Some (_, (a1, a2), _) -> (
            match List.find_opt (fun (_, arr, _) -> arr = (a1, a2)) all_pos_p with
            | Some (d, _, a) -> Some (d, a)
            | None -> None)
    in

    (*returns true if opponent can make a mill by moving*)
    let check_next_move_not_mill all_pos_p coord game_update opp =
        match List.find_opt (fun (_, arr, _) -> arr = coord) all_pos_p with
        | Some (_, arr, _) -> if check_mill game_update arr opp.color then true else false
        | None -> false
    in

    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing -> Placing (place game_update player)
        | Moving -> (
            let opponent = get_opponent game_update player.color in
            let all_pos_p = all_possible_moves game_update player player.bag in
            let all_pos_o = all_possible_moves game_update opponent (get_opponent game_update player.color).bag in
            let choise_mouv =
                let j = random (List.length all_pos_p) in
                let coord, _, dir = List.nth all_pos_p j in
                Moving (coord, dir)
            in

            (*make a mill if we can*)
            match move_to_mill all_pos_p player game_update with
            | Some (dep, _, dir) -> Moving (dep, dir)
            | None -> (
                (*avoid mill from opponent if we can*)
                match move_to_avoid_mill_from_opp game_update player all_pos_o all_pos_p with
                | Some (a, b) -> Moving (a, b)
                | None -> (
                    match
                      (*step out from a mill we can*)
                      List.find_opt
                        (fun (dep, _, _) -> check_mill_from_position (get_board game_update) dep player.color)
                        all_pos_p
                    with
                    | Some (dep, arr, dir) ->
                        (*check if the move makes the opponent able to mill*)
                        if not (check_next_move_not_mill all_pos_p arr game_update opponent)
                        then Moving (dep, dir)
                        else choise_mouv
                    | None -> choise_mouv)))
        | Flying ->
            let i = random (List.length player.bag) in
            let depart = List.nth player.bag i in
            Flying (depart, place game_update player)
    in
    let strategie_remove (game_update : game_update) (player : player) : action =
        let opponent = get_opponent game_update player.color in
        let all_pos_o = all_possible_moves game_update opponent (get_opponent game_update player.color).bag in
        let smart_remove =
            let rec aux_rem l =
                match l with
                | [] -> (
                    (*removing at strategic coordinates*)
                    match inter [(2, 6); (6, 2); (6, 10); (10, 6)] opponent.bag with
                    | (i, j) :: _ -> (i, j)
                    | [] -> (
                        match inter [(0, 0); (0, 12); (12, 0); (12, 12)] opponent.bag with
                        | (i, j) :: _ -> (i, j)
                        | [] ->
                            let i = random (List.length opponent.bag) in
                            List.nth opponent.bag i))
                | coord :: tl ->
                    (*removing in priority a man that can move*)
                    if List.length (possible_moves_directions game_update coord opponent.color) > 0
                    then coord
                    else aux_rem tl
            in
            aux_rem opponent.bag
        in
        let rec coord_remove all_moves_o =
            match all_moves_o with
            | (dep, arr, dir) :: tl -> (
                let new_game_up = apply game_update opponent (Moving (dep, dir)) in
                (*if opponent can move to mill we remove the man*)
                if check_mill new_game_up arr opponent.color
                then dep
                else
                  (*if removing a man can lead us to a mill we do it*)
                  let all_moves_p = all_possible_moves game_update player player.bag in
                  match move_to_mill all_moves_p player game_update with
                  | Some _ -> dep
                  | None -> coord_remove tl)
            | [] -> smart_remove
        in
        Remove (coord_remove all_pos_o)
    in
    { strategie_play; strategie_remove }
