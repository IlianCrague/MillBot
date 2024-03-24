open Engine

type pos_tree =
    (*Node : ((game_update, player) * move_from_father * sons*)
    | Node of (game_update * player) * action * pos_tree list

(*Player that uses minimax only on the moving phase, kept placing and removing phase same as the other player
  I suggest running it with a depth of 4, can be very slow sometimes, average of time for a game against player_random is 38 seconds
*)
let my_player_minmax ((profondeur : int), (random : int -> int)) =
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

    (* (nb mill, nb blocked, nb of man on versatile positions) , here nb_mills is 3 times the usual nb of mill*)
    let nb_mill_and_blocked_man game_update player =
        let plus (m, b, n) (m2, b2, n2) = (m + m2, b + b2, n + n2) in
        let eq (i, j) (i2, j2) = i = i2 && j = j2 in
        let rec aux_nb_mill_and_blocked game_update player bag =
            match bag with
            | [] -> (0, 0, 0)
            | coord :: tl ->
                let add_versatile =
                    if eq coord (2, 6) || eq coord (6, 2) || eq coord (6, 10) || eq coord (10, 6) then 1 else 0
                in
                let add_mill = if check_mill game_update coord player.color then 1 else 0 in
                let add_blocked =
                    if List.length (possible_moves_directions game_update coord player.color) = 0 then 1 else 0
                in
                plus (add_mill, add_blocked, add_versatile) (aux_nb_mill_and_blocked game_update player tl)
        in
        aux_nb_mill_and_blocked game_update player player.bag
    in

    let win gu pl = if lost gu pl then -1 else if lost gu (get_opponent gu pl.color) then 1 else 0 in

    (*
    80% against player_random at depth of 4 after 100 games*)
    let evaluation_move tree =
        let aux (gu, pl) =
            let opp = get_opponent gu pl.color in
            let nb_mill_p, nb_blocked_p, nb_versatile_p = nb_mill_and_blocked_man gu pl in
            let nb_mill_o, nb_blocked_o, nb_versatile_o = nb_mill_and_blocked_man gu opp in
            (*very good if we have more mills than opponent*)
            let res =
                (2 * (nb_mill_p - nb_mill_o))
                (*good if we have less blocked man than opponent*)
                + (1 * (nb_blocked_o - nb_blocked_p))
                (*good if we more men than opponent*)
                + (2 * (List.length pl.bag - List.length opp.bag))
                (*tried using the fact that having pieces on some specific places was good, made it worse*)
                + (0 * (nb_versatile_p - nb_versatile_o))
                (*better than anything if we win, worst than anything if we lose*)
                + (1000 * win gu pl)
            in
            res
        in
        match tree with
        | Node ((gu, pl), _, _) -> aux (gu, pl)
    in

    (*67% against player_random at depth 4 after 50 games
        let evaluation_move tree =
            let aux (gu, pl) =
                let opp = get_opponent gu pl.color in
                let (nb_mill_p, nb_blocked_p, nb_versatile_p) = nb_mill_and_blocked_man gu pl in
                let (nb_mill_o, nb_blocked_o, nb_versatile_o) = nb_mill_and_blocked_man gu opp in
                let res = ((20 * (nb_mill_p - nb_mill_o))
                + (10 * (nb_blocked_o - nb_blocked_p))
                + (20 * (List.length pl.bag - List.length opp.bag))
                + (5 * (nb_versatile_p - nb_versatile_o))
                + (1000 * win gu pl)) in res
            in
            match tree with
            | Node ((gu, pl), _, _) -> aux (gu, pl)
        in
    *)
    let inter l1 l2 = List.fold_left (fun acc x -> if List.exists (fun y -> y = x) l1 then x :: acc else acc) [] l2 in

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

    let remove game_update player =
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

    let rec get_coord_mills game_update bag player =
        match bag with
        | [] -> []
        | coord :: tl ->
            if check_mill game_update coord player.color
            then coord :: get_coord_mills game_update tl player
            else get_coord_mills game_update tl player
    in

    (*returns "l1 \ l2" : all elements in l1 which are not in l2*)
    let rec minus l1 l2 =
        let rec element_of_l1_in_l2 el_l1 l2 =
            match l2 with
            | [] -> false
            | n :: tl -> if n = el_l1 then true else element_of_l1_in_l2 el_l1 tl
        in
        match l1 with
        | [] -> []
        | n :: tl -> if element_of_l1_in_l2 n l2 then minus tl l2 else n :: minus tl l2
    in

    (*apply and call remove if mill*)
    let my_apply game_update player action =
        let rec remove_for_each_new_mill l gu =
            match l with
            | [] -> gu
            | _ :: tl -> remove_for_each_new_mill tl (apply gu player (remove gu player))
        in
        let mills = get_coord_mills game_update player.bag player in
        let new_gu = apply game_update player action in
        let new_mills = get_coord_mills new_gu player.bag player in
        remove_for_each_new_mill (minus new_mills mills) new_gu
    in

    let rec tree_list_from_gu_move gu player poss_moves =
        match poss_moves with
        | (dep, _, dir) :: tl ->
            Node ((my_apply gu player (Moving (dep, dir)), get_opponent gu player.color), Moving (dep, dir), [])
            :: tree_list_from_gu_move gu player tl
        | [] -> []
    in

    let init_tree gu player move =
        Node ((gu, player), move, tree_list_from_gu_move gu player (all_possible_moves gu player player.bag))
    in

    (*from a leaf : replacing tree_list by next_move*)
    let add_one_move (Node ((g, p), mv, _)) =
        let rec add_one_move_to_list tree_list move =
            match tree_list with
            | [] -> []
            | Node ((new_gu, new_pl), new_mv, t) :: next_node ->
                Node ((new_gu, new_pl), new_mv, List.map (fun _ -> init_tree new_gu new_pl new_mv) t)
                :: add_one_move_to_list next_node move
        in
        add_one_move_to_list (tree_list_from_gu_move g p (all_possible_moves g p p.bag)) mv
    in

    let minmax_tree_list children (tree_and_value_list : (pos_tree * int) list) (g : game_update) (player : player) :
        pos_tree * int =
        let rec aux_minmax tree_list acc =
            match tree_list with
            | [] ->
                Format.printf "no new children\n";
                (Node ((g, player), Placing (-1, -1), []), 0)
            | (Node ((_, pl), mv, _), n) :: [] ->
                let comp = if pl = player then ( < ) else ( > ) in
                if comp n acc then (Node ((g, player), mv, children), n) else (Node ((g, player), mv, children), acc)
            | (Node ((_, pl), _, _), n) :: tl ->
                let comp = if pl = player then ( < ) else ( > ) in
                if comp n acc then aux_minmax tl n else aux_minmax tl acc
        in
        aux_minmax tree_and_value_list 0
    in
    (*generates n levels of pos_tree and returns the father*)
    let generate_n_levels n game_update player =
        let rec aux_generate_n_levels n (Node ((g, p), mv, l)) =
            if n = 0
            then (Node ((g, get_opponent g p.color), mv, l), evaluation_move (Node ((g, get_opponent g p.color), mv, l)))
            else
              let children = add_one_move (Node ((g, p), mv, l)) in
              if List.length children = 0
              then (Node ((g, p), mv, l), evaluation_move (Node ((g, p), mv, l)))
              else
                let new_children = List.map (aux_generate_n_levels (n - 1)) children in
                minmax_tree_list children new_children g p
        in
        aux_generate_n_levels n (init_tree game_update player (Placing (-1, -1)))
    in

    let best_move game_update player =
        let rec move_from_tree gu_father pl list_tree =
            match list_tree with
            | [] ->
                Format.printf "fail best move\n";
                Moving ((-1, -1), Up)
            | Node ((gu_son, _), res, _) :: tl ->
                if my_apply gu_father pl res = gu_son then res else move_from_tree gu_father pl tl
        in
        let Node ((gu, pl), _, l), _ = generate_n_levels profondeur game_update player in
        move_from_tree gu pl l
    in

    let place_versatile free_pos list_priority =
        let rec get_v pos list_priority =
            match list_priority with
            | [] -> false
            | p :: tl -> p = pos || get_v pos tl
        in
        List.find_opt (fun pos -> get_v pos list_priority) free_pos
    in
    let place_next_opponent_move_mill (free_pos : coordinates list) (game_update : game_update) (player : player) :
        coordinates option =
        let rec is_move_mill game_update (free_pos : coordinates list) : coordinates option =
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
    let strategie_play (game_update : game_update) (player : player) : action =
        match player.phase with
        | Placing -> Placing (place game_update player)
        | Moving -> (*pretty_print_board (get_board game_update);*) best_move game_update player
        | Flying ->
            let i = random (List.length player.bag) in
            let depart = List.nth player.bag i in
            Flying (depart, place game_update player)
    in
    let strategie_remove (game_update : game_update) (player : player) : action = remove game_update player in
    { strategie_play; strategie_remove }
