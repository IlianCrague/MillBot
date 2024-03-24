(** Defines the two type of piece, Black pieces and White pieces *)
type color = Black | White

(**
  Will be used in the type : square.
  Example : If a piece wants to move down, but the box below do not contain a "Path of V" (V for vertical), it means that it can't go down.   
*)
type direction = H | V | DR | DL

(** Coordinates *)
type coordinates = int * int

(** The grid squares, stored in our type board *)
type square = Empty | Path of direction | Wall | Color of color

(** This will represent the game's board *)
type board = square list list

type phase = Placing | Moving | Flying

(** This type will be used when moving a piece to a certain direction *)
type direction_deplacement = Up | Down | Right | Left | Up_right | Up_left | Down_right | Down_left

(** Will represent the players *)
type player = { phase: phase; color: color; piece_placed: int; nb_pieces_on_board: int; bag: coordinates list }

(**This type represent an action*)
type action =
    | Placing of coordinates
    | Moving of coordinates * direction_deplacement
    | Flying of coordinates * coordinates
    | Remove of coordinates

(** This type will be returned after each function that alterate the state of the game *)
type game_update = {
    board: board;
    mill: bool;
    player1: player;
    player2: player;
    game_is_changed: bool;
    max_pieces: int;
  }

type player_strategie = {
    strategie_play: game_update -> player -> action;
    strategie_remove: game_update -> player -> action;
  }

(** Will be returned after a move, and will let us know if the move produce a mill or not *)
type got_mill = board * bool

(** Represent the name of defaults board (templates) *)
type template = Three_mens_morris | Six_mens_morris | Nine_mens_morris | Twelve_mens_morris

(**This type will be used after end game*)

type end_game = { game: game_update; winner: player; loser: player }

let affiche_tour_info color =
    match color with
    | Black -> Format.printf "Le tour de BLACK\n"
    | White -> Format.printf "Le tour de  WHITE\n"

let print_move (m : direction_deplacement) =
    match m with
    | Up -> Format.printf "Up\n"
    | Down -> Format.printf "Down\n"
    | Right -> Format.printf "Right\n"
    | Left -> Format.printf "Left\n"
    | Up_right -> Format.printf "Up_right\n"
    | Up_left -> Format.printf "Up_left\n"
    | Down_right -> Format.printf "Down_right\n"
    | Down_left -> Format.printf "Down_left\n"

let pretty_print_list_direction l = l |> List.iter (fun a -> print_move a)

let print_cord (x, y) =
    let exit = "x :" ^ string_of_int x ^ " y :" ^ string_of_int y ^ "\n" in
    Format.printf "%s" exit

(** Function that print a board square *)
let print_square (s : square) =
    match s with
    | Color White -> Format.printf "{W}"
    | Color Black -> Format.printf "{B}"
    | Empty -> Format.printf "{ }"
    | Path H -> Format.printf "---"
    | Path V -> Format.printf " | "
    | Path DR -> Format.printf " / "
    | Path DL -> Format.printf " \\ "
    | _ -> Format.printf "   "

(** Print the board in the shell *)
let pretty_print_board (b : board) : unit =
    List.iter
      (fun l ->
        List.iter print_square l;
        Format.printf "@.")
      b;
    Format.printf "\n"

let pretty_print_phase (p : phase) =
    match p with
    | Placing -> Format.printf "Phase de placement\n"
    | Moving -> Format.printf "Phase de deplacement\n"
    | Flying -> Format.printf "Phase de vol\n"

(**
  A map that apply the function "f" to the square at the coordinate (i,j) of the board
  @param f : the function to apply to the square
  @param board : the board
  @param (i,j) : the coordinate of the square to change
*)
let board_map (f : square -> square) (board : board) ((i, j) : coordinates) =
    List.mapi (fun x line -> if x = i then List.mapi (fun y el -> if y = j then f el else el) line else line) board

(**
  Function to get the square from board at (i,j)
  @param board : the board
  @param ij : the coordinate of the square that we want to get
*)
let get_square (board : board) (i, j) : square option =
    if List.length board = 0
    then None
    else if i >= List.length board || i < 0 || j >= List.length (List.nth board 0) || j < 0
    then None (*of course we know that the board is a square*)
    else Some (List.nth (List.nth board i) j)

let get_square_row (row : square list) (j : int) : square option =
    if List.length row = 0 then None else if j >= List.length row || j < 0 then None else Some (List.nth row j)

(** function who tests the maximum of nodes a column can have in order to tell if the point (i,j) can be a path or not *)
let rec test_3_squares_row
    (board : board)
    (width : int)
    (height : int)
    (i : int)
    (j : int)
    (nb_nodes : int)
    (nb_squares : int) : bool =
    if nb_nodes = 3 && j != width / 2
    then
      false
      (*if we already saw 3 nodes and we're not in the middle then we're stopping right here because if we're in the middle we put path(V) everywhere except for the center*)
    else if i < 0
    then
      true
      (*if we reach the end of the board this means we didn't see enough nodes so we can put another path in direction of this last node*)
    else if List.nth (List.nth board i) j = Empty
    then
      test_3_squares_row board width height (i - 1) j (nb_nodes + 1)
        nb_squares (*if we see a node then we increment the nb_nodes for the rest of the board*)
    else test_3_squares_row board width height (i - 1) j nb_nodes nb_squares

let rec aux_init_board (width : int) (height : int) (i : int) (nb_squares : int) (diagonal : bool) (acc : board) : board
    =
    if i > width
    then acc (*base case of the function*)
    else
      (*function that takes width height and a number of squares in argument and returns all the coordinates of the points that board should have, so of course this coordinates form the shape of a square*)
      let rec square_coordinates (w : int) (h : int) (start : int) (nb_squares : int) (acc2 : coordinates list) :
          coordinates list =
          if nb_squares <= 0
          then acc2
          else if width / 2 = start
          then acc2 @ [(start, start)]
          else
            square_coordinates (w - 4) (h - 4) (start + 2) (nb_squares - 1)
              (acc2
              @ [
                  (start, start);
                  (start, start + (w / 2));
                  (start, start + w);
                  (start + (h / 2), start);
                  (start + (h / 2), start + w);
                  (start + h, start);
                  (start + h, start + (w / 2));
                  (start + h, start + w);
                ])
      in
      let square_coord = square_coordinates width height 0 nb_squares [] in
      (*function who creates recursively row in the board with the coordinates of the nodes and connect them with some paths*)
      let rec create_row (j : int) (acc2 : square list) (nb_nodes : int) : square list =
          if j > width
          then acc2
          else if List.mem (i, j) square_coord
          then create_row (j + 1) (acc2 @ [Empty]) (nb_nodes + 1)
          else if ((nb_squares - 1) * 2 < i && i < width - ((nb_squares - 1) * 2))
                  && (nb_squares - 1) * 2 < j
                  && j < width - ((nb_squares - 1) * 2)
          then create_row (j + 1) (acc2 @ [Wall]) nb_nodes
          else if diagonal && i = j
          then create_row (j + 1) (acc2 @ [Path DL]) nb_nodes
          else if diagonal && i = width - j
          then create_row (j + 1) (acc2 @ [Path DR]) nb_nodes
          else if (List.mem (i, j - 1) square_coord || get_square_row acc2 (j - 1) = Some (Path H))
                  && (nb_nodes != 3 || i = width / 2)
          then create_row (j + 1) (acc2 @ [Path H]) nb_nodes
          else if (List.mem (i - 1, j) square_coord || get_square acc (i - 1, j) = Some (Path V))
                  && test_3_squares_row acc width height (i - 1) j 0 nb_squares
          then
            create_row (j + 1) (acc2 @ [Path V]) nb_nodes
            (*if the square above is a node or a path(V) then we create a path(V)
              but only if we didn't have the maximum nodes in the column*)
          else create_row (j + 1) (acc2 @ [Wall]) nb_nodes
      in
      aux_init_board width height (i + 1) nb_squares diagonal (acc @ [create_row 0 [] 0])

(** Function with width height nb_squares and diagonal as arguments and creates a board with *)
let init_board (width : int) (height : int) (nb_squares : int) (diagonal : bool) : board =
    if match (width, height, nb_squares) with
       | 4, 4, _ -> true (*three men's morris like a tic-tac-toe; this version is with 2 squares and with diagonals*)
       | 8, 8, _ -> true (*six men's morris; this version is without diagonal and with 2 squares*)
       | 12, 12, _ ->
           true
           (*nine men's morris or twelve men's morris; classic size board but the normal one is without diagonal and with 3 squares*)
       | _, _, _ -> false
    then aux_init_board width height 0 nb_squares diagonal []
    else []

(**
  Function that returns the max number of pieces that we can place by each player depending of the template
  @param board : the template of the board
*)
let max_piece_from_template (board : template) : int =
    match board with
    | Three_mens_morris -> 3
    | Six_mens_morris -> 6
    | Nine_mens_morris -> 9
    | Twelve_mens_morris -> 9

(**
Function that init a board from a template
@param template : the template of the board
*)
let init_board_with_template (template : template) : board =
    match template with
    | Three_mens_morris -> init_board 4 4 2 true
    | Six_mens_morris -> init_board 8 8 2 false
    | Nine_mens_morris -> init_board 12 12 3 false
    | Twelve_mens_morris -> init_board 12 12 3 true

let board_length board = List.length board

let board_flatten (board : board) : square list = List.flatten board

let equals_board (board1 : board) (board2 : board) : bool =
    let rec compare l1 l2 =
        match (l1, l2) with
        | [], [] -> true
        | [], _ -> false
        | _, [] -> false
        | x :: xs, y :: ys -> (
            match (x, y) with
            | Empty, Empty -> compare xs ys
            | Wall, Wall -> compare xs ys
            | Path d, Path g when d = g -> compare xs ys
            | Color c1, Color c2 when c1 = c2 -> compare xs ys
            | _ -> false)
    in
    compare (List.flatten board1) (List.flatten board2)

let board_map_all (f : square -> square) (board : board) : board =
    List.map (fun line -> List.map (fun el -> f el) line) board

let for_all_board (f : square -> bool) (board : board) : bool =
    List.for_all (fun line -> List.for_all (fun el -> f el) line) board

let fill_all_node (template : template) (color : color) : board =
    let b = init_board_with_template template in
    board_map_all
      (fun x ->
        match x with
        | Empty -> Color color
        | _ -> x)
      b

(** Represent the number of pieces that you have to align to get a mill *)
let nb_to_get_mill = 3

(**This function return a player who has the same color that the color in argument*)
let get_player (game_update : game_update) (color : color) : player =
    match color with
    | White -> game_update.player1
    | Black -> game_update.player2

let reverse_color (c : color) : color =
    match c with
    | Black -> White
    | White -> Black

let get_opponent game_update color = get_player game_update (reverse_color color)

(**
  Function that return a none updated game
  This function is used when the move is not legit 
  @param game : the game state  
*)
let not_updated_game (game : game_update) : game_update =
    {
      board = game.board;
      mill = false;
      player1 = game.player1;
      player2 = game.player2;
      game_is_changed = false;
      max_pieces = game.max_pieces;
    }

(**
  Returns the coordinates of one step from a coordinate and a direction
  @param d : the direction
  @param ij : the coordinate
*)
let coordinates_from_directions d (i, j) =
    match d with
    | Up -> (i - 1, j)
    | Down -> (i + 1, j)
    | Right -> (i, j + 1)
    | Left -> (i, j - 1)
    | Up_right -> (i - 1, j + 1)
    | Up_left -> (i - 1, j - 1)
    | Down_right -> (i + 1, j + 1)
    | Down_left -> (i + 1, j - 1)

(**
  Function that returns the path that we have to have from a direction : example : if we want to go up, we have to have a path(V) (vertical path)
  @param d : the direction    
*)
let path_to_have_from_direction d =
    match d with
    | Up | Down -> Path V
    | Right | Left -> Path H
    | Up_right | Down_left -> Path DR
    | Up_left | Down_right -> Path DL

(**
  Function to get coordinates of a node from another node and a direction. 
  Return None if there is no node in this direction OR if the first step is not a legit Path regarding the direction
  @param board : the board
  @param ij : the coordinate of the departure node
  @param d : the direction of the node that we want to get
*)
let node_from_direction (board : board) ((i, j) : coordinates) (d : direction_deplacement) : coordinates option =
    let rec go_to (board : board) ((x, y) : coordinates) (d : direction_deplacement) : coordinates option =
        let coord_bis = coordinates_from_directions d (x, y) in
        let case = get_square board coord_bis in
        if case = Some (path_to_have_from_direction d)
        then go_to board coord_bis d
        else
          match case with
          | Some Empty | Some (Color _) -> Some coord_bis
          | _ -> None
    in
    if get_square board (coordinates_from_directions d (i, j)) = Some (path_to_have_from_direction d)
    then go_to board (i, j) d
    else None

(**
  Function that check if there is a mill from a certain position (i,j)
  @param board : the board
  @param ij : the coordinate where we want to check if there is a mill
  @param color : the color of the player that wants to check if there is a mill
*)
let check_mill_from_position (board : board) ((i, j) : coordinates) (color : color) : bool =
    match get_square board (i, j) with
    | Some (Color c) when c = color ->
        let rec count_from_dir (x, y) d =
            match node_from_direction board (x, y) d with
            | Some (a, b) -> if get_square board (a, b) = Some (Color color) then 1 + count_from_dir (a, b) d else 0
            | _ -> 0
        in
        let count_row = count_from_dir (i, j) Right + count_from_dir (i, j) Left in
        let count_col = count_from_dir (i, j) Up + count_from_dir (i, j) Down in
        let count_diag1 = count_from_dir (i, j) Up_right + count_from_dir (i, j) Down_left in
        let count_diag2 = count_from_dir (i, j) Up_left + count_from_dir (i, j) Down_right in
        1 + max (max count_row count_col) (max count_diag1 count_diag2) >= nb_to_get_mill
    | _ -> false

(**
  THIS FUNCTION HAS TO BE PRIVATE :
  Function that put a piece on the board at the coordinate (i,j) and return the new board
  If the position is not legit for a piece, return the old state of the game with the old board
*)
let place_piece_on_board (board : board) ((i, j) : coordinates) color : got_mill =
    let board = board_map (fun x -> if x = Empty then Color color else x) board (i, j) in
    let check = check_mill_from_position board (i, j) color in
    (board, check)

(**
  Function that put a start piece on the board at the coordinate (i,j) and return the new game state
  If the position is not legit for a piece, return the old state of the game
  @param game : the game state
  @param ij : the coordinates where we want to place the piece
  @param color : the color of the player that wants to place the piece
*)
let place_start_piece (game : game_update) ((i, j) : coordinates) (color : color) : game_update =
    if (get_player game color).piece_placed < game.max_pieces && (get_player game color).phase = Placing
    then
      let concerned_player = if game.player1.color = color then game.player1 else game.player2 in
      if get_square game.board (i, j) = Some Empty && concerned_player.piece_placed < game.max_pieces
      then
        let board, isMill = place_piece_on_board game.board (i, j) color in
        let updated_player =
            {
              phase = concerned_player.phase;
              color = concerned_player.color;
              piece_placed = concerned_player.piece_placed + 1;
              nb_pieces_on_board = concerned_player.nb_pieces_on_board + 1;
              bag = concerned_player.bag @ [(i, j)];
            }
        in
        if color = game.player1.color
        then
          {
            board;
            mill = isMill;
            player1 = updated_player;
            player2 = game.player2;
            game_is_changed = true;
            max_pieces = game.max_pieces;
          }
        else
          {
            board;
            mill = isMill;
            player1 = game.player1;
            player2 = updated_player;
            game_is_changed = true;
            max_pieces = game.max_pieces;
          }
      else
        {
          board = game.board;
          mill = false;
          player1 = game.player1;
          player2 = game.player2;
          game_is_changed = false;
          max_pieces = game.max_pieces;
        }
    else not_updated_game game

(**
  THIS FUNCTION HAS TO BE PRIVATE :
  This function remove a piece from the board and returns it
  Return an unchanged board if there is no piece in (i,j) 
  @param board : the board
  @param (i,j) : the coordinates of the piece to remove
  @param color : the color of the player that gets his piece eliminated (AND NOT THE COLOR OF THE PLAYER THAT ELIMINATES THE PIECE)
*)
let remove_from_board (board : board) ((i, j) : coordinates) color : board =
    board_map (fun x -> if x = Color color then Empty else x) board (i, j)

(**
  This function eliminate a piece from the board and returns the new game state.
  Return an unchanged game if there is no piece in (i,j) or if the piece is not from the player "color"
  By convention, we set the attribute "isMill" to false because we know that there is no mill after an elimination
  @param game : the game state
  @param ij : the coordinates of the piece to eliminate
  @param color : the color of the player that gets his piece eliminated (AND NOT THE COLOR OF THE PLAYER THAT ELIMINATES THE PIECE)
  @return the new game state (with the piece eliminated if the move is legit, else, return the old game state)
*)
let eliminate_piece (game : game_update) ((i, j) : coordinates) (color : color) : game_update =
    match get_square game.board (i, j) with
    | Some (Color c) when c = color ->
        (* We remove the piece and apply the changes for the bag of the concerned player *)
        let concerned_player = get_player game c in
        let new_bag = List.filter (fun (x, y) -> (x, y) <> (i, j)) concerned_player.bag in
        let new_board = remove_from_board game.board (i, j) c in
        let updated_player =
            {
              phase = concerned_player.phase;
              color = concerned_player.color;
              piece_placed = concerned_player.piece_placed;
              nb_pieces_on_board = concerned_player.nb_pieces_on_board - 1;
              bag = new_bag;
            }
        in
        if c = game.player1.color
        then
          {
            board = new_board;
            mill = false;
            player1 = updated_player;
            player2 = game.player2;
            game_is_changed = true;
            max_pieces = game.max_pieces;
          }
        else
          {
            board = new_board;
            mill = false;
            player1 = game.player1;
            player2 = updated_player;
            game_is_changed = true;
            max_pieces = game.max_pieces;
          }
    | _ -> not_updated_game game (* If the piece doesn't exist in (i,j), we do nothing *)

(**
  This function moves a piece from (i1,j1) to (i2,j2)
  Return the changed board if the move is legal, else, return the unchanged board
  @param game : the game state
  @param i1j1 : the coordinate of the piece to move
  @param i2j2 : the coordinate of the destination
  @param color : the color of the player that wants to move the piece
*)
let move_to_coordinates (game : game_update) ((i1, j1) : coordinates) ((i2, j2) : coordinates) (color : color) :
    game_update =
    let arrive = get_square game.board (i2, j2) in
    let depart = get_square game.board (i1, j1) in
    if arrive = Some Empty && depart = Some (Color color)
    then
      let concerned_player = if game.player1.color = color then game.player1 else game.player2 in
      let sub = remove_from_board game.board (i1, j1) concerned_player.color in
      let new_bag = List.map (fun (x, y) -> if (x, y) = (i1, j1) then (i2, j2) else (x, y)) concerned_player.bag in
      let new_board, isMill = place_piece_on_board sub (i2, j2) concerned_player.color in
      let new_player =
          {
            phase = concerned_player.phase;
            color = concerned_player.color;
            piece_placed = concerned_player.piece_placed;
            nb_pieces_on_board = concerned_player.nb_pieces_on_board;
            bag = new_bag;
          }
      in
      if color = game.player1.color
      then
        {
          board = new_board;
          mill = isMill;
          player1 = new_player;
          player2 = game.player2;
          game_is_changed = true;
          max_pieces = game.max_pieces;
        }
      else
        {
          board = new_board;
          mill = isMill;
          player1 = game.player1;
          player2 = new_player;
          game_is_changed = true;
          max_pieces = game.max_pieces;
        }
    else not_updated_game game

(**
  Function that move a piece from the coordinate (i,j) to a certain direction only if there is a Path in this direction
  Return the changed board if the move is legal, else, return the unchanged board
  @param game : the game state
  @param (i,j) : the coordinate of the piece to move
  @param d : the direction of the move
  @param color : the color of the player that wants to move the piece
*)
let move_to_direction (game : game_update) ((i, j) : coordinates) (d : direction_deplacement) (color : color) :
    game_update =
    match node_from_direction game.board (i, j) d with
    | Some (a, b) -> (
        match get_square game.board (a, b) with
        | Some Empty -> move_to_coordinates game (i, j) (a, b) color
        | _ -> not_updated_game game)
    | _ -> not_updated_game game

(**
  Function to get a list of possible moves from a coordinate (i,j)
  @param game : the game state
  @param (i,j) : the coordinate of the piece
  @param player : the color of the player that wants to move the piece
*)
let possible_moves_directions (game : game_update) ((i, j) : coordinates) (player : color) : direction_deplacement list
    =
    let rec aux (game : game_update) ((i, j) : coordinates) (player : color) (acc : direction_deplacement list) :
        direction_deplacement list =
        match acc with
        | [] -> []
        | x :: xs -> (
            let destination = node_from_direction game.board (i, j) x in
            match destination with
            | Some (a, b) -> (
                match get_square game.board (a, b) with
                | Some Empty -> x :: aux game (i, j) player xs
                | _ -> aux game (i, j) player xs)
            | _ -> aux game (i, j) player xs)
    in
    aux game (i, j) player [Up; Down; Right; Left; Up_right; Up_left; Down_right; Down_left]

(**
    This function return a bool if the player can't move
    @param player : the player
    @param game : the game
*)
let cant_move_piece_on_board (player : player) (game : game_update) : bool =
    let rec aux (player : player) (game : game_update) (bag : coordinates list) : bool =
        match bag with
        | [] -> true
        | (x, y) :: xs -> List.length (possible_moves_directions game (x, y) player.color) = 0 && aux player game xs
    in
    aux player game player.bag

(**
    Function that return a bool if the player lost
    @param game : the game
    @param player : the player    
*)
let lost (game : game_update) (player : player) : bool =
    match player.phase with
    | Moving -> cant_move_piece_on_board player game
    | _ -> player.nb_pieces_on_board <= 2 && player.piece_placed = game.max_pieces

(**
    This function init a player based on a color
    @param c : the color of the player   
*)
let init_player (c : color) : player =
    { phase = Placing; color = c; bag = []; piece_placed = 0; nb_pieces_on_board = 0 }

(**
  Private function which update the phase of a player if necessary
  @param player the player to update    
  @param max_pieces the maximum number of pieces that a player can place on the board
*)
let update_player_phase player max_pieces =
    if player.piece_placed = max_pieces && player.nb_pieces_on_board = 3
    then
      {
        phase = Flying;
        color = player.color;
        piece_placed = player.piece_placed;
        nb_pieces_on_board = player.nb_pieces_on_board;
        bag = player.bag;
      }
    else
      match player.phase with
      | Placing ->
          if player.piece_placed = max_pieces (* if the player has placed all of his pieces, he can start moving them *)
          then
            {
              phase = Moving;
              color = player.color;
              piece_placed = player.piece_placed;
              nb_pieces_on_board = player.nb_pieces_on_board;
              bag = player.bag;
            }
          else player (* else, no changes *)
      | Moving ->
          if player.nb_pieces_on_board = 3 (* if the player has only 3 pieces left, he can start flying them *)
          then
            {
              phase = Flying;
              color = player.color;
              piece_placed = player.piece_placed;
              nb_pieces_on_board = player.nb_pieces_on_board;
              bag = player.bag;
            }
          else player (* else, no changes *)
      | Flying -> player (* if the player is already flying, no changes *)

(**
Private function that update the phase of both players in the game_update
@param game_update the game_update to update    
*)
let update_phase (game_update : game_update) : game_update =
    {
      board = game_update.board;
      mill = game_update.mill;
      player1 = update_player_phase game_update.player1 game_update.max_pieces;
      player2 = update_player_phase game_update.player2 game_update.max_pieces;
      game_is_changed = game_update.game_is_changed;
      max_pieces = game_update.max_pieces;
    }

let distance (xa, ya) (xb, yb) : int =
    int_of_float (sqrt ((float_of_int (xb - xa) ** 2.) +. (float_of_int (yb - ya) ** 2.)))

(**Return coordinates for the developer from coordinates from the user in any board you can create with the function "initBoard width ..."
    or return -1 if this isn't possible*)
let traductor ((i, j) : coordinates) (width : int) : coordinates =
    if (j > 2 && width = 4)
       || (j > (width / 2) - 1 && width != 4 && i = width / 4)
       || (j > 2 && width != 4 && i != width / 4)
       || j < 0
       || i < 0
       || i > width / 2
    then (-1, -1)
    else if i = width / 4
    then if j < width / 4 || width = 4 then (i * 2, j * 2) else (i * 2, (j * 2) + 2)
    else
      let curSquare =
          min
            (min (distance (0, 0) (i, j)) (distance (0, 2) (i, j)))
            (min (distance (width / 2, 0) (i, j)) (distance (width / 2, 2) (i, j)))
      in
      (i * 2, (curSquare * 2) + (j * 2 * ((width / 4) - curSquare)))
(*width/2 is the distance between the middle and the origin and when we divide by 2 this means the maximal number of squares we can put in the board *)
(*2*(maxSquare-curSquare) is the number of cell we jump when we increment the ordinate from the concerned square and we add to cursSquare*2 because it is the start
  of the first cell of the square (each square is separated by 2)*)
(*private function*)

let init_game_update (template : template) : game_update =
    {
      board = init_board_with_template template;
      mill = false;
      player1 = init_player White;
      player2 = init_player Black;
      game_is_changed = false;
      max_pieces = max_piece_from_template template;
    }

let get_board (game_update : game_update) : board = game_update.board

let get_is_mill (game_update : game_update) : bool = game_update.mill

let get_board_is_changed (game_update : game_update) : bool = game_update.game_is_changed

let get_player_1 (game_update : game_update) : player = game_update.player1

let get_player_2 (game_update : game_update) : player = game_update.player2

let get_max_pieces (game_update : game_update) : int = game_update.max_pieces

let get_all_free_positions (game_update : game_update) : coordinates list =
    let rec aux (board : board) (acc : coordinates list) (i : int) (j : int) : coordinates list =
        if i >= board_length board
        then acc
        else if j >= board_length (List.nth board 0)
        then aux board acc (i + 1) 0
        else
          match get_square board (i, j) with
          | Some Empty -> aux board (acc @ [(i, j)]) i (j + 1)
          | _ -> aux board acc i (j + 1)
    in
    aux game_update.board [] 0 0

(**
  Function to apply a move in a game_update
  @param game_update : the game state
  @param player : the player that wants to apply the move
  @param move : the move to apply
*)
let apply (game_update : game_update) (player : player) (action : action) : game_update =
    match (action, player.phase) with
    | Placing c, Placing ->
        let newGU = place_start_piece game_update c player.color in
        update_phase newGU
    | Moving (c, dir), Moving ->
        let newGU = move_to_direction game_update c dir player.color in
        update_phase newGU
    | Flying (c1, c2), Flying ->
        let newGU = move_to_coordinates game_update c1 c2 player.color in
        update_phase newGU
    | _ -> not_updated_game game_update
