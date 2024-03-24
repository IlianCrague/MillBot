open Mill.Arena
open Mill.Engine
open Mill.My_player

let () = Random.self_init ()

let randomSeed n = Random.int n

let play_n_games pp blancs noirs (n : int) : int * int =
    let rec aux (n : int) (result : int * int) : int * int =
        let print_stats () =
            if pp
            then
              Format.printf "%d : %f\n"
                (fst result + snd result)
                ((fst result |> float_of_int) /. ((fst result |> float_of_int) +. (snd result |> float_of_int)) *. 100.);
            Format.print_flush ()
        in
        if n <= 1
        then result
        else if n mod 2 = 0
        then
          let end_game = arena blancs noirs Nine_mens_morris in
          if end_game.winner.color = White
          then (
            print_stats ();
            aux (n - 1) (fst result + 1, snd result))
          else (
            print_stats ();
            aux (n - 1) (fst result, snd result + 1))
        else
          let end_game = arena noirs blancs Nine_mens_morris in
          if end_game.winner.color = Black
          then (
            print_stats ();
            aux (n - 1) (fst result + 1, snd result))
          else (
            print_stats ();
            aux (n - 1) (fst result, snd result + 1))
    in
    aux n (1, 1)

let () =
    let _ = play_n_games true (my_player randomSeed) (player_random randomSeed) 10000 in
    ()
(*let a = arena (my_player randomSeed) player_human Nine_mens_morris in
  pretty_print_board (get_board a.game)*)
