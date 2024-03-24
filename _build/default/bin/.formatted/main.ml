open Mill.Arena
open Mill.Engine
open Mill.My_player

let () = Random.self_init ()

let randomSeed n = Random.int n

let play_n_games pp blancs noirs (n : int) : int * int =
    let total_time = ref 0. in
    let rec aux (n : int) (result : int * int) : int * int =
        let print_stats t =
            if pp
            then
              if fst result + snd result = 0
              then ()
              else
                Format.fprintf Format.err_formatter "%d : %f | time : %f\n"
                  (fst result + snd result)
                  ((fst result |> float_of_int)
                  /. ((fst result |> float_of_int) +. (snd result |> float_of_int))
                  *. 100.)
                  t;
            Format.pp_print_flush Format.err_formatter ()
        in
        if n <= 0
        then (
          Format.printf "average time : %f\n" (!total_time /. 50.);
          result)
        else
          let time = Unix.time () in
          if n mod 2 = 0
          then (
            let end_game = arena blancs noirs Nine_mens_morris in
            let t = Unix.time () -. time in
            total_time := !total_time +. t;
            print_stats t;
            if end_game.winner.color = White
            then aux (n - 1) (fst result + 1, snd result)
            else aux (n - 1) (fst result, snd result + 1))
          else
            let end_game = arena noirs blancs Nine_mens_morris in
            let t = Unix.time () -. time in
            total_time := !total_time +. t;
            print_stats t;
            if end_game.winner.color = Black
            then aux (n - 1) (fst result + 1, snd result)
            else aux (n - 1) (fst result, snd result + 1)
    in
    aux n (0, 0)

let () =
    let _ = play_n_games true (my_player_minmax (4, randomSeed)) (player_random randomSeed) 50 in
    ()
