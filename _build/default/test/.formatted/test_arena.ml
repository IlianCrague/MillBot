open Mill.Arena
open Mill.Engine
open Utils

(*TEST*)

let test_config_end_game =
    let open QCheck in
    Test.make ~count:100 ~name:"for all game : one of players can't move or has two pieces left" small_int (fun _ ->
        let randomSeed n =
            Random.self_init ();
            Random.int n
        in
        let player1 = player_random randomSeed in
        let player2 = player_random randomSeed in
        let end_game = arena player1 player2 Nine_mens_morris in
        cant_move_piece_on_board end_game.loser end_game.game || end_game.loser.nb_pieces_on_board <= 2)

(**This test check that with the same seed, we will get the same end*)
let testSeed =
    let open QCheck in
    Test.make ~count:10 ~name:"for all seed : END gamePlayWithSeed = END gamePlayWithSeed when both seeds are the same"
      small_int (fun x ->
        Random.init x;
        let randomSeed n = Random.int n in
        let player1 = player_random randomSeed in
        let player2 = player_random randomSeed in
        let game1 = arena player1 player2 Nine_mens_morris in
        Random.init x;
        let randomSeed n = Random.int n in
        let player1 = player_random randomSeed in
        let player2 = player_random randomSeed in
        let game2 = arena player1 player2 Nine_mens_morris in
        equals_end_game game1 game2)

let test_error_player =
    let open QCheck in
    Test.make ~count:1000 ~name:"for all game : the dumb bot will never finish the game"
      (pair small_int arbitrary_templates) (fun (x, template) ->
        Random.init x;
        let randomSeed n = Random.int n in
        let player1 = player_random_dumb randomSeed in
        let player2 = player_random_dumb randomSeed in
        try
          let _ = arena player1 player2 template in
          false
        with
        | Not_Allowed _ | Invalid_Strategy _ -> true
        | _ -> false)

let test_player_invalid_pos =
    let open Alcotest in
    [
      test_case "Invalid move" `Quick (fun () ->
          let wrapper_fn () =
              try ignore (arena (player_random Random.int) player_invalid_pos Twelve_mens_morris)
              with Invalid_Strategy _ | Not_Allowed _ -> raise (Failure "Expected exception")
          in
          Alcotest.(check_raises) "Expected exception" (Failure "Expected exception") wrapper_fn);
    ]

let () =
    let open Alcotest in
    run "TEST ARENA"
      [
        ("Test with Seed generate", [QCheck_alcotest.to_alcotest testSeed]);
        ("Test configuration end game", [QCheck_alcotest.to_alcotest test_config_end_game]);
        ("Test error player", [QCheck_alcotest.to_alcotest test_error_player]);
        ("player generates only invalide positions", test_player_invalid_pos);
      ]
