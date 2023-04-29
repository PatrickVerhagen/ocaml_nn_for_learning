[@@@ocaml.warning "-32"]
open Matrix


  
    
(*end
let () = Random.self_init ()*)


(* Helper function to generate random dimensions *)
let random_dimensions () =
  let random_min = 1 in
  let random_max = 10 in
  (random_min + (Random.int (random_max - random_min)), random_min + (Random.int (random_max - random_min)))


let () =
  (* Generate random dimensions *)
  let rows, cols = random_dimensions () in

  (* Create a random matrix with random dimensions *)
  match Matrix.create_random rows cols with
  | Error _ ->
      print_endline "Error creating matrix"
  | Ok matrix ->
      print_endline "Original Matrix:";
      Matrix.print matrix;
      (* Transpose the matrix *)
      let transposed_matrix = Matrix.transpose matrix in
      print_endline "Transposed Matrix:";
      Matrix.print transposed_matrix


