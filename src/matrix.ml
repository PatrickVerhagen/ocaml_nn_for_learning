[@@@ocaml.warning "-32"]
module Matrix : sig
  type t
  type error = InvalidDimensions
  val create_with_scalar : int -> int -> float -> (t, error) result
  val create_random : int -> int ->  (t, error) result
  val create_ones : int -> int -> (t, error) result
  val create_zeros : int -> int -> (t, error) result
  val get : t -> int -> int -> float
  val set : t -> int -> int -> float -> unit
  val shape : t -> (int * int)
  val transpose : t -> t
  val print : t -> unit
  end = struct
  type t = float array array
  type error = InvalidDimensions

  let check_dimensions rows cols =
    if rows <= 0 || cols <= 0 then
      Error InvalidDimensions
    else
      Ok ()

  let consistent_dimensions mat =
    let cols = Array.length mat.(0) in
    Array.for_all (fun row -> Array.length row = cols) mat

  let create_with_scalar rows cols (scalar : float): (t, error) result =
    match check_dimensions rows cols with
    | Error e -> Error e
    | Ok () -> Ok (Array.init rows (fun _ -> Array.make cols scalar))
    
  let create_zeros rows cols =
    create_with_scalar rows cols 0.00
    
  let create_ones rows cols =
    create_with_scalar rows cols 1.00
    
  let create_random rows cols =
    create_with_scalar rows cols (Random.float 1.0)
    

  let get mat row col = mat.(row).(col)

  let set mat row col value = mat.(row).(col) <- value

  let shape mat = (Array.length mat, Array.length mat.(0))
  
  let shape_same mat_a mat_b : bool = 
    let (a_row_len, a_col_len) = shape mat_a in
    let (b_row_len, b_col_len) = shape mat_b in
    a_row_len = b_row_len && a_col_len = b_col_len

    
    

    
  let print mat =
    let print_float_array arr =
      Array.iter (fun x -> Printf.printf "%f " x) arr
    in
    Array.iter (fun x -> print_float_array x; print_newline ()) mat


  let elementwise_generic (op : float -> float -> float) (mat_a : t) (mat_b : t) : t =
    Array.mapi (fun  i row -> Array.mapi(fun i_2 item -> op item mat_b.(i).(i_2) ) row ) mat_a 
  
  let elementwise_addition (mat_a : t) (mat_b : t) : t = 
    elementwise_generic ( +. ) mat_a mat_b
  
  let elementwise_subtraction (mat_a : t) (mat_b : t) : t = 
    elementwise_generic ( -. ) mat_a mat_b
    
  let scalar_multiplication (matrix : t) (scalar : float) : t =
    Array.map  (fun row -> Array.map (fun item ->  item *. scalar ) row ) matrix
    
  let transpose (matrix: t) : t =
    let (number_of_rows, number_of_cols) = shape matrix in
    Array.init number_of_cols (fun col_i -> Array.init number_of_rows (fun row_i -> matrix.(row_i).(col_i)))
end