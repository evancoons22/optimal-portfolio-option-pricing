let mean l = 
    match l with
    | [] -> failwith "Need to pass a list" 
    | _ -> 
        let sum = List.fold_left (+.) 0.0 l in
        sum /. float_of_int (List.length l)

let variance col = 
    match col with
    | [] -> failwith "Need to pass a list"
    | col -> 
      let mu = mean col in
      let n = float_of_int (List.length col) in
      let sum_sq_diff = List.fold_left (fun acc x -> acc +. (x -. mu)*.(x -. mu)) 0.0 col in
      sum_sq_diff /. (n -. 1.0)  (* Using n-1 for sample variance *)

(* Function to transpose a list of lists *)
let transpose list_of_lists =
  match list_of_lists with
  | [] -> []
  | [] :: _ -> []
  | first :: _ ->
      let num_cols = List.length first in
      List.init num_cols
      (fun col ->
        List.map 
        (fun row -> 
          List.nth row col
        ) list_of_lists
      )

(* Helper to calculate covariance between two lists *)
let covariance_pair l1 l2 =
  if List.length l1 <> List.length l2 then
    failwith "Lists must be of equal length"
  else
    let mu1 = mean l1 in
    let mu2 = mean l2 in
    let n = float_of_int (List.length l1) in
    let sum = List.fold_left2 (fun acc x y -> 
      acc +. ((x -. mu1) *. (y -. mu2))
    ) 0.0 l1 l2 in
    sum /. (n -. 1.0)

(* Create covariance matrix from list of lists *)
let covariance_matrix ll =
  let n = List.length ll in
  let float_lists = List.map (List.map float_of_string) ll in
  List.init n (fun i ->
    let list_i = List.nth float_lists i in
    List.init n (fun j ->
      let list_j = List.nth float_lists j in
      if i = j then
        variance list_i  (* Diagonal elements are variances *)
      else
        covariance_pair list_i list_j  (* Off-diagonal elements are covariances *)
    )
  )

let print_vec v headers = 
  try
    Printf.printf "%-10s" "";  (* Empty corner cell *)
    Printf.printf "\n";
    List.iter2 (fun x header ->
      Printf.printf "%-10s" header;
      Printf.printf "%-10.4f" x;
      Printf.printf "\n"
    ) v headers
  with
    | e -> Printf.printf "Error printing vector: %s\n" (Printexc.to_string e)

let print_mat cov_matrix headers = 
  (* Print covariance matrix *)
  try
    (* let cov_matrix = covariance_matrix stock_data in *)
    Printf.printf "%-10s" "";  (* Empty corner cell *)
    List.iter (fun header -> Printf.printf "%-10s" header) headers;
    Printf.printf "\n";
    List.iter2 (fun row header ->
      Printf.printf "%-10s" header;
      List.iter (fun x -> Printf.printf "%-10.4f" x) row;
      Printf.printf "\n"
    ) cov_matrix headers
  with
    | e -> Printf.printf "Error calculating covariance matrix: %s\n" (Printexc.to_string e)


let dot_product l1 l2 = 
    match l1, l2 with
    | [], [] -> 0.0
    | [], _ | _, [] -> failwith "Lists must be of equal length"
    | _ -> List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 l1 l2


let mat_vec_mul a b = 
    match a, b with
    | [], _ | _, [] -> []
    | _, _ ->
        List.map (fun row ->
            dot_product row b
        ) a

let mat_mul a b = 
    match a, b with
    | [], _ | _, [] -> []
    | _, _ ->
        let b_t = transpose b in
        match b_t with
        | [] -> []
        | _ -> 
            List.map (fun row_a -> 
                List.map (fun col_b -> 
                    dot_product row_a col_b
                ) b_t
            ) a

(* Helper to get a specific element from matrix *)
let get_element matrix i j =
  List.nth (List.nth matrix i) j

(* Helper to set element in a list at index *)
let set_element lst index value =
  List.mapi (fun i x -> if i = index then value else x) lst

(* Helper to set row in matrix *)
let set_row matrix row_idx new_row =
  List.mapi (fun i row -> if i = row_idx then new_row else row) matrix

(* Helper to swap rows in matrix *)
let swap_rows matrix i j =
  let row_i = List.nth matrix i in
  let row_j = List.nth matrix j in
  set_row (set_row matrix i row_j) j row_i

let inverse a =
  let n = List.length a in
  (* Create augmented matrix [A|I] *)
  let identity = List.init n (fun i ->
    List.init n (fun j -> if i = j then 1.0 else 0.0)
  ) in
  let augmented = List.map2 (fun row id_row ->
    row @ id_row
  ) a identity in

  (* Gauss-Jordan elimination *)
  let rec eliminate matrix i =
    if i >= n then matrix
    else
      (* Find pivot *)
      let pivot_val = get_element matrix i i in
      if abs_float pivot_val < 1e-10 then
        (* Find non-zero pivot *)
        let rec find_pivot j =
          if j >= n then failwith "Matrix is singular"
          else if abs_float (get_element matrix j i) > 1e-10 then
            eliminate (swap_rows matrix i j) i
          else find_pivot (j + 1)
        in
        find_pivot (i + 1)
      else
        (* Normalize row i *)
        let row_i = List.nth matrix i in
        let normalized_row = List.map (fun x -> x /. pivot_val) row_i in
        let matrix = set_row matrix i normalized_row in
        
        (* Eliminate column i from other rows *)
        let matrix = List.mapi (fun j row ->
          if j <> i then
            let factor = get_element matrix j i in
            List.mapi (fun k x ->
              x -. factor *. (get_element matrix i k)
            ) row
          else row
        ) matrix in
        
        eliminate matrix (i + 1)
  in
  
  try
    let result = eliminate augmented 0 in
    (* Extract right half of augmented matrix (the inverse) *)
    List.map (fun row ->
      List.filteri (fun i _ -> i >= n) row
    ) result
  with
    | Failure msg -> failwith ("Matrix inversion failed: " ^ msg)
    | _ -> failwith "Matrix inversion failed"
