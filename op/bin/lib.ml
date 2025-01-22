
let rec get_returns l = 
    match l with
    | [] | [_] -> []
    | a :: b :: rest -> 
        ((b -. a) /. a) :: get_returns (b :: rest)  (* Calculate return and recurse on rest *)

let min_risk_portfolio cov_matrix = 
    try
        (* Check if matrix is empty *)
        if List.length cov_matrix = 0 then
            failwith "Empty covariance matrix";

        (* Calculate inverse *)
        let inv = 
            try Linalg.inverse cov_matrix
            with e -> failwith ("Matrix inversion failed: " ^ Printexc.to_string e)
        in

        (* Create ones vector *)
        let n = List.length (List.hd cov_matrix) in
        let ones = List.init n (fun _ -> 1.0) in

        (* Calculate numerator *)
        let numer = 
            try Linalg.mat_vec_mul inv ones
            with e -> failwith ("Numerator calculation failed: " ^ Printexc.to_string e)
        in

        (* Calculate denominator *)
        let denom = 
            try Linalg.dot_product numer ones
            with e -> failwith ("Denominator calculation failed: " ^ Printexc.to_string e)
        in

        (* Get scaling factor *)
        let c = 
            try denom
            with _ -> failwith "Failed to extract denominator value"
        in

        if abs_float c < 1e-10 then
            failwith "Denominator too close to zero";

        let weights = List.map (fun x -> x /. c) numer in
        
        (* Verify weights sum to approximately 1 *)
        let sum = List.fold_left (+.) 0.0 weights in
        
        if abs_float (sum -. 1.0) > 1e-6 then
            Printf.printf "Warning: Weights sum to %f (should be close to 1.0)\n" sum;

        weights

    with
    | Failure msg -> 
        Printf.printf "Error in portfolio optimization: %s\n" msg;
        raise (Failure msg)
    | e -> 
        Printf.printf "Unexpected error: %s\n" (Printexc.to_string e);
        raise e

