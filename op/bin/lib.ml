
let rec get_returns l = 
    match l with
    | [] | [_] -> []
    | a :: b :: rest -> 
        ((b -. a) /. a) :: get_returns (b :: rest)  (* Calculate return and recurse on rest *)

(* defining all helper functions *)
let fun_A cov returns = 
    match cov with
    | [] -> failwith "Empty covariance matrix"
    | _ -> 
        match returns with 
        | [] -> failwith "Empty returns vector"
        | _ -> 
            let n = List.length cov in
            let ones = List.init n (fun _ -> 1.0) in
            let invcov = Linalg.inverse cov in
            Linalg.dot_product (Linalg.mat_vec_mul invcov ones) returns

let fun_B cov returns = 
    match cov with
    | [] -> failwith "need covariance"
    | _ -> 
        match returns with 
        | [] -> failwith "need return" 
        | _ -> 
            let invcov = Linalg.inverse cov in
            Linalg.dot_product (Linalg.mat_vec_mul invcov returns) returns

let fun_C cov = 
    match cov with
    | [] -> failwith "need covariance"
    | _ -> 
        List.fold_left (fun acc row -> acc +. List.fold_left (+.) 0.0 row) 0.0 cov

let fun_lambda_1 a b c e = 
    (c *. e -. a) /. (b *. c -. a *. a)

let fun_lambda_2 a b c e = 
    (b -. a *. e) /. (b *. c -. a *. a)

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
        
let min_risk_portfolio_risk_free_asset cov_matrix returns r_f= 
    try
        let returns_risk_free = List.map (fun x -> x -. r_f) returns in

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
            try Linalg.mat_vec_mul inv returns_risk_free
            with e -> failwith ("Numerator calculation failed: " ^ Printexc.to_string e)
        in

        (* Calculate denominator *)
        let denom = 
            try Linalg.dot_product numer ones
            with e -> failwith ("Denominator calculation failed: " ^ Printexc.to_string e)
        in

        if abs_float denom < 1e-10 then
            failwith "Denominator too close to zero";

        let weights = List.map (fun x -> x /. denom) numer in
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

let x_E_x x e = 
    let r = Linalg.mat_vec_mul e x in 
    Linalg.dot_product r x

let min_risk_portfolio_e cov r e = 
    (* cov is covariance matrix, r is average return of each stock, e is expected return *)
    let a_const = fun_A cov r in
    let b_const = fun_B cov r in
    let c_const = fun_C cov in
    let l1 = fun_lambda_1 a_const b_const c_const e in 
    let l2 = fun_lambda_2 a_const b_const c_const e in 
    let n  = List.length r in
    let ones = List.init n (fun _ -> 1.0) in
    let result = List.map2  (fun x y -> x *. l1 +. y *. l2) ones r in
    Linalg.mat_vec_mul (Linalg.inverse cov) result


let regress ri rmarket = 
    (* this is one way to solve for the betas. we can also solve by doing cov(ri,rm) / var(rm), then solve for alpha = mean(ri) - betai * rm ;; *)
    (* First check if vectors have same length *)
    if List.length ri <> List.length rmarket then
        failwith "Return vectors must have same length";
    
    let n = List.length ri in
    if n < 2 then 
        failwith "Need at least 2 data points for regression";

    (* Create X matrix with ones column and market returns *)
    let x_matrix = List.map (fun rm -> [1.0; rm]) rmarket in
    
    (* Calculate X'X *)
    let xt_x = Linalg.mat_mul (Linalg.transpose x_matrix) x_matrix in
    
    (* Calculate X'y *)
    let xt_y = Linalg.mat_vec_mul (Linalg.transpose x_matrix) ri in
    
    (* Solve (X'X)^(-1)X'y to get [alpha; beta] *)
    let coeffs = Linalg.mat_vec_mul (Linalg.inverse xt_x) xt_y in
    
    match coeffs with
    | [alpha; beta] -> (alpha, beta)
    | _ -> failwith "Regression calculation failed"


let single_index_portfolio headers returns  = 
    (* Find index of SPY in headers *)
    let market_index = 
        try List.find_index (fun x -> x = "SPY") headers
        with Not_found -> failwith "\nSPY required. Not found in data." in
    
    (* Extract market returns *)
    let market_returns = match market_index with
        | Some idx -> List.map (fun row -> List.nth row idx) returns
        | None -> failwith "SPY required. Not found in data."
    in
    
    (* Calculate market variance *)
    let market_mean = List.fold_left (+.) 0.0 market_returns /. float (List.length market_returns) in
    let market_var = List.fold_left (fun acc x -> acc +. (x -. market_mean) ** 2.0) 0.0 market_returns 
                    /. float (List.length market_returns - 1) in
    
    (* For each stock, calculate beta and residual variance *)
    let n = List.length headers in
    let betas_and_vars = List.init n (fun i ->
        match market_index with
        | Some idx when i = idx -> (1.0, 0.0)  (* SPY has beta=1 and no residual variance *)
        | _ ->
            let stock_returns = List.map (fun row -> List.nth row i) returns in
            let (alpha, beta) = regress stock_returns market_returns in
            
            (* Calculate residual variance *)
            let predicted_returns = List.map (fun rm -> alpha +. beta *. rm) market_returns in
            let residuals = List.map2 (-.) stock_returns predicted_returns in
            let residual_var = List.fold_left (fun acc x -> acc +. x *. x) 0.0 residuals 
                             /. float (List.length residuals - 1) in
            (beta, residual_var)
    ) in
    
    (* Construct covariance matrix *)
    let cov_matrix = List.init n (fun i ->
        List.init n (fun j ->
            let (beta_i, var_i) = List.nth betas_and_vars i in
            let (beta_j, _) = List.nth betas_and_vars j in
            if i = j then
                (* Diagonal elements: beta_i^2 * market_var + residual_var *)
                beta_i *. beta_i *. market_var +. var_i
            else
                (* Off-diagonal elements: beta_i * beta_j * market_var *)
                beta_i *. beta_j *. market_var
        )
    ) in
    
    (* Use the existing min_risk_portfolio_e function with the constructed covariance matrix *)
    min_risk_portfolio cov_matrix


