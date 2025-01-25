
(* Helper function to split a string by comma *)
let split_by_comma str =
  String.split_on_char ',' str

(* entry point of the program *)
let () =
  (* Read the entire file *)
  let ic = open_in "historical_prices.csv" in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (split_by_comma line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in

  let all_rows = read_lines [] in
  
  (* Get headers (first row) *)
  let headers = match all_rows with
    | h :: _ -> List.tl h  (* Skip the date column *)
    | [] -> []
  in
  
  (* Get data rows (skip header row) *)
  let data_rows = match all_rows with
    | _ :: rest -> rest
    | [] -> []
  in

  (* Get first and last dates *)
  let dates = match (List.hd all_rows, List.hd (List.rev all_rows)) with
    | (first :: _, last :: _) -> (first, last)
    | _ -> ("", "")
  in

  (* Transpose data to get columns and skip date column *)
  let stock_data = 
    List.map List.tl data_rows  (* Remove date column *)
    |> Linalg.transpose
  in


  (* Print some information *)
  Printf.printf "Date range: %s to %s\n" (fst dates) (snd dates);
  Printf.printf "Number of stocks: %d\n" (List.length headers);
  List.iter2 (fun header data -> 
    Printf.printf "Stock %s has %d data points\n" header (List.length data)
  ) headers stock_data;

  (* Convert string lists to float lists and calculate variance *)
  Printf.printf "\nVariances:\n";
  List.iter2 (fun header data -> 
    try
      let float_data = List.map float_of_string data in
      let var = Linalg.variance float_data in
      let mean = Linalg.mean float_data in
      Printf.printf "Variance of %s: %f ... mean of: %s: %f\n" header var header mean
    with 
    | Failure _ -> Printf.printf "Could not calculate variance for %s (invalid number format)\n" header
    | e -> Printf.printf "Error calculating variance for %s: %s\n" header (Printexc.to_string e)
  ) headers stock_data;
  
    (* Convert all stock data to floats once *)
    let float_data = List.map (fun stock_prices -> List.map float_of_string stock_prices) stock_data in
    
    (* Calculate returns once *)
    let returns = List.map Lib.get_returns float_data in
    let e_returns = List.map Linalg.mean returns in
    Printf.printf "\nReturns:";
    Linalg.print_vec e_returns headers;

    (* Calculate covariance matrix once *)
    Printf.printf "\nCovariance Matrix:\n";
    let cov_matrix = Linalg.covariance_matrix stock_data in
    Linalg.print_mat cov_matrix headers;

    Printf.printf "\nInverse Covariance Matrix:\n";
    let cov_mat_inv = Linalg.inverse cov_matrix in
    Linalg.print_mat cov_mat_inv headers;

    (* Calculate minimum risk portfolio *)
    Printf.printf "\nOptimal Portfolio:";
    let cmi = Lib.min_risk_portfolio cov_matrix in
    let sigma_2 = Lib.x_E_x cmi cov_matrix in
    Printf.printf "\nsigma^2 = %f" sigma_2; 
    Printf.printf "\nsigma = %f" (sqrt sigma_2);
    Linalg.print_vec cmi headers;

    (* Calculate portfolio with expected return *)
    let e = 0.0001 in
    Printf.printf "\nOptimal Portfolio with expected return E = %f" e;
    let result = Lib.min_risk_portfolio_e cov_matrix e_returns e in
    let sigma_2 = Lib.x_E_x result cov_matrix in
    Printf.printf "\nsigma^2 = %f" sigma_2; 
    Printf.printf "\nsigma = %f" (sqrt sigma_2);
    Linalg.print_vec result headers;

    (*With risk free rate Rf = 0.0001 *) 
    (*tangent portfolio*)
    let r_f = 0.0001 in
    Printf.printf "\nOptimal Portfolio with risk free rate = %f" r_f;
    let portfolio_risk_free = Lib.min_risk_portfolio_risk_free_asset cov_matrix e_returns r_f in 
    let sigma_2 = Lib.x_E_x portfolio_risk_free cov_matrix in
    Printf.printf "\nsigma^2 = %f" sigma_2; 
    Printf.printf "\nsigma = %f" (sqrt sigma_2);
    Linalg.print_vec portfolio_risk_free headers;

    (*for expected return AND risk free rate, look at the capital allocation line*)

    (*Single index model*)
    let portfolio_single_index = Lib.single_index_portfolio headers returns  in
    Printf.printf "\nSingle Index Model:";
    Linalg.print_vec portfolio_single_index headers;

    (*regress each stock on SPX *)


    (*Multi index model*)

    (*multi group model? might be annoying to implement*)
