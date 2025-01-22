
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
  
    (* First convert strings to floats, then calculate returns *)
    let returns = List.map (fun stock_prices -> 
        let float_prices = List.map float_of_string stock_prices in
        Lib.get_returns float_prices
    ) stock_data in
    let e_returns = List.map Linalg.mean returns in
    Printf.printf "\nReturns:";
    Linalg.print_vec e_returns  headers;

    Printf.printf "\nCovariance Matrix:\n";
    let cov_matrix = Linalg.covariance_matrix stock_data  in
    Linalg.print_mat cov_matrix headers;

    Printf.printf "\nInverse Covariance Matrix:\n";
    let cov_mat_inv = Linalg.inverse cov_matrix in
    Linalg.print_mat cov_mat_inv headers;

    Printf.printf "\nOptimal Portfolio:";
    let cmi = Lib.min_risk_portfolio cov_matrix in
    Linalg.print_vec cmi headers;

