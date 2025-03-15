
(* Helper function to split a string by comma *)
let split_by_comma str =
    String.split_on_char ',' str


    (* entry point of the program *)
let () =
    (* Read the entire file *)
    let ic = open_in "data/historical_prices.csv" in
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

  (* Get first and last dates from data rows (skip header row) *)
  let dates = match (List.hd data_rows, List.hd (List.rev data_rows)) with
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

  (* basic information, too verbose so commenting out *)
  (*
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
    *)

    (* Convert all stock data to floats once *)
    let float_data = List.map (fun stock_prices -> List.map float_of_string stock_prices) stock_data in

    (* Calculate all portfolios first *)
    let returns = List.map Lib.get_returns float_data in
    let e_returns = List.map Linalg.mean returns in
    let cov_matrix = Linalg.covariance_matrix stock_data in
    let cov_mat_inv = Linalg.inverse cov_matrix in

    (* Print first row of matrices *)
    Format.printf "@[<v 0>@,";
    Format.printf "@[<h>%s@]@," (String.make 120 '=');
    Format.printf "@[<h>Expected Returns, Covariance Matrix, and Inverse Covariance Matrix@]@,";
    Format.printf "@[<h>%s@]@," (String.make 120 '=');

    (* Three sections layout *)
    Format.printf "@[<v>";
    Format.printf "Expected Returns:@,";
    List.iter2 (fun h r -> Format.printf "%-10s: %8.4f@," h r) headers e_returns;
    Format.printf "@]@,";  (* Double newline for separation *)

    Format.printf "@[<v>";
    Format.printf "Covariance Matrix:@,";
    Lib.print_matrix_header headers;
    List.iteri (fun i row ->
        Lib.print_matrix_row (List.nth headers i) row
        ) cov_matrix;
    Format.printf "@]@,";  (* Double newline for separation *)

    Format.printf "@[<v>";
    Format.printf "Inverse Covariance Matrix:@,";
    Lib.print_matrix_header headers;
    List.iteri (fun i row ->
        Lib.print_matrix_row (List.nth headers i) row
        ) cov_mat_inv;
    Format.printf "@]@,";

    (* Calculate all portfolios *)
    let e = 0.0001 in
    let r_f = 0.0001 in
    let min_risk = Lib.min_risk_portfolio cov_matrix in
    let target_return = Lib.min_risk_portfolio_e cov_matrix e_returns e in
    let risk_free = Lib.min_risk_portfolio_risk_free_asset cov_matrix e_returns r_f in
    let single_index = Lib.single_index_portfolio headers returns in

    (* Print portfolio comparison table *)
    Format.printf "@[<v 0>@,";
    Format.printf "@[<h>%s@]@," (String.make 125 '=');
    Format.printf "@[<h>Portfolio Comparison@]@,";
    Format.printf "@[<h>%s@]@," (String.make 125 '=');

    let portfolio_names = [
      "Min Risk";
      Printf.sprintf "Target Return (%.4f)" e;
      Printf.sprintf "Risk Free (%.4f)" r_f;
      "Single Index"
    ] in
    let portfolios = [min_risk; target_return; risk_free; single_index] in
    Lib.print_portfolio_table headers portfolios portfolio_names;

    (* Print summary statistics for each portfolio *)
    Format.printf "@,Portfolio Statistics:@,";
    Format.printf "@[<v 0>";

    (* Header row *)
    Format.printf "%-22s |" "";  (* Empty cell for alignment *)
    List.iter (fun name -> Format.printf " %22s |" name) portfolio_names;
    Format.printf "@,";
    Format.printf "%s" (String.make (25 * (List.length portfolio_names + 1)) '-');
    Format.printf "@,";

    (* Get all summary stats *)
    let all_stats = List.map (fun p -> Lib.get_summary_stats p cov_matrix e_returns) portfolios in

    (* Print Expected Returns row *)
    Format.printf "%-22s |" "Expected Return";
    List.iter (fun (er, _, _) -> Format.printf " %22.4f |" er) all_stats;
    Format.printf "@,";

    (* Print Risk row *)
    Format.printf "%-22s |" "Risk";
    List.iter (fun (_, risk, _) -> Format.printf " %22.4f |" risk) all_stats;
    Format.printf "@,";

    (* Print Sharpe ratio row *)
    Format.printf "%-22s |" "Sharpe Ratio";
    List.iter (fun (_, _, sharpe) -> Format.printf " %22.4f |" sharpe) all_stats;
    Format.printf "@,";

    Format.printf "@]@."

    (*regress each stock on SPX *)

    (*Multi index model*)

    (*multi group model? might be annoying to implement*)
