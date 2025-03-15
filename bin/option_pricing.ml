(* Helper function to split a string by comma *)
let split_by_comma str =
    String.split_on_char ',' str

let rec get_log_returns l = 
    match l with
    | [] | [_] -> []
    | a :: b :: rest -> 
        (log (b /. a)) :: get_log_returns (b :: rest)

let binom n k =
  if k > n then 0
  else
      let k = min k (n - k) in  (* Use symmetry to reduce the number of multiplications *)
      let rec loop i acc =
          if i > k then acc
      else loop (i + 1) (acc * (n - i + 1) / i)
      in
    loop 1 1


let normal_cdf z =
  (* Constants for Abramowitz and Stegun approximation (1964) *)
  let a1 = 0.254829592 in
  let a2 = -0.284496736 in
  let a3 = 1.421413741 in
  let a4 = -1.453152027 in
  let a5 = 1.061405429 in
  let p = 0.3275911 in
  
  (* Take absolute value of z *)
  let abs_z = abs_float z in
  
  (* Calculate t *)
  let t = 1.0 /. (1.0 +. p *. abs_z) in
  
  (* Calculate erf approximation *)
  let erf = 1.0 -. ((((a5 *. t +. a4) *. t +. a3) *. t +. a2) *. t +. a1) *. t *. exp (-.abs_z *. abs_z) in
  
  (* Adjust for sign of z *)
  if z >= 0.0 then 0.5 +. 0.5 *. erf
  else 0.5 -. 0.5 *. erf

let put_from_call c e r s0 = 
    c -. s0 +. e /. (1.0 +. r)

let get_bsm_price r t strike_price stock_data =  
    let last_price = List.hd (List.rev stock_data) in
    let daily_volatility = get_log_returns stock_data |> Linalg.variance |> sqrt in
    let annual_volatility = daily_volatility *. sqrt 252.0 in
    let d_1 = (log ( last_price /. strike_price) +. (r +. 0.5 *. annual_volatility ** 2.0) *. t) /. (annual_volatility *. sqrt (t)) in 
    let d_2 = d_1 -. annual_volatility *. sqrt ( t ) in
    last_price *. normal_cdf (d_1) -. strike_price /. exp ( r *. t) *. normal_cdf (d_2)


let get_option_price r n strike_price stock_data = 
  let n = float_of_int n in
  let last_price = List.hd (List.rev stock_data) in 
  let daily_volatility = get_log_returns stock_data |> Linalg.variance |> sqrt in
  let annual_volatility = daily_volatility *. sqrt 252.0 in
  (* start with 73 days .... 1/5 of year or 0.2. And using five periods in that time frame *)
  let u = exp ( annual_volatility *. sqrt(0.2 /. n) ) in
  let d = 1.0 /. u in
  (* going to use the rate for 1/5 of a year, and then use 5 periods *)
  let rp = (1.0 +. r) ** (1.0 /. 5.0 /. n) -. 1.0 in
  let p = (1.0 +. rp -. d) /. (u -. d) in 
  let pp = p *. u /. (1.0 +. rp) in
  let k = log ( strike_price /. (d ** n *. last_price) ) /. log ( u /. d) in   (* max iterations needed *)
  let start = ceil k in 

  let rec loop i acc = 
      if i >= n then acc
      else 
      let binom_i = float_of_int (binom (int_of_float n) (int_of_float i)) in 
      let pp_coef = pp ** i *. (1.0 -. pp) ** (n -. i) in
      let p_coef = p ** i *. (1.0 -. p) ** (n -. i) in
      let term = last_price *. binom_i *. pp_coef -. (strike_price /. ((1.0 +. rp) ** n)) *. binom_i *. p_coef
      in
      loop (i +. 1.0) (acc +. term)
  in
  loop start 0.0



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

  let r = 0.05 in
  let t = 0.2 in 
  let n = 5 in 

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

  let float_data = List.map (fun stock_prices -> List.map float_of_string stock_prices) stock_data in

  Printf.printf "Date range: %s to %s\n" (fst dates) (snd dates);
  
  Printf.printf "Headers: %s\n" (String.concat ", " headers);

  let latest_prices = List.map (fun f -> (List.hd (List.rev f))) float_data in

  (* Calculate strike prices *)
  let strike_prices = List.map (fun x -> x +. 2.0) latest_prices in
  
  (* Calculate minimum call prices *)
  let minimum_calls = List.map2 (fun price strike -> max 0.0 (price -. strike /. (1.0 +. r))) latest_prices strike_prices in
  let minimum_puts = List.map2 (fun price strike -> max 0.0 (strike /. (1.0 +. r) -. price)) latest_prices strike_prices in

  let maximum_calls = latest_prices in 
  let maximum_puts = List.map( fun x -> x /. (1.0 +. r)) strike_prices in 
  
  let binomial_call_prices = List.map2 ( fun x y -> get_option_price r n y x ) float_data strike_prices in 

  let binomial_put_prices = List.map2 (fun (c, e) s0 -> put_from_call c e r s0) 
                   (List.combine binomial_call_prices strike_prices) 
                   latest_prices in 

  let bsm_call_prices = List.map2 ( fun s stock -> get_bsm_price r t s stock) strike_prices float_data in
  let bsm_put_prices = List.map2 (fun (c, e) s0 -> put_from_call c e r s0) (List.combine bsm_call_prices strike_prices) latest_prices in 

  Lib.print_metadata r t n;
  Lib.print_calls headers latest_prices strike_prices minimum_calls maximum_calls binomial_call_prices bsm_call_prices;
  Lib.print_puts headers latest_prices strike_prices minimum_puts maximum_puts binomial_put_prices bsm_put_prices;


