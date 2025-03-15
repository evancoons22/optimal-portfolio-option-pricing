## Optimal Portfolio, Option Pricing
- this is an implementation of [Stats c183/c283 with Professor Christou](http://www.stat.ucla.edu/~nchristo/statistics_c183_c283/) at UCLA. 
- the first half of the class is on optimal portfolio selection and the second on options and pricing
- This is an implementation from scratch in Ocaml to learn functional programming and relearn concepts. 

### Running the program
- install python3, ocaml, dune (ocaml build system) 
- fetch stock data with default values, or specify tickers and dates: 

```
# default
python3 fetch.py

# with options
python3 fetch.py --tickers AAPL,SPY,NVDA,AMZN --start 2020-01-31 --end 2025-01-31
```

- run the ocaml program 

```
# optimal portfolio
dune exec op

# option pricing
dune exec option
```

### TODO 
#### Optimal Portfolio
- [X] Markowitz optimal portfolio calculation (no limitations)    
- [X] Optimal portfolio with expected return   
- [X] with risk free rate   
- [X] single index model   
- [ ] multi index model   
- [ ] constant correlation model    

#### Option Pricing 
- [X] binomial option pricing model
- [X] black scholes model

### example result 

#### optimal portfolio
```
=============================================================================================================================
Portfolio Comparison
=============================================================================================================================
Stock/Portfolio        |               Min Risk | Target Return (0.0001) |     Risk Free (0.0001) |           Single Index |
-----------------------------------------------------------------------------------------------------------------------------
AAPL                   |                 0.8876 |                13.8865 |                 0.6651 |                 0.1968 |
SPY                    |                -0.6639 |               -10.3870 |                -0.7473 |                -0.2752 |
NVDA                   |                 0.3704 |                 5.7955 |                 0.9052 |                 0.1689 |
AMZN                   |                 0.4059 |                 6.3500 |                 0.1770 |                 0.9096 |


Portfolio Statistics:
                       |               Min Risk | Target Return (0.0001) |     Risk Free (0.0001) |           Single Index |
-----------------------------------------------------------------------------------------------------------------------------
Expected Return        |                 0.0021 |                 0.0322 |                 0.0032 |                 0.0014 |
Risk                   |                13.6029 |               212.8186 |                17.0035 |                24.3147 |
Sharpe Ratio           |                 0.0002 |                 0.0002 |                 0.0002 |                 0.0001 |
```

#### option pricing

```
============================================================
Pricing Parameters
============================================================
Risk-Free Rate           :   5.0000%
Time to Expiration (yrs) :   0.2000 years
Number of Steps          :        5
------------------------------------------------------------

=================================================================================================================================================
Call Option Pricing
=================================================================================================================================================
Stock           |      Last Price |    Strike Price |       Min Price |       Max Price |  Binomial Price |   Black-Scholes |
-------------------------------------------------------------------------------------------------------------------------------------------------
AAPL            |        237.3291 |        239.3291 |          9.3966 |        237.3291 |         11.5511 |         19.1307 |
SPY             |        605.0400 |        607.0400 |         26.9067 |        605.0400 |         20.9091 |         34.1120 |
NVDA            |        124.6385 |        126.6385 |          4.0304 |        124.6385 |         10.0618 |         16.7395 |
AMZN            |        234.6400 |        236.6400 |          9.2686 |        234.6400 |         12.9382 |         21.3622 |
-------------------------------------------------------------------------------------------------------------------------------------------------

=================================================================================================================================================
Put Option Pricing
=================================================================================================================================================
Stock           |      Last Price |    Strike Price |       Min Price |       Max Price |  Binomial Price |   Black-Scholes |
-------------------------------------------------------------------------------------------------------------------------------------------------
AAPL            |        237.3291 |        239.3291 |          0.0000 |        227.9324 |          2.1544 |          9.7340 |
SPY             |        605.0400 |        607.0400 |          0.0000 |        578.1333 |         -5.9976 |          7.2053 |
NVDA            |        124.6385 |        126.6385 |          0.0000 |        120.6081 |          6.0314 |         12.7091 |
AMZN            |        234.6400 |        236.6400 |          0.0000 |        225.3714 |          3.6696 |         12.0936 |
-------------------------------------------------------------------------------------------------------------------------------------------------
```


