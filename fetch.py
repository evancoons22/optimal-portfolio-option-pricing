import yfinance as yf
import pandas as pd

def download_data(tickers, start_date="2020-01-01", end_date="2024-12-31"):
    # Dictionary to hold the dataframes
    data_frames = []
    
    for ticker in tickers:
        print(f"Downloading data for {ticker}...")
        data = yf.download(ticker, start=start_date, end=end_date)[['Close']]
        data.columns = [ticker]  # Rename column to the ticker symbol
        data_frames.append(data)
    
    # Merge all dataframes on the date index
    combined_data = pd.concat(data_frames, axis=1)
    combined_data.index.name = "Date"  # Set the index name to Date
    combined_data.reset_index(inplace=True)  # Move Date index to a column

    # Save to CSV
    output_filename = "op/historical_prices.csv"
    combined_data.to_csv(output_filename, index=False)
    print(f"Data saved to {output_filename}")

# List of tickers
tickers = ["AAPL", "SPY", "GOOGL"]

# Download and save the data
download_data(tickers)

