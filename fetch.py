import yfinance as yf
import pandas as pd
import argparse
import os


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

    # Create output directory if it doesn't exist
    output_dir = "data"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Created directory: {output_dir}")
    
    # Save to CSV
    output_filename = f"{output_dir}/historical_prices.csv"
    combined_data.to_csv(output_filename, index=False)
    print(f"Data saved to {output_filename}")

def parse_args():
    parser = argparse.ArgumentParser(description='Download historical stock data')
    parser.add_argument('--tickers', type=str, default="SPY,AAPL,GOOGL,NVDA",
                        help='Comma-separated list of ticker symbols (default: SPY,AAPL,GOOGL,NVDA)')
    parser.add_argument('--start', type=str, default="2020-01-01",
                        help='Start date in YYYY-MM-DD format (default: 2020-01-01)')
    parser.add_argument('--end', type=str, default="2024-12-31",
                        help='End date in YYYY-MM-DD format (default: 2024-12-31)')
    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()
    
    # Parse the comma-separated list of tickers
    tickers = [ticker.strip() for ticker in args.tickers.split(',')]
    
    print(f"Downloading data for tickers: {', '.join(tickers)}")
    print(f"Date range: {args.start} to {args.end}")
    
    # Download and save the data
    download_data(tickers, args.start, args.end)

