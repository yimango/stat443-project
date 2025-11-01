import csv
from datetime import datetime
import os

def parse_date(date_str):
    """Parse date string in MM/DD/YYYY format and return YYYY-MM-DD format."""
    try:
        if isinstance(date_str, str) and date_str.strip():
            # Try MM/DD/YYYY format
            date_obj = datetime.strptime(date_str.strip(), '%m/%d/%Y')
            return date_obj.strftime('%Y-%m-%d')
    except:
        return None
    return None

def is_valid_value(val):
    """Check if a value is valid (not NA, not zero, not '#N/A N/A')."""
    if val is None:
        return False
    
    val_str = str(val).strip()
    
    # Check for NA indicators
    if val_str.upper() in ['#N/A N/A', '#N/A', 'N/A', 'NA', '', 'NAN', 'NONE']:
        return False
    
    # Try to convert to float to check if it's zero
    try:
        float_val = float(val_str)
        return float_val != 0
    except (ValueError, TypeError):
        return False

def process_csv_file(input_file, output_dir='output'):
    """
    Process the input CSV file and split it into separate CSV files for each ticker.
    Each output CSV will only contain columns (fields) where all values are non-NA and non-zero.
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    with open(input_file, 'r', encoding='utf-8') as f:
        reader = csv.reader(f)
        rows = list(reader)
    
    # Row structure:
    # Row 0: Start Date
    # Row 1: End Date
    # Row 2: Empty
    # Row 3 (index 3): Ticker names (SPY US Equity, VIX Index, etc.)
    # Row 4 (index 4): Column headers (Dates, PX_LAST, PX_HIGH, etc.)
    # Row 5+ (index 5+): Data rows
    
    if len(rows) < 5:
        print(f"Error: File has fewer than 5 rows. Cannot process.")
        return []
    
    ticker_row = rows[3]  # Row with ticker names
    header_row = rows[4]  # Row with column headers
    data_rows = rows[5:]  # All data rows
    
    # Identify tickers and their column ranges
    # Each ticker has 6 data columns + 1 empty separator = 7 columns
    tickers = []
    current_col = 0
    
    while current_col < len(ticker_row):
        ticker_name = ticker_row[current_col] if current_col < len(ticker_row) else ''
        
        # Skip empty columns
        if not ticker_name or ticker_name.strip() == '':
            current_col += 1
            continue
        
        # Extract ticker name and clean it for filename
        ticker_clean = ticker_name.strip().replace(' ', '_').replace('/', '_').replace('.', '_')
        
        # Get column indices for this ticker (6 columns: Dates, PX_LAST, PX_HIGH, PX_LOW, OPEN, VOLUME)
        ticker_data_cols = list(range(current_col, min(current_col + 6, len(header_row))))
        
        if len(ticker_data_cols) < 6:
            current_col += 7
            continue
        
        # Column names for this ticker
        column_names = ['Dates'] + [header_row[i] if i < len(header_row) else '' for i in ticker_data_cols[1:]]
        
        # Extract and process data for this ticker
        ticker_data = []
        
        for row in data_rows:
            # Ensure row has enough columns
            if len(row) <= max(ticker_data_cols):
                continue
            
            # Extract the 6 columns for this ticker
            date_val = row[ticker_data_cols[0]] if ticker_data_cols[0] < len(row) else ''
            
            # Parse and format date
            formatted_date = parse_date(date_val)
            if not formatted_date:
                continue
            
            # Extract all data values
            row_data = {
                'Dates': formatted_date,
                'PX_LAST': row[ticker_data_cols[1]] if ticker_data_cols[1] < len(row) else '',
                'PX_HIGH': row[ticker_data_cols[2]] if ticker_data_cols[2] < len(row) else '',
                'PX_LOW': row[ticker_data_cols[3]] if ticker_data_cols[3] < len(row) else '',
                'OPEN': row[ticker_data_cols[4]] if ticker_data_cols[4] < len(row) else '',
                'VOLUME': row[ticker_data_cols[5]] if ticker_data_cols[5] < len(row) else ''
            }
            
            ticker_data.append(row_data)
        
        # Determine which columns to keep (only those where ALL values are valid)
        data_columns = ['PX_LAST', 'PX_HIGH', 'PX_LOW', 'OPEN', 'VOLUME']
        valid_columns = ['Dates']  # Always include Dates
        
        for col in data_columns:
            # Check if all values in this column are valid
            if all(is_valid_value(row_data[col]) for row_data in ticker_data):
                valid_columns.append(col)
        
        # Filter data to only include valid columns
        filtered_data = []
        for row_data in ticker_data:
            filtered_row = [row_data[col] for col in valid_columns]
            filtered_data.append(filtered_row)
        
        # Sort by date
        date_idx = 0
        filtered_data.sort(key=lambda x: x[date_idx])
        
        # Save to CSV if there's valid data
        if len(filtered_data) > 0 and len(valid_columns) > 1:  # Must have at least Dates + one data column
            output_file = os.path.join(output_dir, f'{ticker_clean}.csv')
            with open(output_file, 'w', newline='', encoding='utf-8') as f:
                writer = csv.writer(f)
                # Write header
                writer.writerow(valid_columns)
                # Write data
                writer.writerows(filtered_data)
            
            print(f"Saved {len(filtered_data)} rows for {ticker_name} to {output_file}")
            print(f"  Columns included: {', '.join(valid_columns)}")
            tickers.append(ticker_name)
        
        # Move to next ticker (skip the empty separator column)
        current_col += 7
    
    print(f"\nProcessed {len(tickers)} tickers:")
    for ticker in tickers:
        print(f"  - {ticker}")
    
    return tickers

if __name__ == '__main__':
    input_file = 'Stat443Data.csv'
    output_dir = 'r_ready_data'
    
    print(f"Processing {input_file}...")
    print(f"Output directory: {output_dir}\n")
    
    process_csv_file(input_file, output_dir)
    
    print(f"\nDone! All files saved to '{output_dir}' directory.")
