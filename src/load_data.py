import pandas as pd


def load_data():
    # Load files, convert timestamps
    gt = pd.read_csv('output/ground_truth_states_imputation.csv')\
        .rename(columns={'State_Name': 'location'})
    gt['timestamp'] = pd.to_datetime(gt['Month Code'], format='%Y/%m')
    cdc_overdose = pd.read_csv('input/CDC_ts.csv')
    cdc_overdose['timestamp'] = pd.to_datetime(
        cdc_overdose.end_date,
        format='%m/%d/%Y'
    )

    # Merge NYC with New York State
    nyc = cdc_overdose.loc[
        cdc_overdose.location == 'New York City', ['timestamp', 'predicted_val']
        ].rename(columns={'predicted_val': 'nyc'})
    nyc_merged = cdc_overdose.merge(nyc, how='left', validate='m:1')
    nyc_merged.loc[nyc_merged.location == 'New York', 'predicted_val'] = \
        nyc_merged[nyc_merged.location == 'New York']['predicted_val'] + \
        nyc_merged[nyc_merged.location == 'New York']['nyc']
    nyc_merged = nyc_merged.loc[nyc_merged.location != 'New York City', [
        'timestamp',
        'location',
        'predicted_val'
    ]]

    # Merge with ground truth month by month data
    data = nyc_merged.merge(
        gt[['timestamp', 'location', 'Deaths']],
        how='left',
        validate='1:1'
    )

    return data.sort_values(by=['location', 'timestamp'])
