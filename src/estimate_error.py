import src.defilter_data as dd
import pandas as pd
import numpy as np


def calculate_error(
    data: pd.DataFrame,
    k: int = 12,
    max_months_out: int = 6
) -> pd.DataFrame:
    req_cols = ['timestamp', 'location', 'predicted_val', 'Deaths']
    if data.columns.to_list() != req_cols:
        print(data.columns.to_list())
        raise ValueError('Data frame does not contain correct columns: ' + ','.join(req_cols))

    error_data = {
        'location': [],
        'pred_timestamp': [],
        'truth': [],
        'error': [],
        'pred': [],
        'percent_error': [],
        'month_out': []
    }
    timepoints = data['timestamp'].unique()
    timepoints.sort()

    # Iterate through data with at least 16 months ahead
    for ts in timepoints[:-(k - 2 + max_months_out)]:
        time_filt = data.loc[data.timestamp >= ts]

        # Calculate predictions for each state
        for loc in data.location.unique():
            prev_truth = time_filt.loc[
                data.location == loc, 'Deaths'
                ].values
            estimates = dd.unroll_sum(
                    time_filt.loc[
                        time_filt.location == loc, 'predicted_val'
                    ].iloc[k-1:].values, k, prev_truth[:k-1]
                )
            # Predictions and errors at different months out
            for month_out in np.arange(max_months_out) + 1:
                pred_index = k - 2 + month_out

                pred = estimates[pred_index]
                truth = prev_truth[pred_index]

                error = truth - pred
                percent_error = 100 * np.abs(error/truth)
                pred_time = time_filt.loc[
                    time_filt.location == loc, 'timestamp'
                ].values[pred_index]

                # Add to error data dictionary
                error_data['location'].append(loc)
                error_data['pred_timestamp'].append(pred_time)
                error_data['truth'].append(truth)
                error_data['error'].append(error)
                error_data['pred'].append(pred)
                error_data['month_out'].append(month_out)
                error_data['percent_error'].append(percent_error)

    return pd.DataFrame.from_dict(
        error_data
        ).sort_values(
        by=['location', 'pred_timestamp', 'month_out']
        )
