import os
import sys
currentdir = os.path.dirname(os.path.realpath(__file__))
parentdir = os.path.dirname(os.path.dirname(currentdir))
sys.path.append(parentdir)

import src.estimate_error as ee
import src.load_data as ld
import src.defilter_data as dd
import numpy as np
import pandas as pd


# Test on tiny data when errors should not be equal for different months out
def test_small_data():
    month_error_data = pd.DataFrame.from_dict({
        'timestamp': np.arange(4),
        'location': ['t'] * 4,
        'predicted_val': 3 + np.arange(4),
        'Deaths': np.arange(4)
    })
    out = ee.calculate_error(month_error_data, k=2, max_months_out=2)

    # Hand calculated expected values
    expected_error = np.array([-3, -2, 1, 1])
    print(out)
    np.testing.assert_array_equal(out.error.values, expected_error)


# Test on 3 month sum when errors should not be equal for different months out
def test_3m_data():
    month_error_data = pd.DataFrame.from_dict({
        'timestamp': np.arange(7),
        'location': ['t'] * 7,
        'predicted_val': 3 + np.arange(7),
        'Deaths': np.arange(7)
    })
    out = ee.calculate_error(month_error_data, k=3, max_months_out=3)

    # Hand calculated expected value for timestamp 4
    expected_p4 = np.array([2, 2, 2])
    np.testing.assert_array_equal(
        out[out.pred_timestamp == 4].error,
        expected_p4
    )


# No error if exact match of monthly to cumulative data
def test_no_error():
    no_error_data = pd.DataFrame.from_dict({
        'timestamp': np.arange(20),
        'location': ['t'] * 20,
        'predicted_val': [12] * 20,
        'Deaths': [1] * 20
    })
    out = ee.calculate_error(no_error_data)
    assert np.sum(out.error) == 0

# Error non-zero if random values given for monthly data
def test_with_error():
    error_data = pd.DataFrame.from_dict({
        'timestamp': np.arange(20),
        'location': ['t'] * 20,
        'predicted_val': [12] * 20,
        'Deaths': np.random.rand(20)
    })
    out = ee.calculate_error(error_data)
    print(out)
    assert np.sum(out.error) != 0


# Tests that the predicted value in the error estimate occurs at the same date as expected
def test_pred_same():
    data = ld.load_data()
    error_est = ee.calculate_error(data)

    k = 12 # 12 month rolling sum
    time_filt = data[data.timestamp >= pd.to_datetime('2018-02-01')]
    time_filt['raw_predicted_val'] = np.nan
    for loc in time_filt['location'].unique():
        truth = time_filt.loc[time_filt.location == loc, 'Deaths'].values
        times = time_filt.loc[time_filt.location == loc, 'timestamp'].values
        pred = dd.defilter(time_filt.loc[data.location == loc, 'predicted_val'].iloc[11:].values, k, truth[:11])
        time_filt.loc[time_filt.location == loc, 'raw_predicted_val'] = pred
        for m in np.arange(5) + 1:
            pred_m = pred[10 + m]
            ee_m = error_est[
                (error_est.location == loc) &
                (error_est.month_out == m) &
                (error_est.pred_timestamp == times[10 + m])
            ].pred.values
            print(times[m+10], loc, pred_m, ee_m)
            assert ee_m.shape[0] == 1
            assert ee_m[0] == pred_m
