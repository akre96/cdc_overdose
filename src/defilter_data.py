import numpy as np


# "unrolls" data with a k-windowed rolling sum (right-aligned)
def defilter(A: np.ndarray, k: int, prev_data: np.ndarray = None):
    if k <= 1:
        raise ValueError('k must be greater than 1')
    if np.isnan(np.sum(A)):
        raise ValueError('NaN Found in input data')

    if prev_data is not None:
        if np.isnan(np.sum(prev_data)):
            raise ValueError('NaN Found in prev_data')
        if prev_data.shape[0] != k-1:
            raise ValueError('Prevous data must be k-1 in length')
    else:
        prev_data = np.zeros(k) + A[0]/k

    D = np.append(prev_data, np.zeros(A.shape[0]))
    offset = prev_data.shape[0]
    for i in (np.arange(A.shape[0]) + offset):
        prev_sum = D[i-k+1:i].sum()
        D[i] = A[i-offset] - prev_sum
    return D
