import numpy as np


# "unrolls" data with a k-windowed rolling sum (right-aligned)
def unroll_sum(
        A: np.ndarray, # k-window rolling sum data
        k: int, # rolloing sum window size
        prev_data: np.ndarray = None # k-1 sized previous unrolled data
    ):
    if k <= 1:
        raise ValueError('k must be greater than 1')
    if np.isnan(np.sum(A)):
        raise ValueError('NaN Found in input data')

    # If no "truth" data set first k-1 points to first summed point/k
    if prev_data is not None:
        if np.isnan(np.sum(prev_data)):
            raise ValueError('NaN Found in prev_data')
        if prev_data.shape[0] != k-1:
            raise ValueError('Prevous data must be k-1 in length')
    else:
        prev_data = np.zeros(k) + A[0]/k

    D = np.append(prev_data, np.zeros(A.shape[0]))
    offset = k - 1
    for i in (np.arange(A.shape[0]) + offset):
        # Note i starts at k-1
        prev_sum = D[i-k+1:i].sum()
        D[i] = A[i-offset] - prev_sum
    return D