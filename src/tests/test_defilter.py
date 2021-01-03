import os
import sys
currentdir = os.path.dirname(os.path.realpath(__file__))
parentdir = os.path.dirname(os.path.dirname(currentdir))
sys.path.append(parentdir)

import src.defilter_data as dd
import numpy as np
import pandas as pd

# Tests that a sinusoid can be reconstructed from its rolling sum and initial values
def test_sin_with_prev():
    time = np.arange(0, 12*6)/12
    truth = np.sin(time*2) + 1
    cumTruth = pd.Series(truth).rolling(window=12).sum().values
    out = dd.defilter(cumTruth[11:], 12, truth[:11])

    # Returns same shape as truth
    assert out.shape == truth.shape

    # Returns same values as truth
    np.testing.assert_allclose(truth, out)
