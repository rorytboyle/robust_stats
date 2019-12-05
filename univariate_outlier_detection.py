"""
Robust univariate outlier detection
@author: Rory Boyle @rorytboyle@gmail.com
@date: 05/12/2019
"""
import pandas as pd
import numpy as np
import statsmodels.robust as robust

def make_boolean_column(col):
    """
    Creates a boolean column where True=value that is identifed as an outlier
    using MAD criterion
    
    :param col: df containing single col from df containing data with upper and
    lower rejection thresholds appended.
    :return bool_col: df with single Boolean col containing True in cases
    containing identified outliers and False elsewhere.
    """
    # make boolean for upper threshold
    upper_bool = col > col.loc['upper']
    lower_bool = col < col.loc['lower']
    
    # set values to false if not an outlier
    bool_col = col.where(upper_bool | lower_bool, False)
    
    # if not false, replace with true (i.e. set outliers to True)
    bool_col.where(bool_col==False, True ,inplace=True)
    
    return bool_col

def replace_outliers(col):
    """ 
    Replaces identified outliers with the median +/- (t * Median Absolute
    Deviation) where t = threshold as specific in detect_univariate_outliers
    
    :param col: df containing single col from df containing data with upper and
    lower rejection thresholds appended.
    :return col: df with single col containing data where outliers 
    have been replaced with next most extreme values   
    """
    # replace outliers above upper threshold with the upper threshold value
    # (i.e. the median + (t * MAD))
    upperThresh = col.loc['upper'].item()
    col.where(col <= upperThresh, upperThresh, inplace=True)
    
    # replace outliers below lower threshold with the lower threshold value
    # (i.e. the median - (t * MAD))
    lowerThresh = col.loc['lower'].item()    
    col.where(col >= lowerThresh, lowerThresh, inplace=True)
    
    return col
    
def detect_univariate_outliers(df, threshold=2.5, replace=True):
    """
    Detects outliers in a dataframe and returns a boolean dataframe where True
    = outlier, and False = not an outlier. Detects outliers based on criterion
    of median +/- (t * Median Absolute Deviation) where t = a specified 
    threshold. Also returns a 'winsorized' dataframe where outliers are
    replaced with a value = median +/- (t * Median Absolute Deviation) 

    :param df: pandas df containing data to be checked for outliers
    :param threshold: rejection threshold. Default = 2.5 (Leys et al., 2013). 
    2.5 constitutes a moderately conservative threshold. Other thresholds = 3 
    (very conservative), 2 = (poorly conservative). 
    https://doi.org/10.1016/j.jesp.2013.03.013
    :param replace: Boolean stating whether detected outliers should be 
    replaced. If True, detected outliers will be replaced with next most
    extreme values that do not exceed rejection threshold.
    :return outlier_df: Boolean df containing True in cases containing 
    identified outliers and False elsewhere.
    :return replaced: df containing data with outliers replaced (only returned
    if replace=True)
    """
    # Calculate MAD for each variable p
    mad_values = robust.scale.mad(df)
    
    # Get MAD for each column
    mad = pd.DataFrame(mad_values.reshape(-1, len(mad_values)),
                       columns=df.columns, index=['mad'])
    
    # Get median and add to mad dataframe
    median = df.median().rename('median')
    mad = mad.append(median)
    
    # Calculate upper and lower rejection thresholds
    upper=[mad.loc['median', col] + (threshold * mad.loc['mad', col]) for
           col in mad.columns]
    
    lower=[mad.loc['median', col] - (threshold * mad.loc['mad', col]) for
           col in mad.columns]
    
    # append thresholds to df
    rej_thresholds = pd.DataFrame([upper, lower], columns=mad.columns, 
                                  index=['upper', 'lower'])
    mad = mad.append(rej_thresholds)
    
    # check for outliers - create boolean Df
        ## merge mad with df so that df can call apply function - save df as 
        ## temporary df
    temp_df = df.append(mad.loc[['upper', 'lower'],:])
    outlier_df = temp_df.apply(make_boolean_column).drop(['upper', 'lower'])
    
    # replace outliers with median +/- (threshold * MAD) 
    if replace:
        replaced_df = temp_df.apply(replace_outliers).drop(['upper', 'lower'])
        return outlier_df, replaced_df
    else:
        return outlier_df
