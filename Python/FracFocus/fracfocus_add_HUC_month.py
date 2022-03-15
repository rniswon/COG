# -*- coding: utf-8 -*-
"""
Created on Tue Mar 15 10:22:10 2022

@author: galanter
"""

import os
import pandas as pd
#%%

os.getcwd()


#%% bring in registry_1
reg_1 = pd.read_csv(os.path.join('data-raw','registryupload_1.csv'))

#%% bring in HUC data set

#%% join reg_1 with HUC dataset