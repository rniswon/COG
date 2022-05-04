#!/usr/bin/env python
# coding: utf-8

# ***frac focus data work - COG Water Use Permian Basin - jmcdowell@usgs.gov - 2020***
# ** AEG made a few modifications to preserve month
#%%
# Import required packages
import os
import glob
import pandas as pd
import numpy as np
#%% set up directory 
# do what you have to do to get into 'data-raw' 
# os.chdir('data-raw')

print('current working directory:',os.getcwd())


#%% bring in the frac focus data 

ext = 'csv'
all_filenames = [i for i in glob.glob('*.{}'.format(ext))]


#combine all files
combined_csv = pd.concat([pd.read_csv(f, low_memory=False) for f in all_filenames])

pd.options.display.max_columns = None
combined_csv

#FF file access date: 11/30/21

#%% county codes

#list of state codes in study area
tx = [42]
nm = [30]

#bringing in county codes and tables
tx_county = [3,17,33,43,79,81,95,101,103,105,107,109,115,125,135,137,141,151,153,165,169,173,189,219,227,229,235,243,253,263,
             265,267,269,271,279,301,303,305,307,317,327,329,335,345,353,371,377,383,385,389,413,415,431,433,435,443,445,451,
             461,463,465,475,495,501]

#%% filtering, merging, combining. 
tx_county_table = pd.read_csv("study_counties_tx.csv")
nm_county = [5,9,11,15,25,27,35,41]
nm_county_table = pd.read_csv("study_counties_nm.csv")

#filter by API state code 42 for TX
df_tx = combined_csv[(combined_csv.StateNumber.isin(tx))]
df_tx.reset_index(drop=True, inplace=True)

#filter by API county codes for TX in study area
df_tx_2 = df_tx[(df_tx.CountyNumber.isin(tx_county))]
df_tx_2.reset_index(drop=True, inplace=True)

#add a new column for County, using a vlookup-type function to convert county number to a county name
#this will ensure accurate county names, unlike the CountyName column, which has typos aplenty
df_tx_3 = pd.merge(df_tx_2, 
                    tx_county_table, 
                    on='CountyNumber',
                    how='left')

#filter by API state code 30 for NM
df_nm = combined_csv[(combined_csv.StateNumber.isin(nm))]
df_nm.reset_index(drop=True, inplace=True)

#filter by API county codes for NM in study area
df_nm_2 = df_nm[(df_nm.CountyNumber.isin(nm_county))]
df_nm_2.reset_index(drop=True, inplace=True)

#add a new column for County, using a vlookup-type function to convert county number to a county name
#this will ensure accurate county names, unlike the CountyName column, which has typos aplenty
df_nm_3 = pd.merge(df_nm_2, 
                    nm_county_table, 
                    on='CountyNumber',
                    how='left')
df_nm_3.reset_index(drop=True, inplace=True)

#merge tx and nm data
combine = [df_nm_3, df_tx_3]
ff_tx_nm = pd.concat(combine)
ff_tx_nm.reset_index(drop=True, inplace=True)

#%% filtering data, cleaning up, prepping for deduplication - this results in a full record of treatments

#remove rows with NaN in the TotalBaseWaterVolume column
ff_tx_nm_2 = ff_tx_nm.dropna(how='any', subset=['TotalBaseWaterVolume'])
ff_tx_nm_2.reset_index(drop=True, inplace=True)

#need to convert StateNumber to StateName
ff_tx_nm_3 = ff_tx_nm_2
ff_tx_nm_3['State'] = np.where(ff_tx_nm_2['StateNumber']>35, 'Texas', 'New Mexico')
ff_tx_nm_4 = ff_tx_nm_3.drop(['StateNumber'], axis=1)
ff_tx_nm_4.reset_index(drop=True, inplace=True)
#%%
# AEG modified this cell to use pd.datetime and to preserve month
ff_tx_nm_4['job_end_date'] = pd.to_datetime(ff_tx_nm_4['JobEndDate']).dt.date
ff_tx_nm_4.reset_index(drop=True, inplace=True)

#%% AEG removed some of these lines that weren't necessary after using datetime
# also retained 2020

#cleanup
ff_tx_nm_5 = ff_tx_nm_4.drop(['JobEndDate'], axis=1)
ff_tx_nm_5 = ff_tx_nm_5[['APINumber', 
                        'State',
                        'COUNTY',
                        'Latitude',
                        'Longitude',
                        'job_end_date',
                        'TotalBaseWaterVolume',
                        'TVD']]
ff_tx_nm_5.rename(columns={'COUNTY': 'County'}, inplace=True)
ff_tx_nm_5.reset_index(drop=True, inplace=True)

#this is the final version before messing with duplicates or removing any entries 20200602
ff_tx_nm_5.info()


#%% should revisit the deduplication and potentially merge jeremy's new script?

# In[14]:


#merge full and dedup counts
#to be used later in notebook to compare to IHS data

well_trt_counts = pd.merge(counties_dedup,counties, how='left')


# In[44]:


#list of individual wells by county from FF database
counties_dedup.head(10)


# In[ ]:





# ***This cell results in a list of unique wells, with VolSum as summed water volumes for all records for that APINumber***

# In[65]:


#turns out this ff_tx_nm_7 is wayyyy overestimating water volumes when compared to IHS (because of using sum instead of mean)
# 
ff_tx_nm_7 = ff_tx_nm_5
ff_tx_nm_7['VolSum'] = ff_tx_nm_5.groupby(['APINumber'])['TotalBaseWaterVolume'].transform('sum')
ff_tx_nm_7['VolMean'] = ff_tx_nm_5.groupby(['APINumber'])['TotalBaseWaterVolume'].transform('mean')
ff_tx_nm_7 = ff_tx_nm_7.drop_duplicates(subset=['APINumber'], keep='first')
ff_tx_nm_7.reset_index(drop=True, inplace=True)
ff_tx_nm_7['delta'] = ff_tx_nm_7['TotalBaseWaterVolume'] - ff_tx_nm_7['VolMean']
ff_tx_nm_7['sum_to_total'] = (ff_tx_nm_7['VolSum']) / (ff_tx_nm_7['TotalBaseWaterVolume'])
ff_tx_nm_7.describe()


# ***this is the important df here, after this some minimal processing was done such as lat/long cleanup/verification in arc***

# In[64]:


ff_tx_nm_5.to_csv(os.path.join('..','data','ff_tx_nm_withstats.csv'))




