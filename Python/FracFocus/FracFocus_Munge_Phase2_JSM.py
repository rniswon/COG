#!/usr/bin/env python
# coding: utf-8

# ***frac focus data work - COG Water Use Permian Basin - jmcdowell@usgs.gov - 2020***

# In[5]:


import os
import glob
import pandas as pd
import string
import numpy as np
import matplotlib.pyplot as plt

#fracfocuscsv2 is the folder with the full list of 20 FF files, which fracfocuscsv_check is the 2 registry files
#downloaded 5-28-20 from FF website
os.chdir("D:/OneDrive - DOI/1. Projects/COG Water Use/COG_python/fracfocuscsv2/")
print('current working directory:',os.getcwd())


# ***bring in the frac focus data***

# In[2]:


ext = 'csv'
all_filenames = [i for i in glob.glob('*.{}'.format(ext))]


#combine all files
combined_csv = pd.concat([pd.read_csv(f, low_memory=False) for f in all_filenames])

pd.options.display.max_columns = None
combined_csv

#FF file access date: 05-28-2020


# In[ ]:





# ***Mitchell Co test case***

# In[3]:


#want to filter by Mitchell County
#mitchell = [335]
#mitch = df_tx[(df_tx.CountyNumber.isin(mitchell))]
#mitch.reset_index(drop=True, inplace=True)
#mitch2 = mitch.drop_duplicates(subset=['APINumber'], keep='first')
#mitch2.to_csv('mitchell_county_wells.csv')


# In[ ]:





# ***working with counties, and combining TX and NM data***

# In[4]:


#list of state codes in study area
tx = [42]
nm = [30]

#bringing in county codes and tables
tx_county = [3,17,33,43,79,81,95,101,103,105,107,109,115,125,135,137,141,151,153,165,169,173,189,219,227,229,235,243,253,263,
             265,267,269,271,279,301,303,305,307,317,327,329,335,345,353,371,377,383,385,389,413,415,431,433,435,443,445,451,
             461,463,465,475,495,501]
tx_county_table = pd.read_csv("D:/OneDrive - DOI/1. Projects/COG Water Use/COG_python/study_counties_tx.csv")
nm_county = [5,9,11,15,25,27,35,41]
nm_county_table = pd.read_csv("D:/OneDrive - DOI/1. Projects/COG Water Use/COG_python/study_counties_nm.csv")

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


# In[ ]:





# ***filtering data, cleaning up, prepping for deduplication - this results in a full record of treatments***

# In[5]:


#remove rows with NaN in the TotalBaseWaterVolume column
ff_tx_nm_2 = ff_tx_nm.dropna(how='any', subset=['TotalBaseWaterVolume'])
ff_tx_nm_2.reset_index(drop=True, inplace=True)

#need to convert StateNumber to StateName
ff_tx_nm_3 = ff_tx_nm_2
ff_tx_nm_3['State'] = np.where(ff_tx_nm_2['StateNumber']>35, 'Texas', 'New Mexico')
ff_tx_nm_4 = ff_tx_nm_3.drop(['StateNumber'], axis=1)
ff_tx_nm_4.reset_index(drop=True, inplace=True)

#now need to pull year from JobEndDate
ff_tx_nm_4['Year'] = ff_tx_nm_4['JobEndDate'].astype(str).str.slice(-16,-11)
ff_tx_nm_4.reset_index(drop=True, inplace=True)

#fix the spaces after and slashes before in Year column - modified from Grady Ball's notebook
newyear = {'/2009':2009, '2010 ':2010, '2011 ':2011,'2012 ':2012,'/2012':2012,'/2013':2013,'2013 ':2013,
           '/2015':2015,'2015 ':2015, '2016 ':2016, '/2016':2016, '2017 ':2017, '/2017':2017, '2018 ':2018, 
           '/2018':2018,'/2019':2019, '2019 ':2019, '2014 ':2014, '/2014':2014, '/2020':2020, '2020 ':2020}
ff_tx_nm_4['Year'] = ff_tx_nm_4['Year'].map(newyear)
ff_tx_nm_4.reset_index(drop=True, inplace=True)

#cleanup
ff_tx_nm_5 = ff_tx_nm_4.drop(['JobEndDate'], axis=1)
ff_tx_nm_5 = ff_tx_nm_5[['APINumber', 
                        'State',
                        'COUNTY',
                        'Latitude',
                        'Longitude',
                        'Year',
                        'TotalBaseWaterVolume',
                        'TVD']]
ff_tx_nm_5.rename(columns={'COUNTY': 'County'}, inplace=True)
ff_tx_nm_5.reset_index(drop=True, inplace=True)

#want to remove all from year 2020
ff_tx_nm_6 = ff_tx_nm_5[ff_tx_nm_5.Year != 2020]
ff_tx_nm_6.reset_index(drop=True, inplace=True)

#this is the final version before messing with duplicates or removing any entries 20200602
ff_tx_nm_6.info()


# In[6]:


#full list of treatments
#used to filter by county/year on 20200625
#ff_tx_nm_6.to_csv('D:\\OneDrive - DOI\\1. Projects\\COG Water Use\\COG_python\\python_results\\ff_tx_nm_20200604.csv')


# In[ ]:





# ***de-duplication cell***

# In[7]:


#optional df: ff_tx_nm_dedup - remove duplicate API numbers to see just individual wells, not sum of treatments

#removing duplicates with same API number
ff_tx_nm_dedup = ff_tx_nm_6.drop_duplicates(subset=['APINumber'], keep='first')
ff_tx_nm_dedup.reset_index(drop=True, inplace=True)

#want to remove all from year 2020
ff_tx_nm_dedup = ff_tx_nm_dedup[ff_tx_nm_dedup.Year != 2020]
ff_tx_nm_dedup.info()


# In[8]:


#individual wells from ff_tx_nm_6 but with duplicate API numbers removed
#ff_tx_nm_dedup.to_csv('D:\\OneDrive - DOI\\1. Projects\\COG Water Use\\COG_python\\python_results\\ff_tx_nm_dedup_20200604.csv')


# In[9]:


ff_tx_nm_dedup


# In[ ]:





# ***filter by year and county - splitting into full (ff) and de-duplicated (ff_dedup) datasets ***

# In[10]:


#rename df for clean start

ff = ff_tx_nm_6
ff_dedup = ff_tx_nm_dedup


# In[11]:


#create df for counties

counties = ff.County.value_counts().rename_axis('county').reset_index(name='ff_count') #indicates treatments
counties_dedup = ff_dedup.County.value_counts().rename_axis('county').reset_index(name='ff_count_dedup') #indicates wells


# In[12]:


#create df for years

years = ff.Year.value_counts().reset_index() #indicates treatments
years_dedup = ff_dedup.Year.value_counts().reset_index() #indicates wells


# In[13]:


#export csv's

#counties.to_csv("D:/OneDrive - DOI/1. Projects/COG Water Use/COG_python/counties.csv")
#years.to_csv("D:/OneDrive - DOI/1. Projects/COG Water Use/COG_python/years.csv")
#ff.to_csv("D:/OneDrive - DOI/1. Projects/COG Water Use/COG_python/ff.csv")


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

ff_tx_nm_7 = ff_tx_nm_6
ff_tx_nm_7['VolSum'] = ff.groupby(['APINumber'])['TotalBaseWaterVolume'].transform('sum')
ff_tx_nm_7['VolMean'] = ff.groupby(['APINumber'])['TotalBaseWaterVolume'].transform('mean')
ff_tx_nm_7 = ff_tx_nm_7.drop_duplicates(subset=['APINumber'], keep='first')
ff_tx_nm_7.reset_index(drop=True, inplace=True)
ff_tx_nm_7['delta'] = ff_tx_nm_7['TotalBaseWaterVolume'] - ff_tx_nm_7['VolMean']
ff_tx_nm_7['sum_to_total'] = (ff_tx_nm_7['VolSum']) / (ff_tx_nm_7['TotalBaseWaterVolume'])
ff_tx_nm_7.describe()


# ***this is the important df here, after this some minimal processing was done such as lat/long cleanup/verification in arc***

# In[64]:


#ff_tx_nm_7.to_csv('D:\\OneDrive - DOI\\1. Projects\\COG Water Use\\COG_python\\20200626_results\\ff_tx_nm_7_withstats.csv')


# In[ ]:




