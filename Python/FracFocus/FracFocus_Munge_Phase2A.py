#!/usr/bin/env python
# coding: utf-8

# ***frac focus data work - COG Water Use Permian Basin - jmcdowell@usgs.gov - 2020***


# ** 2022- AEG made a few modifications to preserve month 6/2022 and integrated 
#    Natalie's Jupyter notebook 'AddHUC12ToFracFocusdata.ipynb
#    - if you want, you can use  conda environment from geo.yml which includes 
#      all needed packages. 
#    1. Create the environment from the geo.yml (stored in Python/envs ) by 
#       opening the anaconda prompt, natigating to the location of the yml 
#       and typing command 'conda env create -f geo.yml'
#    2. Activate the environment by typing 'conda activate geo'
#    3. Run the script as you would (type spyder to open the spyder ide)

# General workflow -
# process the raw Frac Focus data by: 
# - various formatting
# - filtering for only counties in NM and TX
# - removing rows with NaN in the TotalBaseWaterVolume column
# - retaining the date 
# - fixing projection issues
# - adding huc12 into the dataframe
# - (didn't include the clip by permian boundary based on our discussions to 
# -     be as similar to our previous work as possible in which we used 
# -     counties to define our boundary)
# - export as a csv  


#%%
# Import required packages
import os
import glob
import pandas as pd
import numpy as np
import geopandas as gp
from shapely.geometry import Point
import matplotlib.pyplot as plt

# points working directory to the location of this script
os.chdir(os.path.dirname(__file__))
print('current working directory:',os.getcwd())


#%% set up directory 
# move working directory to 'data-raw' 
os.chdir('../../data-raw')
print('current working directory:',os.getcwd())


#%% bring in the frac focus data 

ext = 'csv'
all_filenames = [i for i in glob.glob('reg*.{}'.format(ext))]


# combine all files
combined_csv = pd.concat([pd.read_csv(f, low_memory=False) for f in all_filenames])

pd.options.display.max_columns = None
#combined_csv


#%% bring in state and county codes

tx_county_table = pd.read_csv("study_counties_tx.csv")
nm_county_table = pd.read_csv("study_counties_nm.csv")

tx = [42]
nm = [30]

tx_county = tx_county_table['CountyNumber'].tolist()
nm_county = nm_county_table['CountyNumber'].tolist()


#%% filtering, merging, combining. 

# filter by API state code 42 for TX
df_tx = combined_csv[(combined_csv.StateNumber.isin(tx))]
df_tx.reset_index(drop=True, inplace=True)

# filter by API county codes for TX in study area
df_tx_2 = df_tx[(df_tx.CountyNumber.isin(tx_county))]
df_tx_2.reset_index(drop=True, inplace=True)

# add a new column for County, using a vlookup-type function to convert county 
# number to a county name. This will ensure accurate county names, unlike the 
# CountyName column, which has typos aplenty
df_tx_3 = pd.merge(df_tx_2, 
                    tx_county_table, 
                    on='CountyNumber',
                    how='left')

# filter by API state code 30 for NM
df_nm = combined_csv[(combined_csv.StateNumber.isin(nm))]
df_nm.reset_index(drop=True, inplace=True)

# filter by API county codes for NM in study area
df_nm_2 = df_nm[(df_nm.CountyNumber.isin(nm_county))]
df_nm_2.reset_index(drop=True, inplace=True)

# add a new column for County, using a vlookup-type function to convert county
# number to a county name this will ensure accurate county names, unlike the 
# CountyName column, which has typos aplenty
df_nm_3 = pd.merge(df_nm_2, 
                    nm_county_table, 
                    on='CountyNumber',
                    how='left')
df_nm_3.reset_index(drop=True, inplace=True)

#merge tx and nm data
combine = [df_nm_3, df_tx_3]
ff_tx_nm = pd.concat(combine)
ff_tx_nm.reset_index(drop=True, inplace=True)


#%% filtering data, cleaning up, prepping for deduplication - 
# this results in a full record of treatments

# remove rows with NaN in the TotalBaseWaterVolume column
ff_tx_nm_2 = ff_tx_nm.dropna(how='any', subset=['TotalBaseWaterVolume'])
ff_tx_nm_2.reset_index(drop=True, inplace=True)

# need to convert StateNumber to StateName
ff_tx_nm_3 = ff_tx_nm_2
ff_tx_nm_3['State'] = np.where(ff_tx_nm_2['StateNumber']>35, 'Texas', 'New Mexico')
ff_tx_nm_4 = ff_tx_nm_3.drop(['StateNumber'], axis=1)
ff_tx_nm_4.reset_index(drop=True, inplace=True)


#%% AEG modified this cell to use pd.datetime and to preserve month
ff_tx_nm_4['job_end_date'] = pd.to_datetime(ff_tx_nm_4['JobEndDate']).dt.date
ff_tx_nm_4.reset_index(drop=True, inplace=True)


#%% AEG removed some of these lines that weren't necessary after using datetime, and retained the year 2020

# cleanup
ff_tx_nm_5 = ff_tx_nm_4.drop(['JobEndDate','pKey',], axis=1) #'COUNTY_x'
ff_tx_nm_5.rename(columns={'COUNTY': 'County'}, inplace=True)
ff_tx_nm_5.reset_index(drop=True, inplace=True)

# this is the final version before messing with duplicates or removing any entries
ff_tx_nm_5.info()


#%% crerate mean volume column, with only a single entry for each API

ff_tx_nm_6 = ff_tx_nm_5
ff_tx_nm_6['VolMeanPerAPI'] = ff_tx_nm_5.groupby(['APINumber'])['TotalBaseWaterVolume'].transform('mean')
ff_tx_nm_6 = ff_tx_nm_6.drop_duplicates(subset=['APINumber'], keep='first')
ff_tx_nm_6.reset_index(drop=True, inplace=True)
ff_tx_nm_6['delta'] = ff_tx_nm_6['TotalBaseWaterVolume'] - ff_tx_nm_6['VolMeanPerAPI']
ff_tx_nm_6.describe()


#%% working with projections (NAH)

# there are 4 unique Projections- 'NAD27', 'WGS84', 'NAD83', 'Nad27'
ff_tx_nm_6.Projection.unique()

# merge NAD27 and Nad27, reproject all to NAD83
ff_tx_nm_6 = ff_tx_nm_6.replace(['Nad27'],'NAD27')
df_NAD27 = ff_tx_nm_6[ff_tx_nm_6['Projection'] == "NAD27"].copy()
df_WGS84 = ff_tx_nm_6[ff_tx_nm_6['Projection'] == "WGS84"].copy()
df_NAD83 =  ff_tx_nm_6[ff_tx_nm_6['Projection'] == "NAD83"].copy()

# convert dataframes to a geodataframe - for Shapely 2.0 will need to convert 

# 'coords' to np array
geometry = df_NAD27[["Latitude", "Longitude"]].apply(
    lambda coord: Point(coord["Longitude"], coord["Latitude"]), axis=1)

# Set Coordinate Reference System
crs = "EPSG:4267"
df_geo27 = gp.GeoDataFrame(df_NAD27, geometry=geometry, crs=crs)

geometry = df_NAD83[["Latitude", "Longitude"]].apply(
    lambda coord: Point(coord["Longitude"], coord["Latitude"]), axis=1)

# Set Coordinate Reference System
crs = "EPSG:4269"
df_geo83 = gp.GeoDataFrame(df_NAD83, geometry=geometry, crs=crs)

geometry = df_WGS84[["Latitude", "Longitude"]].apply(
    lambda coord: Point(coord["Longitude"], coord["Latitude"]), axis=1)

# Set Coordinate Reference System
crs = "EPSG:4326"
df_geo84 = gp.GeoDataFrame(df_WGS84, geometry=geometry, crs=crs)

# reproject geodataframes so that everything is in NAD83
df_geo27to83 = df_geo27.to_crs(epsg=4269)
df_geo84to83 = df_geo84.to_crs(epsg=4269)

# check the projections, Name in all is NAD83
df_geo27to83.crs
df_geo84to83.crs
df_geo83.crs

# replace the Projection name since we reprojected
df_geo27to83a = df_geo27to83.replace(['NAD27'],'NAD83')
df_geo84to83a = df_geo84to83.replace(['WGS84'],'NAD83')

# append dataframes
df_geo83a = df_geo83.append(df_geo27to83a)
df_geo83b = df_geo83a.append(df_geo84to83a)

# Update latitude and longitude fields with the new geometry all projected to 
# NAD83 ESPG 4269 
df_geo83b['Longitude'] = df_geo83b.geometry.apply(lambda p: p.x)
df_geo83b['Latitude'] = df_geo83b.geometry.apply(lambda p: p.y)

df_geo83b.Projection.unique() # should be only 'NAD83'


#%% read HUCS into dataframe; within repository but gitignored
# from sciencebase(https://www.sciencebase.gov/catalog/item/5fc90839d34e4b9faad8a148)

HUCS = gp.read_file("gis/WBD_HUC12_CONUS_10262020/WBD_HUC12_CONUS_10262020.shp")

# check projection of HUCS, should be 'NAD83' aka epsg:4269
print(HUCS.crs)

# Add HUC12 info data from HUC shapefile to FracFocus data using sjoin
df_geo83c = gp.sjoin(df_geo83b, HUCS, how='left', op='within')


#%% some checking and clean up
# plot it with the permian boundary out of the boundary since we used the counties to clip
BND = gp.read_file("gis/PermianBasin_Extent_201712.shp")
nm_tx_cnties = gp.read_file('gis/nm_tx_nad83.shp')

# reproject to NAD83
BND2 = BND.to_crs(epsg=4269)
BND2.crs

#clip to counties
clipped = gp.clip(df_geo83c, nm_tx_cnties)

num_rows_orig = len(df_geo83c)
num_rows_clip = len(clipped)
prct_removed = (1- num_rows_clip/num_rows_orig)*100
prct_removed = round(prct_removed,2)
print (prct_removed,'% of the records were removed.', num_rows_orig - num_rows_clip,
       'rows were removed after clipping by counties')

ax = nm_tx_cnties.plot(figsize=(15,5))
plt.scatter(clipped["geometry"].x, clipped["geometry"].y, color='r', s=1)


#%% write csv with 49471 records
fracfocus_short = clipped[['APINumber','OperatorName','TVD','State','County', 'Latitude', 'Longitude',
                         'job_end_date', 'VolMeanPerAPI', 'huc12']].copy()
fracfocus_short.reset_index(drop=True, inplace=True)
fracfocus_short.to_csv(os.path.join('..','data','fracfocus.csv'))

