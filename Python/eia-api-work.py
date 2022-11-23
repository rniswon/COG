#!/usr/bin/env python
# coding: utf-8

#%%

## jmcdowell 20211216
## python 3.7.6

## script to pull selected EIA APIs (www.eia.gov/opendata/) and write each to individual CSV

import requests
import json
import pandas as pd
import sys

print('python version:', sys.version)
#%%

#create function to check API status code, 200 means its working fine
def checkStatusCode(code):
    if code == 200:
        print("200 - good to go")
    elif code != 200:
        print("oh no, there is an error...")

#%%

#unique API key
key = '0c9ab25986283cac3c81d09557fa771f' #jmcdowell EIA API key, working as of 12-16-2021

#list of APIs
rigOnshoreCount = f"http://api.eia.gov/series/?api_key={key}&series_id=TOTAL.OGNRPON.M"
brentPrice = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.BREPUUS.M"
wtiPrice = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.WTIPUUS.M"
henryPrice = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.NGHHMCF.M"
crudeProd = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.COPRPUS.M"
crudeInvent = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.COSXPUS.M"
natGasProd = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.NGMPPUS.M"
natGasInvent = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.NGWGPUS.M"
prodPricePetroIndex = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.WP57IUS.M"
totIndProdIndex = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.ZOTOIUS.M"
heatingDays = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.ZWHDPUS.M"
coolingDays = f"http://api.eia.gov/series/?api_key={key}&series_id=STEO.ZWCDPUS.M"
crudeWells = f"http://api.eia.gov/series/?api_key={key}&series_id=TOTAL.PATWPUS.M"
natGasWells = f"http://api.eia.gov/series/?api_key={key}&series_id=TOTAL.NGTWPUS.M"
crudeFootage = f"http://api.eia.gov/series/?api_key={key}&series_id=TOTAL.OGPFPUS.M"
natGasFootage = f"http://api.eia.gov/series/?api_key={key}&series_id=TOTAL.OGGFPUS.M"

#list of strings and API addresses for use in loop
API_str_list = ['rigOnshoreCount', 'brentPrice', 'wtiPrice', 'henryPrice',
                'crudeProd', 'crudeInvent', 'natGasProd', 'natGasInvent', 
                'prodPricePetroIndex', 'totIndProdIndex', 'heatingDays', 'coolingDays',
                'crudeWells', 'natGasWells', 'crudeFootage', 'natGasFootage']
API_link_list = [rigOnshoreCount, brentPrice, wtiPrice, henryPrice,
                 crudeProd, crudeInvent, natGasProd, natGasInvent,
                 prodPricePetroIndex, totIndProdIndex, heatingDays, coolingDays,
                 crudeWells, natGasWells, crudeFootage, natGasFootage]

#%%

#loop over all APIs to create individual CSVs
for string,link in zip(API_str_list, API_link_list):
    #pulling API data
    data = requests.get(link)
    print(string)
    checkStatusCode(data.status_code)
    textData = data.text
    jsonData = json.loads(textData)
    #get header line info from API
    name = jsonData['series'][0]['name']
    units = jsonData['series'][0]['units']
    end = jsonData['series'][0]['end']
    updated = jsonData['series'][0]['updated']
    header = ("name: " + name + " /// units: " + units + " /// most recent: " + end + " /// last updated: " + updated)
    #convert to df and export to csv
    df = pd.DataFrame(jsonData['series'][0]['data'], columns= ['date', 'value'])
    df.columns = pd.MultiIndex.from_tuples(zip([header,''], df.columns)) #adding new header above dataframe column headers
    df.to_csv('../../data-raw/eia_' + string + '.csv')

#%%



