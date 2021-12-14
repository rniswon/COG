#!/usr/bin/env python
# coding: utf-8

# In[1]:


#working on using the EIA API for both oil price (WTI?) and number of rigs (onshore?)
#my key: 3b7f20fe3266737c13b77b8c2064ffd5

import requests
import json


# ## other APIs to pull:
# 
# Oil economics (price, supply, and inventory):
# 
# Brent crude oil spot price, Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.BREPUUS.M
# 
# West Texas Intermediate Crude Oil Price, Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.WTIPUUS.M
# 
# U.S. Crude Oil Production, Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.COPRPUS.M
# 
# Crude Oil Inventory (excluding SPR), Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.COSXPUS.M
# 
# 
# Natural gas economics (price, supply, and inventory):
# 
# Natural Gas Henry Hub Spot Price ($/mcf), Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.NGHHMCF.M
# 
# Natural Gas Total Marketed Production, Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.NGMPPUS.M
# 
# Natural Gas Working Inventory U.S. Total, Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.NGWGPUS.M
# 
# 
# Macroeconomics (price and production/-ivity):
# 
# Producer Price Index: Petroleum, Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.WP57IUS.M
# 
# Total Industrial Production Index, Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.ZOTOIUS.M
# 
# 
# Weather:
# 
# Heating Degree Days U.S. Average , Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.ZWHDPUS.M
# 
# Cooling Degree Days U.S. Average, Monthly
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=STEO.ZWCDPUS.M
# 
# 
# The following is annual energy outlook (annual through 2050) data:
# Population:
# Resident population including Armed Forces, United States
# http://api.eia.gov/series/?api_key=YOUR_API_KEY_HERE&series_id=SEDS.TPOPP.US.A

# In[17]:


#unique API key
key = '0c9ab25986283cac3c81d09557fa771f'

#U.S. Crude Oil and Natural Gas Active Well Service Rigs in operation, Monthly
endpoint1 = f"http://api.eia.gov/series/?api_key={key}&series_id=TOTAL.OGNRPON.M" #https://www.eia.gov/opendata/qb.php?sdid=NG.E_ERTS0_XRS_NUS_C.M

#Cushing, OK WTI Spot Price FOB, Monthly
endpoint2 = f"http://api.eia.gov/series/?api_key={key}&series_id=PET.RWTC.M" #https://www.eia.gov/opendata/qb.php?category=241335&sdid=PET.RWTC.M

query_params = {"series":"data"}


# In[18]:


rig_count_monthly = requests.get(endpoint1, params=query_params)#.json() # params=query_params
rig_count_monthly.status_code


# In[19]:


rig_data = rig_count_monthly.text
rig_parse_json = json.loads(rig_data)
rig_parse_json


# In[20]:


wti_cushing_monthly = requests.get(endpoint2, params=query_params)
wti_cushing_monthly.status_code


# In[21]:


wti_data = wti_cushing_monthly.text
wti_parse_json = json.loads(wti_data)
wti_parse_json


# In[ ]:




