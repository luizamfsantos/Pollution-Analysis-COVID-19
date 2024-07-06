# %% [markdown]
# # Analysis of Air Quality Time Series Data From Different Cities
# 
# 
# ## Austin-Round Rock / TX

# %%
# import libraries
import pandas as pd
import matplotlib.pyplot as plt
import sqlite3

# %%
# import data
df = pd.read_csv('air_quality/epa_air_data_daily_air_quality_ozone_pm25_austin_roundrock.csv')

# %%
# clean data
df.head()
df.Date = pd.to_datetime(df.Date)
df['Main Pollutant'] = df['Main Pollutant'].astype('category')
print(df.isna().sum())
print(df.dtypes)
print(df.describe())
print(df.var())

# %%
# visualize data
df.head()

# %%
# plot data
%matplotlib inline
ax = plt.gca()
df.plot(kind = 'line', x = 'Date', y = '2020 AQI Value', ax = ax)
df.plot(kind = 'line', x = 'Date', y = '20-year High (1980-2019)', ax = ax)
df.plot(kind = 'line', x = 'Date', y = '20-year Low (1980-2019)', ax = ax)
df.plot(kind = 'line', x = 'Date', y = '5-year Median (2015-2019)', ax = ax)
#plt.savefig('air_quality_Austin.png')

# %% [markdown]
# It does not seem like Austin Air quality has varied much as it approaches the median historical data.

# %% [markdown]
# # San Diego Air Quality data

# %%
# import data
df = pd.read_csv('air_quality/epa_air_data_daily_air_quality_ozone_pm25_san_diego.csv')

# %%
# clean data

# missing values
df.isnull().sum()
df[df['5-year Median (2015-2019)'] == '.']
try:
    df = df.drop(59)
except:
    pass

# change data types
df = df.astype({'5-year Median (2015-2019)':'int64'})
df.Date = pd.to_datetime(df.Date)

# %%
# visualize data
%matplotlib inline
ax = plt.gca()
df.plot(kind = 'line', x = 'Date', y = '2020 AQI Value', ax = ax, color = 'blue')
df.plot(kind = 'line', x = 'Date', y = '20-year High (1980-2019)', ax = ax, color = 'red')
df.plot(kind = 'line', x = 'Date', y = '20-year Low (1980-2019)', ax = ax, color = 'green')
df.plot(kind = 'line', x = 'Date', y = '5-year Median (2015-2019)', ax = ax, color = 'orange')
plt.title('San Diego Air Quality Index')
#plt.savefig('air_quality_San_Diego.png')

# %%
# import data
ozone20 = pd.read_csv('data/air_quality/2020_epa_air_data_daily_air_quality_ozone_san_diego.csv')

# %%
# clean data
ozone20.head()
ozone20.drop('Source',axis=1)

# %% [markdown]
# Different Air Quality measures API: https://aqs.epa.gov/aqsweb/documents/data_api.html#signup
# check email for key


