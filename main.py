# **************** Import libraries and load data sets *****************************
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load GDP Data and Energy Data into pandas data frames.
energy_df = pd.read_csv('Primary-energy-consumption-from-fossilfuels-nuclear-renewables.csv')
gdp_df = pd.read_csv('API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3263806.csv', header=4)
# Create and read in a region for each country. Self created by author.
regions_df = pd.read_csv('regions.csv')

# ************ Clean Primary-energy-consumption-from-fossilfuels-nuclear-renewables data************
# Take the Country Code, Year and Renewable Percentage Columns.
energy_df = energy_df[['Code', 'Year', 'Renewables (% sub energy)']]
# Take the years from 1970 to 2018 and drop the rest.
energy_df = energy_df[energy_df["Year"] >= 1970]
# Remove the OWID_WRL Rows as these will not be in my other data set.
energy_df = energy_df.drop(energy_df.index[energy_df["Code"] == 'OWID_WRL'])
# Remove NaN values in Code as some countries have NaN.
energy_df = energy_df.dropna(subset=['Code'])

# Sort energy_df by Country Code and Year.
energy_df = energy_df.sort_values(["Code", "Year"], ascending=[True, True])
# Reset the index to clean up the data.
energy_df = energy_df.reset_index(drop=True)

# ****************Create a country list from energy data country list *****************************
# Chose only the country codes from the energy data as a list to sort the GDP data by later.
country_list = energy_df['Code']
# Drop the NaN entries and drop any duplicates to get unique values.
country_list = country_list.dropna().drop_duplicates()
# Reset the index to clean up the data.
country_list = country_list.reset_index(drop=True)

# ****************Clean up GDP_annual_growth_NEW *****************************
#  Drop the columns I do not want from the GDP data.
gdp_df = gdp_df.drop(['Indicator Name', 'Indicator Code', '1960', '1961', '1962', '1963', '1964', '1965',
                      '1966', '1967', '1968', '1969', '2020'], axis=1)
# Changed the data from wide to tall on the country name and country code.
gdp_df = gdp_df.melt(id_vars=['Country Name', 'Country Code'], var_name='Year', value_name='GDP')
# Filter the Country Code by the Country Code list from the Energy Data to get the countries I want. There are many
# more countries in the GDP data that I do not want as I have no energy data for them.
gdp_df = gdp_df[gdp_df['Country Code'].isin(country_list)]
# Sort energy_df by Country Code and Year.
gdp_df = gdp_df.sort_values(["Country Code", "Year"], ascending=[True, True])
# Reset the index to clean up the data.
gdp_df = gdp_df.reset_index(drop=True)
# Year is an object, change to an int64 to allow easier merge later.
gdp_df.Year = gdp_df.Year.astype('int64')
# Make NaN zero
gdp_df['GDP'] = gdp_df['GDP'].fillna(0)

# **************** Merge Two tables together *****************************
# Merge the the GDP and Energy data into the one data frame. Merge these on Year an Country Code.
gdp_energy_join_df = gdp_df.merge(energy_df, how='inner', left_on=['Year', 'Country Code'], right_on=['Year', 'Code'])

# Disclaimer: With this merged data frame I get Country Code and Code. I checked in Excel and with below code. I
# expected the two columns to join together like Year. I thought something must be different, but I have checked.
# I also manually checked in excel. So I dropped the extra Country Code column.
True_for_matching_columns = gdp_energy_join_df['Country Code'].equals(gdp_energy_join_df['Code'])
# Drop the second code column that I do not need.
gdp_energy_join_df = gdp_energy_join_df.drop(['Code'], axis=1)

# Merged the regions on to the gdp_energy_join_df data frame and called this comb_df as its the combination of all
# three data frames, Energy, GDP and Regions.
comb_df = gdp_energy_join_df.merge(regions_df, on='Country Code')

# *****************************************************************************************************************
# ********************************************* Graphs ************************************************************
# ******************************************************************************************************************

# *********** Graph 1: Has the percentage of renewable energy increased or decreased from 1970 to 2018 globally? *****
# Select the energy data per year and get the average per year.
energy_year = energy_df.groupby('Year').mean()

# Plot the energy over the time period.
fig, px1 = plt.subplots()
px1.plot(energy_year, linewidth=3)
plt.xlabel("Year")
plt.ylabel("Renewable Energy Percentage")
plt.title(" Renewable Energy Percentage from 1970 to 2018")
plt.savefig('Graph_1.jpg')

# *********** Graph 2: Has the GDP increased or decreased from 1970 to 2018 globally? ***************
# Select the GDP data per year to and get the average.
gdp_year = gdp_df.groupby('Year').mean()

# Plot the GDP over the time period.
fig, px2 = plt.subplots()
px2.plot(gdp_year, color='r', linewidth=3)
plt.xlabel("Year")
plt.ylabel("GDP in Billions")
plt.title(" GDP in Billions from 1970 to 2018")
# Change the yticks labels so the ticks make more sense, now in Billions.
plt.yticks(np.arange(0e+10, 1.2e+12, step=0.2e+12), ['25B', '50B', '250B', '500B', '750B', '1000B'])
plt.savefig('Graph_2.jpg')

# ************ Graph 3: Has GDP and the percentage of energy from renewable energy increase together? *****************
# A function to plot the percentage of renewable energy with GDP over time to see if there is a link.
def data_time(ax, x, y, color, title, xlabel, ylabel):
    ax.plot(x, y, color=color)
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)

# Get the unique years.
year = comb_df['Year'].unique()

fig, px3 = plt.subplots()
# Call the function data_time.
data_time(px3, year, energy_year, 'blue', 'Compare GDP/Energy over time', 'Year', 'Renewable Energy Percentage')
# Had to manually move the legend location with bbox_to_anchor otherwise it would be on top of the legend of the second
# subplot.
px3.legend(['Energy'], loc='best', bbox_to_anchor=(0, 0., 0.228, 0.92))
px4 = px3.twinx()
data_time(px4, year, gdp_year, 'red', 'Compare GDP/Energy over time', 'Year', 'GDP in Billions')
px4.legend(['GDP'])
# Need to set the ticks and then set the labels when using a second plot. Not needed above as I only used the one plot.
px4.set_yticks(np.arange(0e+10, 1.2e+12, step=0.2e+12))
px4.set_yticklabels(['25B', '50B', '250B', '500B', '750B', '1000B'])
# Writing is outside the paper, this will bring it back.
plt.tight_layout()
plt.savefig('Graph_3.jpg')

# **************** Create regions so we can compare regions more closely *************************
# Get results by take the Year and Region with .groupby method and then getting the GDP average.
gdp_per_con = comb_df.groupby(['Year', 'Region'])['GDP'].mean()
# Arrange these with unstack.
gdp_per_con = gdp_per_con.unstack(level='Region')
# Get results by take the Year and Region with .groupby method and then getting the Renewable energy average.
energy_per_con = comb_df.groupby(['Year', 'Region'])['Renewables (% sub energy)'].mean()
energy_per_con = energy_per_con.unstack(level='Region')

# ******** Graph 4: Which region shows the biggest correlation between GDP and Renewable Energy? ****************
# Create line plot with GDP and Renewable Energy by regions.
fig, px5 = plt.subplots()
px5.plot(gdp_per_con, label=gdp_per_con.columns.values)
px5.legend(title='Legend', shadow=True, framealpha=0.1)
px5.set_xlabel("Year")
px5.set_ylabel("Renewable Energy Percentage")
px5.set_title(" GDP and Energy increases per region")
# For this plot I do not need values, I only want to see which values have increased together.
px5.set_yticks([])
px6 = px5.twinx()
px6.plot(energy_per_con, linestyle='--')
px6.set_ylabel("GDP in Billions")
px6.set_yticks([])
plt.savefig('Graph_4.jpg')

# **************** Graph 5: Show renewable energy by region *************************

# Plot a box plot by regions.
fig, px7 = plt.subplots()
plt.boxplot(energy_per_con)
plt.xlabel("Region")
plt.ylabel("Renewable Energy Percentage")
plt.title("Renewable Energy Per Region")
plt.xticks([1, 2, 3, 4, 5, 6, 7], ['Africa', 'Asia', 'Europe', 'Middle \nEast',
                                 'North \nAmerica', 'Ocenia', 'South \nAmerica'])
plt.tight_layout()
plt.savefig('Graph_5.jpg')
