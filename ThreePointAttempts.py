# -*- coding: utf-8 -*-
"""
Created on Thu Mar  5 13:59:15 2026

@author: Jackson Bayuk
"""
#Packages
import pandas as pd
import statistics as stat


#Getting Total Three Point attempt data
years = list(range(2010,2025))



url = 'https://www.basketball-reference.com/leagues/NBA_2023.html'

tables = pd.read_html(url)[7]

StatTotals2026 = 

stat.mean(StatTotals2026['3PA'])


for year in years:
    
    url = f"https://www.basketball-reference.com/leagues/NBA_{year}.html"
    
    
    
import pandas as pd
import time

start_year = 2016
end_year = 2025

dfs = []

for year in range(start_year, end_year + 1):

    url = f"https://www.basketball-reference.com/leagues/NBA_{year}.html"
    
    print(f"Scraping {year}...")
    
    df = pd.read_html(url)[7]
    
    # remove repeated header rows
    df = df[df["Team"] != "Team"]
    
    # add season column
    df["Season"] = year
    
    dfs.append(df)
    
    time.sleep(2)   # polite delay so you don't get blocked

# combine all seasons
all_teams = pd.concat(dfs, ignore_index=True)

print(all_teams.head())



avg_3pa_by_yearNew = all_teams.groupby("Season")["3PA"].mean().reset_index()

# rename column
avg_3pa_by_yearNew.columns = ["Season", "Avg_Team_3PA"]

print(avg_3pa_by_yearNew)

newDFs = [avg_3pa_by_year, avg_3pa_by_yearNew]

all_teamsNew = pd.concat(newDFs, ignore_index=True)

all_teamsNew.to_csv('ThreePointAttemptsByYearAll.csv')


