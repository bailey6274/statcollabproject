import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pandas.io.parsers.readers import read_csv
from nba_api.stats.endpoints import playercareerstats
from nba_api.stats.endpoints import commonplayerinfo
from nba_api.stats.static import teams
from nba_api.stats.endpoints import commonteamroster
from nba_api.stats.endpoints import commonplayerinfo


########### GET MINNESOTA ROSTER DATA ########### 

nba_teams = teams.get_teams()
min = [team for team in nba_teams if team["full_name"] == "Minnesota Timberwolves"][0]
min_id = min['id']

commonteamroster.CommonTeamRoster(min_id, 2015).get_data_frames()[0]

total_roster_min = commonteamroster.CommonTeamRoster(min_id, 2015).get_data_frames()[0]

for year in range(2016, 2023):
  temp = commonteamroster.CommonTeamRoster(min_id, year).get_data_frames()[0]
  total_roster_min = pd.concat([temp,total_roster_min])

# Save CSV
total_roster_min.to_csv('total_roster_min.csv')

########### GET PLAYER STATS ########### 

player_ids = total_roster_min['PLAYER_ID']
player_stats_min = playercareerstats.PlayerCareerStats(player_id=player_ids[0]).get_data_frames()[0]
for id in player_ids:
  tempsingle = playercareerstats.PlayerCareerStats(player_id=id).get_data_frames()[0]
  tempsingle_min = tempsingle.loc[tempsingle['TEAM_ID'] == min_id]
  player_stats_min = pd.concat([tempsingle_min, player_stats_min])

# Save CSV
player_stats_min.to_csv('player-stats-minnesota.csv')








