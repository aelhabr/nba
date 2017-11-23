
source("R/vars_global.R")

# Create g_teams.rds. ####
wb <- "db_nba.xlsm"
ws <- "g_teams"

g_teams <- readxl::read_excel(paste0(dir_nba, "/", wb), sheet = ws)
g_teams$team <- as.factor(g_teams$team)

# save(g_teams, file = 'g_teams.rds') # This saves things as factors.
saveRDS(g_teams, file = paste0(dir_data, ws, ".rds"))

# Create d_win_totals.rds. ####
wb <- "db_nba.xlsm"
ws <- "d_win_totals"

d_win_totals <- readxl::read_excel(paste0(dir_nba, wb), sheet = ws)
d_win_totals$team <- as.factor(d_win_totals$team)
d_win_totals[,6:ncol(d_win_totals)] <- sapply(d_win_totals[,6:ncol(d_win_totals)], as.numeric)
d_win_totals <- d_win_totals %>%
  dplyr::select(record_id, pick_id, yr_start, 
                team, w_sportsbook, 
                odds_over, odds_under, w)

saveRDS(d_win_totals, file = paste0(dir_data, ws, ".rds"))

# Create p_win_totals.rds. ####
wb <- "db_nba.xlsm"
ws <- "p_win_totals"
p_win_totals <- readxl::read_excel(paste0(dir_nba, wb), sheet = ws)
p_win_totals$person <- as.factor(p_win_totals$person)
p_win_totals$pick <- as.factor(p_win_totals$pick)
saveRDS(p_win_totals, file = paste0(dir_data, ws, ".rds"))
