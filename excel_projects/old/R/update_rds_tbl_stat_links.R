
remove(list = ls())
source("R/vars_global.R")
source("R/vars_stat_links.R")
source("R/save_html_from_url.R")

url_base_tr <- "https://www.teamrankings.com"

# Some functions. ####
abbreviate_stat <- function(stat = NULL, delim = NULL) {
  # delim should replece "_" in the following line.
  return(sapply(lapply(strsplit(stat, "_"), substr, 1, 1), paste, collapse = ""))
}

rename_stat <- function(stat = NULL,
                             replace_stat_dash = TRUE,
                             abbreviate_stat = TRUE) {
  if(is.null(stat)) {
    cat("Input stat not provided./n")
    stop()
  }
  
  char_stat <- stat
  
  if(replace_stat_dash){
    char_stat <- gsub("-", "_", stat)
  }
  
  if(abbreviate_stat){
    if(replace_stat_dash) {
      char_stat <- abbreviate_stat(char_stat, "_")
    } else {
      char_stat <- abbreviate_stat(char_stat, "-")
    }
  }
  
  return(char_stat)
}


url_stat_nba_links_all <- paste0(url_base_tr, "/nba/stats/")
html_stat_nba_links_all <- 
  save_html_from_url(url = url_stat_nba_links_all,
                     save_dir = dir_data,
                     save_file_name_w_ext = "nba_stat_links_all.html")

# categories_all <-
#   html_stat_nba_links_all %>% read_html %>%
#   html_nodes("body div div div main ul li") %>% html_text()
# categories_all_2 <- categories_all[str_detect(categories_all, "\r\n\t\t")]
# categories <- categories_all_2 %>% str_split(" \\\r") %>% purrr:: map(1)


links_all_nba_stat_raw <-
  html_stat_nba_links_all %>% read_html %>%
  html_nodes("ul li ul li a") %>% html_attr("href")

links_all_nba_stat <- links_all_nba_stat_raw[str_detect(links_all_nba_stat_raw, "/nba/stat/")]
# links_all_nba_stat <- str_subset(links_all_nba_stat, "/nba/stat/")
# links_all_nba_stat_no_prefix <- str_replace(links_all_nba_stat, "/nba/stat/", "")

links_all_nba_stat_no_prefix <-
  links_all_nba_stat %>%
  str_subset("/nba/stat/") %>%
  str_replace("/nba/stat/", "")


# Post-process.
# links_renamed <-
#   links_all_nba_stat_no_prefix %>%
#   str_replace_all("1st", "first") %>%
#   str_replace_all("2nd", "second") %>%
#   str_replace_all("3rd", "third") %>%
#   str_replace_all("4th", "fourth")

links_renamed <-
  links_all_nba_stat_no_prefix %>%
  str_replace_all("opponent", "opp") %>%
  
  str_replace_all("overtime", "ot") %>%
  # str_replace_all("fastbreak", "fb") %>%
  
  str_replace_all("average", "avg") %>%
  str_replace_all("per-game", "pg") %>%
  
  str_replace_all("-attempted-", "-a") %>%
  str_replace_all("-made-", "-m") %>%
  
  str_replace_all("1st-quarter", "q1") %>%
  str_replace_all("2nd-quarter", "q2") %>%
  str_replace_all("3rd-quarter", "q3") %>%
  str_replace_all("4th-quarter", "q4") %>%
  
  str_replace_all("1st-half", "h1") %>%
  str_replace_all("2nd-half", "h2") %>%
  
  str_replace_all("possessions", "poss") %>%
  str_replace_all("possession", "poss") %>%
  str_replace_all("percentage", "pct") %>%
  str_replace_all("percent", "pct") %>%
  
  str_replace_all("field-goals", "fg") %>%
  str_replace_all("field-goal", "fg") %>%
  
  str_replace_all("-pointers-", "-ptrs-") %>% 
  str_replace_all("-point-", "-pt-") %>% 
  str_replace_all("-3-", "-three-") %>% 
  str_replace_all("-2-", "-two-") %>% 
  
  str_replace_all("offensive", "off") %>%
  str_replace_all("defensive", "def") %>%
  # str_replace_all("effective", "eff") %>%
  # str_replace_all("efficiency", "effcy") %>%
  
  str_replace_all("free-throws", "ft") %>%  
  str_replace_all("free-throw", "ft") %>%
  
  str_replace_all("points", "pts") %>%
  str_replace_all("pointer", "ptr") %>%
  str_replace_all("turnovers", "turn") %>%
  str_replace_all("turnover", "turn") %>%
  str_replace_all("assists", "ast") %>%
  str_replace_all("assist", "ast") %>%
  str_replace_all("rebounding", "reb") %>% 
  str_replace_all("rebounds", "reb") %>%  
  str_replace_all("rebound", "reb") %>%  
  str_replace_all("steals", "stl") %>%
  str_replace_all("steal", "stl") %>%
  str_replace_all("blocked", "blk") %>%
  str_replace_all("blocks", "blk") %>%
  str_replace_all("block", "blk") %>%
  
#   str_replace_all("-per-", "p")
# str_replace_all("-of-", "") %>%
# str_replace_all("-from-", "")
  rename_stat(abbreviate_stat = FALSE)

links_all_nba_stat_abbrv <- rename_stat(links_all_nba_stat_no_prefix, abbreviate_stat = TRUE)

links_abbrv <-
  links_all_nba_stat_abbrv %>%
  str_replace_all("1q", "q1") %>%
  str_replace_all("2q", "q2") %>%
  str_replace_all("3q", "q3") %>%
  str_replace_all("4q", "q4") %>%
  str_replace_all("1h", "h1") %>%
  str_replace_all("2h", "h2")

tbl_nba_stat_links <- 
  tibble(links_all_nba_stat, 
         links_all_nba_stat_no_prefix,
         links_renamed, 
         links_abbrv)


# tbl_nba_stat_links_main <- tbl_links %>%
#   filter(!str_detect(links_all_nba_stat_no_prefix, "opponent"))
# 
# stats_to_remove <- c("opponent", "quarter", "half", "average")
# tbl_links_main_2 <- tbl_links %>%
#   mutate(remove = links_all_nba_stat_no_prefix %in% stats_to_remove) %>%
#   filter(remove == TRUE)
# 
# tbl_links_main_2 <- tbl_links %>%
#   filter()

rows_scoring <- 1:28
rows_shooting <- 29:46
rows_reb <- 47:53
rows_blk_stl <- 54:57
rows_ast_turn <- 58:64
rows_foul <- 65:68
rows_opp_offset <- 69
rows_scoring_opp <- 69:88
rows_shooting_opp <- 89:106
rows_reb_opp <- 107:112
rows_blk_stl_opp <- 113:117
rows_ast_turn_opp <- 118:124
rows_foul_opp <- 125:128
rows_other <- 129:133
rows_win_pct <- 134: 137

tbl_cat <- 
  tibble(category = "Scoring", row_range = rows_scoring) %>%
  add_row(category = "Shooting", row_range = rows_shooting) %>%
  add_row(category = "Rebounding", row_range = rows_reb) %>%
  add_row(category = "Blocks & Steals", row_range = rows_blk_stl) %>%
  add_row(category = "Assists & Turnovers", row_range = rows_ast_turn) %>%
  add_row(category = "Fouls", row_range = rows_foul) %>%
  # add_row(category = "Opponent", row_range = rows_opp_offset:(2 * rows_opp_offset - 1)) %>% 
  add_row(category = paste0("Opponent ", "Scoring"), row_range = rows_scoring_opp) %>%
  add_row(category = paste0("Opponent ", "Shooting"), row_range = rows_shooting_opp) %>%
  add_row(category = paste0("Opponent ", "Rebounding"), row_range = rows_reb_opp) %>%
  add_row(category = paste0("Opponent ", "Blocks & Steals"), row_range = rows_blk_stl_opp) %>%
  add_row(category = paste0("Opponent ", "Assists & Turnovers"), row_range = rows_ast_turn_opp) %>%
  add_row(category = paste0("Opponent ", "Fouls"), row_range = rows_foul) %>%
  add_row(category = "Other", row_range = rows_other) %>%
  add_row(category = "Winning Percentage", row_range = rows_win_pct)

tbl_nba_stat_links_bind <- tbl_nba_stat_links %>%
  bind_cols(tbl_cat) %>%
  dplyr::select(-row_range)

# Rename some stuff so that this can be generalized to other sports.
tbl_stat_links <- tbl_nba_stat_links_bind %>%
  rename(link_raw = links_all_nba_stat,
         link_no_prefix = links_all_nba_stat_no_prefix,
         stat = links_renamed,
         stat_abbrv = links_abbrv)

file_name_w_ext <- "tbl_stat_links.rds"
file_name_entire <- paste0(dir_data, file_name_w_ext)
if (file.exists(file_name_entire)) {
    # if (file_name_w_ext %in% list.files(dir_data)) {
    file_name_ext <- ".rds"
    file_name_new <- paste0(str_replace(file_name_entire, file_name_ext, ""), "_", 
                            format(Sys.time(), format = "%Y-%m-%d_%I-%M-%S"), 
        file_name_ext)
    file.copy(from = file_name_entire, to = file_name_new)
}

saveRDS(tbl_stat_links, file = file_name_entire)




