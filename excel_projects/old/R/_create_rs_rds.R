
remove(list = ls())
source("R/vars_global.R")
source("R/vars_stat_links.R")
source("R/get_tbl_from_xlsx.R")
source("R/get_tbl_stat.R")
source("R/get_sched_dates.R")


# Variables to be used. ####
wb_name_w_ext <- "db_nba_tr.xlsx"
tbl_stat_links <- try(readRDS(paste0(dir_data, "d_stat_links.rds")))


# Old functions. ####
# This works, but memory is not pre-allocated.
# This could be the predecessor for get_stat_tbl_range_discrete()
# where yrs list would be replaced by start/end dates and source of data would
# be an additional input. Here the location of the data is assumed.
# import_and_append <- function(yrs, stats) {
#   i <- 1
#   while (i <= length(yrs)) {
#     yr <- yrs[i]
#     save_file_name_w_ext <- paste0("db_nba_tr_", yr)
#     stat_list <- vector("list", length(stats))
#     j <- 1
#     while (j <= length(stats)) {
#       stat <- stats[[j]]
#       ws_name <- substr(paste0("d_", stat), 1, 31)
#       tbl_stat_raw <- 
#         get_tbl_from_xlsx(ws_name = ws_name,
#                                save_dir = dir_data,
#                                save_file_name_w_ext = save_file_name_w_ext)
#       
#       tbl_stat <- tbl_stat_raw %>%
#         dplyr::select(one_of(c("team", "yr_start", stats[[j]])))
#       
#       if (j == 1) {
#         tbl_stat_join <- tbl_stat
#       } else {
#         tbl_stat_join <- inner_join(tbl_stat_join, tbl_stat, by = c("team", "yr_start"))
#       }
#     }
#     if (i == 1) {
#       tbl_yrs_rbind <- tbl_stat_join
#     } else {
#       tbl_yrs_rbind <- rbind(tbl_yrs_rbind, tbl_join)
#     }
#   }
#   return(tbl__yrs_rbind)
# }

# Function to join data. ####
import_and_append <- function(yrs, stats) {
    # stats <- stats_select
    cols_join <- c("team", "yr_start")
    m_stats <- matrix(NA, nrow = length(g_teams$team), ncol = length(stats) + length(cols_join))
    m_stats[, 1] <- sort(g_teams$team)
    m_yrs <- matrix(NA, nrow = length(yrs) * length(g_teams$team), ncol = length(stats) + length(cols_join))
    i <- 1
    while (i <= length(yrs)) {
        yr <- yrs[i]
        m_stats[, 2] <- yr
        
        j <- 1
        while (j <= length(stats)) {
            stat <- stats[[j]]
            filter_date <- get_end_of_rs_date(yr)
            ws_name <- substr(paste0("d_", stat), 1, 31)
            tbl_stat_raw <- get_tbl_from_xlsx(ws_name = ws_name, 
                                              filter_date_input = filter_date, 
                                              save_dir = dir_data, 
                                              save_file_name_w_ext = wb_name_w_ext)
            
            tbl_stat <- tbl_stat_raw %>% arrange(team) %>% dplyr::select(one_of(stat))
            
            m_stats[, j + 2] <- as.matrix(tbl_stat, dimnames = NULL)
            j <- j + 1
        }
        m_yrs[((i - 1) * length(g_teams$team) + 1):(i * length(g_teams$team)), ] <- m_stats
        i <- i + 1
    }
    tbl_yrs <- setNames(as_tibble(m_yrs), c(cols_join, stats))
    tbl_yrs_2 <- bind_cols(as_tibble(dplyr::select(tbl_yrs, team)), 
                           as_tibble(sapply(dplyr::select(tbl_yrs, -team), as.numeric)))
    try(tbl_yrs_2$team <- as.factor(tbl_yrs_2$team))
    return(tbl_yrs_2)
}


# Using the function. ####
rds_raw <- import_and_append(yrs, stats_temp)

# Don't think I need this anymore since it is done inside the function.
# rds <- bind_cols(as_tibble(dplyr::select(rds_raw, team)), as_tibble(sapply(dplyr::select(rds_raw, -team), as.numeric)))
# try(rds$team <- as.factor(rds$team))
file_name_w_ext <- "d_scraped_stats_raw.rds"
file_name_entire <- paste0(dir_data, file_name_w_ext)
if (file.exists(file_name_entire)) {
  # if (file_name_w_ext %in% list.files(dir_data)) {
  file_name_ext <- ".rds"
  file_name_new <- paste0(str_replace(file_name_entire, file_name_ext, ""), "_", 
                          format(Sys.time(), format = "%Y-%m-%d_%I-%M-%S"), 
                          file_name_ext)
  file.copy(from = file_name_entire, to = file_name_new)
}

saveRDS(rds_raw, file = file_name_entire)
