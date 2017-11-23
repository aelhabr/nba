
# remove(list = ls()) Only uncomment the following line if workiing with this file directly since tr_functions.R calls this file.
# source('R/functions_main.R')

# interface function get_tbl_stat(date_range_type = NULL, ...)  valid inputs for date_range_type include for_range_cont, for_range_discrete,
# and for_day

# if date_range_type == for_range_cont, then get_tbl_stat() calls get_tbl_stat_for_range_cont (date_start, date_end, customize_names =
# FALSE, ...) which calls get_tbl_stat_for_day(stat = NULL, filter_date = NULL, html_file = NULL, ...)

# if date_range_type == for_range_discrete, then get_tbl_stat() calls get_tbl_stat_for_range_discrete (season_period = thru_rs,
# customize_name = FALSE, ...)  valid input for season_period include thru_rs, thru_plyffs, and only_plyffs

# if season_period == thru_rs, then get_tbl_stat_for_range_discrete() calls get_end_of_rs_date(year = NULL, month = 4) and then calls
# get_tbl_stat_for_day(stat = NULL, filter_date = NULL, html_file = NULL, ...)

# if season_period == thru_plyffs, then get_tbl_stat_for_range_discrete() calls get_end_of_plyffs_date(year = NULL, month = 6) and then
# calls get_tbl_stat_for_day(stat = NULL, filter_date = NULL, html_file = NULL, ...)

# if season_period == only_plyffs, then get_tbl_stat_for_range_discrete() calls get_end_of_rs_date(year = NULL, month = 6) and then calls
# get_end_of_plyffs_date(year = NULL, month = 6) and then calls ?...

# in any case, if customize_names == TRUE, then get_tbl_stat_for_day() calls get_tbl_stat_for_day_custom(char_stat = NULL, replace_stat_dash
# = TRUE, abbreviate_stat = TRUE, rename_rank = TRUE, rename_home_away = TRUE, ...)

# inputs only for for_range function: yr_start = TRUE, yr_end, include_rs = TRUE, include_plyffs = FALSE

# Globals. ####
generic_error_string <- "Invalid required input."
generic_abort_string <- "Function aborted."


# Core function. ####
get_tbl_stat_from_html <- function(html_file = NULL) {
    
    tbl_stat <- html_file %>% read_html %>% html_nodes("table") %>% html_table(header = TRUE) %>% data.frame() %>% tbl_df()
    return(tbl_stat)
}

# Wrapper functions for core function. #### get_tbl_stat_for_day <- function(stat = NULL, filter_date = NULL, ...) { if(is.null(stat) ||
# is.null(filter_date)) { stop() } html_file <- '' # Need to fix this.  tbl_stat <- get_tbl_stat_from_html(html_file)
# process_tbl_stat(customize_names) return(tbl) }

g_teams <- try(readRDS(paste0(dir_data, "g_teams.rds")))
# Function to process results of core function. ####
process_tbl_stat <- function(tbl_stat_raw = NULL, stat = NULL, filter_date = NULL) {
    
    # This check is only necessary if this function is called directly.
    if (is.null(tbl_stat_raw) || is.null(stat) || is.null(filter_date)) {
        stop(generic_error_string)
    }
    
    # Select only important columns.
    yr <- as.numeric(str_split(filter_date, "-")[[1]][1])
    char_yr <- as.character(paste0("X", yr - 1))
    cols <- c("Rank", "Team", char_yr, "Home", "Away")
    tbl_stat <- dplyr::select(tbl_stat_raw, one_of(cols))
    
    # Replace common unwanted things in column headers.  names(tbl_stat) <- gsub(names(tbl_stat), '\\%|/','\\.')
    
    
    # Rename some existing columns.
    char_stat <- as.character(stat)
    tbl_stat <- tbl_stat %>% setNames(gsub(char_yr, char_stat, names(.)))
    
    char_rank <- paste0(char_stat, "_", "rank")
    tbl_stat <- tbl_stat %>% setNames(gsub("Rank", char_rank, names(.)))
    
    char_home <- paste0(char_stat, "_", "h")
    char_away <- paste0(char_stat, "_", "a")
    tbl_stat <- tbl_stat %>% setNames(gsub("Home", char_home, names(.))) %>% setNames(gsub("Away", char_away, names(.)))
    
    tbl_stat <- setNames(tbl_stat, tolower(names(tbl_stat)))
    
    # Manipulate some existing data.
    tbl_stat$team <- plyr::mapvalues(tbl_stat$team, g_teams$team_name, g_teams$team)
    
    # TODO: Make this more dynamic so that it detects any column, not just those specified.
    tbl_stat[3:5] <- lapply(tbl_stat[3:5], function(x) as.numeric(gsub("%", "", x)))
    
    # Add some columns.
    time_period <- "rs"
    if (as.Date(filter_date) > as.Date(get_end_of_rs_date(yr))) {
        time_period <- "rs and plyffs"
    }
    
    tbl_stat <- tbl_stat %>% mutate(yr_start = yr - 1, yr_end = yr, time_period = time_period, filter_date = filter_date, retrieve_time = lubridate::now())
    
    return(tbl_stat)
}


# Interface function. #### get_tbl_stat <- function(date_range_type = NULL, ...) { valid_types <- c('for_day', 'for_range_cont',
# 'for_range_discrete') # Not currently using this.  type_funcs <- c(get_tbl_stat_for_day, get_tbl_stat_for_range_cont,
# get_tbl_stat_for_range_discrete) if(!date_range_type %in% valid_types) { cat(generic_error_string) cat(paste0('Please enter on of ,',
# valid_types, '.\n')) stop(generic_abort_string) } else { if(date_range_type == valid_types[1]) { tbl <- get_tbl_stat_for_day(...)  } else
# if (date_range_type == valid_types[2]) { tbl <- get_tbl_stat_for_range_cont(...)  } else if (date_range_type == valid_types[3]) { tbl <-
# get_tbl_stat_for_range_discrete(...)  } else { stop('Unexpected error.\n') } return(tbl) } }





