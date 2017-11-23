
remove(list = ls())
source("R/vars_global.R")
source("R/vars_stat_links.R")
source("R/save_html_from_url.R")
source("R/save_html_to_xlsx.R")
source("R/get_tbl_stat.R")
source("R/get_sched_dates.R")

# Variables to be used. #### TODO: Then integrate this into the get_d_stat() set of functions.

yrs <- 2007:2011
wb_name_w_ext <- "db_nba_tr.xlsx"
url_base_tr <- "https://www.teamrankings.com"
d_stat_links <- try(readRDS(paste0(dir_data, "d_stat_links.rds")))

scrape_rs_stats <- function(yrs, stats, links) {
    # stats <- stats_shooting links <- links_shooting
    i <- 1
    while (i <= length(yrs)) {
        yr <- yrs[i]
        j <- 1
        while (j <= length(stats)) {
            stat <- stats[[j]]
            link <- links[[j]]
            filter_date <- get_end_of_rs_date(yr)
            url <- paste0(url_base_tr, link, "?date=", filter_date)
            # htm_file_name_w_ext <- paste0(str_sub(url, 1, nchar(url) - 1), '.html') html_file_name_w_ext <- paste0(str_sub(link, 2, nchar(link)),
            # '.html')
            html_file_name_w_ext <- paste0("temp", ".html")
            html_file <- save_html_from_url(url = url, save_dir = dir_data, save_file_name_w_ext = html_file_name_w_ext)
            
            d_stat_raw <- get_tbl_stat_from_html(html_file)
            
            unlink(html_file, recursive = TRUE)
            d_stat <- process_tbl_stat(tbl_stat_raw = d_stat_raw, stat = stat, filter_date = filter_date)
            ws_name <- substr(paste0("d_", stat), 1, 31)
            d_stat_saved <- save_html_to_xlsx(tbl_stat = d_stat, filter_date_input = filter_date, ws_name = ws_name, save_dir = dir_data, 
                save_file_name_w_ext = wb_name_w_ext)
            
            # readline(prompt = 'Press [Enter] to continue.')
            j <- j + 1
        }
        i <- i + 1
    }
}

scrape_rs_stats(yrs, stats_temp, links_temp)



