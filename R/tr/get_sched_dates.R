

# Helper functions for dates. ####
get_start_of_rs_date <- function(yr_start = NULL, month = 10) {
    date_start <- paste(yr_start + 1, str_pad(as.character(month), 2, pad = "0"), "01", sep = "-")
    date_end <- paste(yr_start + 1, str_pad(as.character(month + 1), 2, pad = "0"), "01", sep = "-")
    date_range = as.POSIXlt(seq(as.Date(date_start), as.Date(date_end), "day"))
    days_wday <- date_range[date_range$wday == 4]
    days_wday_2 <- days_wday[ave(days_wday$mon, list(days_wday$mon, days_wday$year), FUN = seq_along) == 4]
    return(format(as.Date(days_wday_2), format = "%Y-%m-%d"))
}


get_end_of_rs_date <- function(yr_start = NULL, month = 4) {
    date_start <- paste(yr_start + 1, str_pad(as.character(month), 2, pad = "0"), "01", sep = "-")
    date_end <- paste(yr_start + 1, str_pad(as.character(month + 1), 2, pad = "0"), "01", sep = "-")
    date_range = as.POSIXlt(seq(as.Date(date_start), as.Date(date_end), "day"))
    days_wday <- date_range[date_range$wday == 4]
    days_wday_2 <- days_wday[ave(days_wday$mon, list(days_wday$mon, days_wday$year), FUN = seq_along) == 3]
    return(format(as.Date(days_wday_2), format = "%Y-%m-%d"))
}


get_end_of_plyffs_date <- function(yr_start = NULL) {
    if (!is.numeric(yr_start)) {
        cat("yr_start must be a numeric.\n")
        stop()
    }
    # date_final <- paste(yr_start + 1, str_pad(as.character(6), 2, pad = '0'), '30', sep = '-') return(format(as.Date(date_final), format =
    # '%Y-%m-%d'))
    paste(yr_start + 1, str_pad(as.character(6), 2, pad = "0"), "30", sep = "-")
}
