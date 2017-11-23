
rm(list = ls())
setwd("O:/_other/projects/nba/")

# Packages. ----
library("dplyr")
library("stringr")
library("rvest")
library("lubridate")

# Parameters. ----
yyyy_start <- 2017
yyyy_end <- 2017

url_base <- "https://www.basketball-reference.com/leagues/"
# url_base <- "https://www.basketball-reference.com/play-index/tgl_finder.cgi?request=1&match=game&lg_id=NBA&is_playoffs=N&team_seed_cmp=eq&opp_seed_cmp=eq&year_min=2011&year_max=2017&is_range=N&game_num_type=team&c1stat=fg&c1comp=gt&c2stat=diff_fg&c2comp=gt&c3stat=off_rtg&c3comp=gt&c4stat=diff_off_rtg&c4comp=gt&order_by=date_game&order_by_asc=Y&offset="

months_per_season <- 6
idx_month_start <- 10
dir_scrape <- "data/"
export <- TRUE

if (export == TRUE) {
  filename_export_base <- "sched"
  filename_export_suffix <- "_scraped"
  filename_export_ext <- ".csv"
  dir_export <- "data/"
  filepath_export <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      filename_export_ext
    )
  filepath_export_ts <-
    str_c(
      dir_export,
      filename_export_base,
      filename_export_suffix,
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      filename_export_ext
    )
}

# if (dir.exists(dir_scrape) == FALSE) {
# 	dir.create(dir_scrape)
# }

# Cleaning function.
process_raw <- function(d, mm, yyyy) {
  
  # d <- raw
  # mm <- 4
  # yyyy <- 2010
  renamed <-
    d %>%
    rename_all(str_to_lower)
  
  names(renamed) <-
    names(renamed) %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("(et|neutral)", "")
  renamed
  
  selected <-
    renamed %>%
    select(-var7, -var8, -notes, -start)
  
  selected <-
    selected %>%
    # rename(time = start) %>%
    rename(
      tm_away = visitor,
      tm_home = home,
      pts_away = pts,
      pts_home = pts1
    ) %>%
    select(date, tm_home, tm_away, pts_away, pts_home)
  selected
  
  cleaned <-
    selected %>%
    # mutate(time = hms(strftime(strptime(time, "%mm_i:%M %p"), "%H:%M:%S"))) %>%
    mutate(date = ymd(strptime(date, "%a, %b %d, %Y"))) %>% 
    mutate(season = yyyy)
  cleaned
  
  if((mm %% 12) == 4) {
    cleaned <-
      cleaned %>% 
      slice(seq_len(min(which(tm_home == "Playoffs")) - 1)) %>% 
      mutate_at(vars(pts_away, pts_home), funs(as.numeric))
  }
  cleaned
}

# Loop. ----

yyyy_i <- yyyy_start

while(yyyy_i <= yyyy_end) {
  # This will change depending on what is being scraped.
	url_subject <- str_c("NBA_", yyyy_i, "_games")

	mm_i <- idx_month_start
	
	# Make an exception for lockout season(s).
	if(yyyy_i == 2012) {
		mm_i <- 12
	}
	while(mm_i <= (idx_month_start + months_per_season)) {

		if((mm_i %% 12 == 0)) {
			mmm <- str_to_lower(month.name[[12]])
		} else {
			mmm <- str_to_lower(month.name[[(mm_i %% 12)]])
		}

		url <- str_c(url_base, url_subject, "-", mmm, ".html")
		# filename_html <- str_c("temp_", yyyy_i, "_", mm_i)
		filename_html <- str_c("temp")
		filepath_html <- str_c(dir_scrape, filename_html, ".html")
		download.file(url, destfile = filepath_html)

		raw <-
			filepath_html %>%
			read_html() %>%
			html_nodes("table") %>%
			html_table(header = TRUE) %>%
			as.data.frame() %>%
			tbl_df()
		raw
		
		if (nrow(raw) == 0)
		  break
		
		cleaned <- process_raw(raw, mm_i, yyyy_i)
		cleaned

		if (mm_i == idx_month_start) {
			mm_all <- cleaned
		} else {
			mm_all <- bind_rows(mm_all, cleaned)
		}

		mm_i <- mm_i + 1
	}
	if (yyyy_i == yyyy_start) {
		yyyy_all <- mm_all
	} else {
		yyyy_all <- bind_rows(yyyy_all, mm_all)
	}

	yyyy_i <- yyyy_i + 1
}

output <- yyyy_all

if (export == TRUE) {
	write_csv(output, filepath_export)
  write_csv(output, filepath_export_ts)
}
