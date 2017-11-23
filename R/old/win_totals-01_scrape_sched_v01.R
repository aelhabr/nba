
rm(list = ls())
setwd("O:/_other/projects/nba/")

# Parameters.
library("stringr")
yr <- 2018
url_base <- "https://www.basketball-reference.com/leagues/"
url_subject <- str_c("NBA_", yr, "_games")
num_months <- 6
idx_month_start <- 10
# idx_month_end <- (idx_month_start + num_months) %% 12
# month.name[8]
dir_scrape <- "data/downloaded/"
export <- FALSE

library("dplyr")
library("rvest")
library("lubridate")

if (dir.exists(dir_scrape) == FALSE) {
	dir.create(dir_scrape)
}

i <- idx_month_start
while(i <= (idx_month_start + num_months)) {

	if((i %% 12 == 0)) {
		mmm <- str_to_lower(month.name[[12]])
	} else {
		mmm <- str_to_lower(month.name[[(i %% 12)]])
	}

		url <- str_c(url_base, url_subject, "-", mmm, ".html")
	filename_html <- str_c("sched_", yr, "_", i)
	filepath_html <-
		str_c(getwd(), "/", dir_scrape, filename_html, ".html")
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

	renamed <-
		raw %>%
		rename_all(str_to_lower)

	names(renamed) <-
		names(renamed) %>%
		str_replace_all("\\.", "") %>%
		str_replace_all("(et|neutral)", "")
	renamed

	selected <-
		renamed %>%
		select(-var7, -var8, -notes, -pts, -pts1, -start) %>%
		# rename(time = start) %>%
		rename(tm_away = visitor, tm_home = home) %>%
		select(date, tm_home, tm_away)
	selected

	cleaned <-
		selected %>%
		# mutate(time = hms(strftime(strptime(time, "%I:%M %p"), "%H:%M:%S"))) %>%
		mutate(date = ymd(strptime(date, "%a, %b %d, %Y")))
	cleaned

	if (i == idx_month_start) {
		all <- cleaned
	} else {
		all <- bind_rows(all, cleaned)
	}

	i <- i + 1
}
idx_cols_tms <- str_which(names(all), c("tm"))

output <- all

output[idx_cols_tms] <-
	lapply(output[idx_cols_tms],
				 function(d) {
				 	d %>%
				 		str_replace_all("Los Angeles Lakers", "LAL") %>%
				 		str_replace_all("Los Angeles Clippers", "LAC") %>%
				 		str_replace_all("New York Knicks", "NY") %>%
				 		str_replace_all("New Orleans Pelicans", "NO")
				 })
output <-
	output %>%
	mutate_at(vars(tm_home, tm_away), funs(str_to_upper(str_sub(., 1, 3))))

output[idx_cols_tms] <-
	lapply(output[idx_cols_tms],
				 function(d) {
				 	d %>%
				 		str_replace_all("BRO", "BKN") %>%
				 		str_replace_all("GOL", "GS") %>%
				 		str_replace_all("BRO", "BKN") %>%
				 		str_replace_all("GOL", "GS") %>%
				 		str_replace_all("OKL", "OKC") %>%
				 		str_replace_all("PHO", "PHX") %>%
				 		str_replace_all("SAN", "SA") %>%
				 		str_replace_all("UTA", "UTAH")
				 })
output %>% distinct(tm_home) %>% arrange(tm_home) %>% pull(tm_home)

if (export == TRUE) {
	filename_export <- "pfref_sched"
	dir_export <- dir_scrape
	filepath_export <-
		str_c(
			dir_export,
			filename_export,
			"_scraped_",
			format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
			".csv"
		)
	write.csv(output, file = filepath_export, row.names = FALSE)
}
