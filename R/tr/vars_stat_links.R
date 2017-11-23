
d_stat_links <- readRDS(paste0(dir_data, "d_stat_links.rds"))

rows_select <- c(1, 2, 3, 26:27, 30, 41:42, 51:52, 56:57, 60, 71)
rows_non_opp <- c(1:68)

# d_link2stat_scoring <- d_stat_links %>% 
#   filter(category == 'Scoring') %>% 
#   dplyr::select(link_raw, stat)
# d_link2stat_scoring[[1]][[1]]

links_scoring <- d_stat_links$link_raw[d_stat_links$category == "Scoring"]
links_select <- d_stat_links$link_raw[rows_select]
links_non_opp <- d_stat_links$link_raw[rows_non_opp]
links_temp <- d_stat_links$link_raw[c(2, 23, 135)]

stats_scoring <- d_stat_links$stat[d_stat_links$category == "Scoring"]
stats_select <- d_stat_links$stat[rows_select]
stats_non_opp <- d_stat_links$stat[rows_non_opp]
stats_temp <- d_stat_links$stat[c(2, 23, 135)]
