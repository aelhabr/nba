
source("R/globals.R")

# Functions for plotting. ####
save_ggplot_as_png <- function(plot_name = NULL, save_dir = NULL) {
    
    if (is.null(plot_name) || is.null(save_dir)) {
        stop(generic_error_string)
    }
    
    if (!(dir.exists(save_dir))) {
        dir.create(save_dir)
        message(paste0("save_dir ", save_dir, "does not exist. It will be created.\n"))
    }
    
    dir_files <- list.files(save_dir)
    # print(dir_files)
    if (!(plot_name %in% dir_files)) {
        message(paste0("\n", plot_name, " does not already exist in \n", save_dir, ".\n"))
        message("It will be created.\n")
    } else {
        message(paste0("\n", plot_name, " already exists in \n", save_dir, ".\n"))
        message("It will be overwritten.\n")
    }
    
    ggsave(paste0(save_dir, deparse(substitute(plot_name)), ".png"), width = 11, height = 7, units = "in")
}
