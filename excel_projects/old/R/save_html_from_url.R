
source("R/vars_global.R")

# Main functions. ####
save_html_from_url <- function(url = NULL, save_dir = NULL, save_file_name_w_ext = NULL) {
    
    if (is.null(url) || is.null(save_dir) || is.null(save_file_name_w_ext)) {
        stop()
    }
    
    # msg_end <- paste0('HTML contents retrieved from\n', url, '.\nHTML contents saved as ', save_file_name_w_ext, '\nin ', save_dir, '.\n')
    # msg_success <- paste0('Succesfully donwloaded ', msg_end) msg_error <- paste0('Could not download ', msg_end)
    
    # tryCatch(download.file(url, destfile = paste0(save_dir, save_file_name_w_ext)), error = message(msg_error), finally =
    # message(msg_success))
    
    # try(download.file(url_final, destfile = paste0(save_dir, save_file_name_w_ext)))
    download.file(url, destfile = paste0(save_dir, save_file_name_w_ext))
    
    return(paste0(save_dir, save_file_name_w_ext))
}




