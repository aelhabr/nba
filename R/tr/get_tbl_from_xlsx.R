
source("R/vars_global.R")

get_tbl_from_xlsx <- function(ws_name = NULL, 
                              filter_date_input = NULL, 
                              save_dir = NULL, 
                              save_file_name_w_ext = NULL) {
    
    if (is.null(ws_name) || 
        is.null(filter_date_input) || 
        is.null(save_dir) || 
        is.null(save_file_name_w_ext)) {
        stop()
    }
    
    dir_files <- list.files(save_dir)
    if (!(save_file_name_w_ext %in% dir_files)) {
        stop(paste0(save_file_name_w_ext, " does not exist in \n", save_dir, "."))
    } else {
        wb <- loadWorkbook(file = paste0(save_dir, save_file_name_w_ext))  # same function for xlsx and openxlsx packages
        msg_exists_ws_not <- paste0("\n", ws_name, " does not exist in \n", save_file_name_w_ext, ".\n")
        
        if (xlsx_package == "xlsx") {
            if (!(ws_name %in% names(xlsx::getSheets(wb)))) {
                message(msg_exists_ws_not)
                message(names(xlsx::getSheets(wb)))
                stop()
            }
        } else if (xlsx_package == "openxlsx") {
            if (!(ws_name %in% names(wb))) {
                message(msg_exists_ws_not)
                message(names(wb))
                stop()
            }
        }
        # message(msg_exist)
        ws <- readxl::read_excel(paste0(save_dir, save_file_name_w_ext), sheet = ws_name)
        filter_date_exists <- filter_date_input %in% unique(ws$filter_date)
        if (!filter_date_exists) {
            stop("Data for ", filter_date_input, " does not exist in ", ws_name, ".")
        } else {
            tbl_output <- filter(ws, filter_date == as.Date(filter_date_input))
            return(tbl_output)
        }
    }
}
