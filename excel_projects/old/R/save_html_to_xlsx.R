
source("R/vars_global.R")

save_html_to_xlsx <- function(tbl_stat = NULL, 
                              filter_date_input = NULL, 
                              ws_name = NULL, save_dir = NULL, 
                              save_file_name_w_ext = NULL) {
    if (is.null(tbl_stat) || 
        is.null(filter_date_input) || 
        is.null(ws_name) || 
        is.null(save_dir) || 
        is.null(save_file_name_w_ext)) {
        stop()
    }
    
    dir_files <- list.files(save_dir)
    # print(dir_files)
    if (!(save_file_name_w_ext %in% dir_files)) {
        message(paste0(save_file_name_w_ext, " does not already exist in \n", save_dir, ".\nIt will be created."))
        wb <- createWorkbook()  # same function for xlsx and openxlsx packages
    } else {
        message(paste0(save_file_name_w_ext, " already exists in \n", save_dir, ".\nNew data will be added to this workbook."))
        wb <- loadWorkbook(file = paste0(save_dir, save_file_name_w_ext))  # same function for xlsx and openxlsx packages
    }
    msg_add_ws <- paste0(ws_name, " does not exist in \n", save_file_name_w_ext, ".\nAdding worksheet.")
    msg_exists_ws <- paste0(ws_name, " already exists in \n", save_file_name_w_ext, ".")
    
    if (xlsx_package == "xlsx") {
        if (!(ws_name %in% names(xlsx::getSheets(wb)))) {
            message(msg_add_ws)
            ws <- xlsx::createSheet(wb, ws_name)
            xlsx::addDataFrame(data.frame(tbl_stat), ws, startColumn = 1)
            xlsx::saveWorkbook(wb, file = paste0(save_dir, save_file_name_w_ext))
            return(tbl_stat)
        } else {
            message(msg_exists_ws)
        }
    } else if (xlsx_package == "openxlsx") {
        if (!(ws_name %in% names(wb))) {
            message(msg_add_ws)
            openxlsx::addWorksheet(wb, ws_name)
            openxlsx::writeData(wb, ws_name, as.data.frame(tbl_stat))
            openxlsx::saveWorkbook(wb, file = paste0(save_dir, save_file_name_w_ext), overwrite = TRUE)
            return(tbl_stat)
        } else {
            message(msg_exists_ws)
        }
    }
    
    msg_add_tbl <- paste0("New data appended to ", ws_name, " in \n", save_file_name_w_ext, ".")
    msg_add_tbl_no <- paste0("Data not written to ", ws_name, " because data for ", filter_date_input, " already exists.")
    
    ws <- readxl::read_excel(paste0(save_dir, save_file_name_w_ext), sheet = ws_name)
    filter_date_exists <- unique(tbl_stat$filter_date) %in% unique(ws$filter_date)
    if (!filter_date_exists) {
        tbl_output <- arrange(bind_rows(ws, tbl_stat), filter_date)
        if (xlsx_package == "openxlsx") {
            openxlsx::writeData(wb, ws_name, as.data.frame(tbl_output))
            openxlsx::saveWorkbook(wb, file = paste0(save_dir, save_file_name_w_ext), overwrite = TRUE)
        }
        message(msg_add_tbl)
        return(tbl_output)
    } else {
        message(msg_add_tbl_no)
        return(ws)
    }
}
