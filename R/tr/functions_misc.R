
# source('tr_functions.R') from http://adv-r.had.co.nz/Exceptions-Debugging.html
show_condition <- function(code) {
    tryCatch(code, error = function(c) "error", warning = function(c) "warning", message = function(c) "message")
}


delete_dirs <- function(input_dir) {
    # lapply(list.dirs(), unlink, recursive = TRUE)
    lapply(input_dir, unlink, recursive = TRUE)
}

# Does the same thing as delete_dirs.
delete_files <- function(input_dir) {
    # lapply(input_dir, file.remove, recursive = TRUE)
    lapply(input_dir, unlink, recursive = TRUE)
}

create_dirs <- function(folder_list) {
    lapply(folder_list, dir.create)
}

create_files <- function(folder_list) {
    lapply(folder_list, file.create)
}
