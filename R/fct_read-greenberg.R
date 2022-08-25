
read_greenberg <- function(file){
    
    raw_data <- fread(file, skip = 68)
    header_data <- fread(file, nrows = 67, header = FALSE)
    
    header_line_numbers <- c(15, 18, 20, 22, 24)
    header_data <- header_data[header_line_numbers,]
    options <- as.list(as.numeric(header_data$V2))
    names(options) <- c("hz", "pn_nm1", "pn_nm2", "nm_v1", "nm_v2")

    list(data = raw_data,
         options = as.data.frame(options))

}


plot_greenberg_raw_data <- function(x){
    
    d <- data.table(x = 1:nrow(x$data)/x$options[["hz"]],
                    bead1 = x$data$Trap1X,
                    bead2 = x$data$Trap2X)
               
    dygraphs::dygraph(data = d) |> dyRangeSelector()
}  


 
