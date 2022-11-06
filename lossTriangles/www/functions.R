add_highlighting <- function(data_triangle) {
  
  # iterate through each column in the triangle data frame starting with the 2nd column
  for(idx in seq_len(ncol(data_triangle) - 1)) {
    # determine the color gradient based on the number of non-empty cells in the column
    colors <- colorRampPalette(c("#5A8AC6", "#FCFCFF", "#F8696B"))(nrow(data_triangle) + 1 - idx)
    
    # assign a rank to each color
    color_lookup <- data.frame(RANK = seq_len(nrow(data_triangle) + 1 - idx),
                               VALUE = colors)
    
    # assign a rank to each value in the column and join the colors to set the background color for each cell
    data_triangle <- data_triangle %>%
      mutate(RANK = rank(!!sym(colnames(data_triangle)[idx + 1]), na.last = "keep", ties.method = "max")) %>%
      left_join(color_lookup, by = "RANK") %>%
      rename(!!sym(paste0(colnames(data_triangle)[idx + 1], "_COLOR")) := VALUE) %>%
      select(-RANK)
  }
  
  return(data_triangle)
  
}

fill_triangle <- function(data_triangle, factors) {
  
  # iterate through each column in the triangle data frame starting with the 2nd column
  for(i_col in seq_len(ncol(data_triangle) - 1) + 1) {
    # for each cell in the column that is missing, take the value in the column preceding it and multiply it by the appropriate development factor
    data_triangle[[i_col]] <- ifelse(is.na(data_triangle[[i_col]]), round(data_triangle[[i_col - 1]] * factors[[i_col - 1]], 0), data_triangle[[i_col]])
  }
  
  return(data_triangle)
  
}

DTLossData <- function(data) {
  
  table <- datatable(data = data,
                     escape = FALSE,
                     rownames = FALSE,
                     filter = "none",
                     selection = "none",
                     class = "stripe compact",
                     colnames = c("Year" = "YEAR",
                                  "Months" = "MONTHS",
                                  "Loss" = "LOSS"),
                     options = list(paging = FALSE,
                                    searching = FALSE,
                                    ordering = FALSE,
                                    info = FALSE,
                                    scrollY = 300,
                                    columnDefs = list(list(width = "33%", targets = 0),
                                                      list(width = "33%", targets = 1),
                                                      list(width = "33%", targets = 2))))
  
  table <- table %>%
    formatCurrency(columns = "Loss",
                   currency = "",
                   interval = 3,
                   digits = 0,
                   mark = ",") %>%
    formatStyle(columns = c("Year", "Months", "Loss"),
                fontSize = "10px")
  
}

DTLossTriangle <- function(data) {
  
  table <- datatable(data = data,
                     escape = FALSE,
                     rownames = FALSE,
                     filter = "none",
                     selection = "none",
                     class = "cell-border compact",
                     colnames = c("Year" = "YEAR",
                                  "12" = "MONTHS_12",
                                  "24" = "MONTHS_24",
                                  "36" = "MONTHS_36",
                                  "48" = "MONTHS_48",
                                  "60" = "MONTHS_60",
                                  "72" = "MONTHS_72",
                                  "84" = "MONTHS_84",
                                  "96" = "MONTHS_96",
                                  "108" = "MONTHS_108",
                                  "120" = "MONTHS_120"),
                     options = list(paging = FALSE,
                                    searching = FALSE,
                                    ordering = FALSE,
                                    info = FALSE,
                                    columnDefs = list(list(width = "10%", targets = 0),
                                                      list(width = "9%", targets = 1),
                                                      list(width = "9%", targets = 2),
                                                      list(width = "9%", targets = 3),
                                                      list(width = "9%", targets = 4),
                                                      list(width = "9%", targets = 5),
                                                      list(width = "9%", targets = 6),
                                                      list(width = "9%", targets = 7),
                                                      list(width = "9%", targets = 8),
                                                      list(width = "9%", targets = 9),
                                                      list(width = "9%", targets = 10),
                                                      list(className = "dt-body-right", targets = 1:10),
                                                      list(className = "dt-body-center", targets = 0),
                                                      list(className = "dt-head-center", targets = "_all"))))
  
  table <- table %>%
    formatCurrency(columns = c("12", "24", "36", "48", "60", "72", "84", "96", "108", "120"),
                   currency = "",
                   interval = 3,
                   digits = 0,
                   mark = ",") %>%
    formatStyle(columns = c("Year", "12", "24", "36", "48", "60", "72", "84", "96", "108", "120"),
                fontSize = "10px")
  
}

DTDevTriangle <- function(data) {
  
  table <- datatable(data = data,
                     escape = FALSE,
                     rownames = FALSE,
                     filter = "none",
                     selection = "none",
                     class = "cell-border compact",
                     colnames = c("Year" = "YEAR",
                                  "12:24" = "MONTHS_12_24",
                                  "24:36" = "MONTHS_24_36",
                                  "36:48" = "MONTHS_36_48",
                                  "48:60" = "MONTHS_48_60",
                                  "60:72" = "MONTHS_60_72",
                                  "72:84" = "MONTHS_72_84",
                                  "84:96" = "MONTHS_84_96",
                                  "96:108" = "MONTHS_96_108",
                                  "108:120" = "MONTHS_108_120"),
                     options = list(paging = FALSE,
                                    searching = FALSE,
                                    ordering = FALSE,
                                    info = FALSE,
                                    columnDefs = list(list(width = "10%", targets = 0),
                                                      list(width = "10%", targets = 1),
                                                      list(width = "10%", targets = 2),
                                                      list(width = "10%", targets = 3),
                                                      list(width = "10%", targets = 4),
                                                      list(width = "10%", targets = 5),
                                                      list(width = "10%", targets = 6),
                                                      list(width = "10%", targets = 7),
                                                      list(width = "10%", targets = 8),
                                                      list(width = "10%", targets = 9),
                                                      list(visible = FALSE, targets = 10:19),
                                                      list(className = "dt-body-right", targets = 1:9),
                                                      list(className = "dt-body-center", targets = 0),
                                                      list(className = "dt-head-center", targets = "_all"))))
  
  table <- table %>%
    formatCurrency(columns = c("12:24", "24:36", "36:48", "48:60", "60:72", "72:84", "84:96", "96:108", "108:120"),
                   currency = "",
                   interval = 3,
                   digits = 3,
                   mark = ",") %>%
    formatStyle(columns = c("Year", "12:24", "24:36", "36:48", "48:60", "60:72", "72:84", "84:96", "96:108", "108:120"),
                fontSize = "10px")
  
  return(table)
  
}

DTDevTriangleFormatted <- function(data) {
  
  table <- datatable(data = data,
                     escape = FALSE,
                     rownames = FALSE,
                     filter = "none",
                     selection = list(mode = "single", target = "cell"),
                     class = "",
                     colnames = c("Year" = "YEAR",
                                  "12:24" = "MONTHS_12_24",
                                  "24:36" = "MONTHS_24_36",
                                  "36:48" = "MONTHS_36_48",
                                  "48:60" = "MONTHS_48_60",
                                  "60:72" = "MONTHS_60_72",
                                  "72:84" = "MONTHS_72_84",
                                  "84:96" = "MONTHS_84_96",
                                  "96:108" = "MONTHS_96_108",
                                  "108:120" = "MONTHS_108_120"),
                     options = list(paging = FALSE,
                                    searching = FALSE,
                                    ordering = FALSE,
                                    info = FALSE,
                                    columnDefs = list(list(width = "10%", targets = 0),
                                                      list(width = "10%", targets = 1),
                                                      list(width = "10%", targets = 2),
                                                      list(width = "10%", targets = 3),
                                                      list(width = "10%", targets = 4),
                                                      list(width = "10%", targets = 5),
                                                      list(width = "10%", targets = 6),
                                                      list(width = "10%", targets = 7),
                                                      list(width = "10%", targets = 8),
                                                      list(width = "10%", targets = 9),
                                                      list(visible = FALSE, targets = 10:19),
                                                      list(className = "dt-body-right", targets = 1:9),
                                                      list(className = "dt-body-center", targets = 0),
                                                      list(className = "dt-head-center", targets = "_all")),
                                    rowCallback = JS(readLines("www/rowCallback.js")),
                                    headerCallback = JS(readLines("www/headerCallback.js"))),
                     callback = JS(readLines("www/callback.js")))
  
  table <- table %>%
    formatCurrency(columns = c("12:24", "24:36", "36:48", "48:60", "60:72", "72:84", "84:96", "96:108", "108:120"),
                   currency = "",
                   interval = 3,
                   digits = 3,
                   mark = ",")
  
  return(table)
  
}

DTAverages <- function(data) {
  
  table <- datatable(data = data,
                     escape = FALSE,
                     rownames = FALSE,
                     filter = "none",
                     selection = "none",
                     class = "stripe",
                     colnames = c("Method" = "AVG_METHOD",
                                  "12:24" = "MONTHS_12_24",
                                  "24:36" = "MONTHS_24_36",
                                  "36:48" = "MONTHS_36_48",
                                  "48:60" = "MONTHS_48_60",
                                  "60:72" = "MONTHS_60_72",
                                  "72:84" = "MONTHS_72_84",
                                  "84:96" = "MONTHS_84_96",
                                  "96:108" = "MONTHS_96_108",
                                  "108:120" = "MONTHS_108_120"),
                     options = list(paging = FALSE,
                                    searching = FALSE,
                                    ordering = FALSE,
                                    info = FALSE,
                                    columnDefs = list(list(width = "10%", targets = 0),
                                                      list(width = "10%", targets = 1),
                                                      list(width = "10%", targets = 2),
                                                      list(width = "10%", targets = 3),
                                                      list(width = "10%", targets = 4),
                                                      list(width = "10%", targets = 5),
                                                      list(width = "10%", targets = 6),
                                                      list(width = "10%", targets = 7),
                                                      list(width = "10%", targets = 8),
                                                      list(width = "10%", targets = 9),
                                                      list(className = "dt-body-right", targets = 1:9),
                                                      list(className = "dt-body-center", targets = 0),
                                                      list(className = "dt-head-center", targets = "_all"))),
                     caption = tags$caption(style = "caption-side: bottom;",
                                            HTML("<i>If there are not enough factors in a column to calculate a specified average, an all year average is used.</i>")))
  
  table <- table %>%
    formatCurrency(columns = c("12:24", "24:36", "36:48", "48:60", "60:72", "72:84", "84:96", "96:108", "108:120"),
                   currency = "",
                   interval = 3,
                   digits = 3,
                   mark = ",")
  
  return(table)
  
}

DTLossTriangleFilled <- function(data) {
  
  table <- datatable(data = data,
                     escape = FALSE,
                     rownames = FALSE,
                     filter = "none",
                     selection = "none",
                     class = "",
                     colnames = c("Year" = "YEAR",
                                  "12" = "MONTHS_12",
                                  "24" = "MONTHS_24",
                                  "36" = "MONTHS_36",
                                  "48" = "MONTHS_48",
                                  "60" = "MONTHS_60",
                                  "72" = "MONTHS_72",
                                  "84" = "MONTHS_84",
                                  "96" = "MONTHS_96",
                                  "108" = "MONTHS_108",
                                  "120" = "MONTHS_120"),
                     options = list(paging = FALSE,
                                    searching = FALSE,
                                    ordering = FALSE,
                                    info = FALSE,
                                    columnDefs = list(list(width = "10%", targets = 0),
                                                      list(width = "9%", targets = 1),
                                                      list(width = "9%", targets = 2),
                                                      list(width = "9%", targets = 3),
                                                      list(width = "9%", targets = 4),
                                                      list(width = "9%", targets = 5),
                                                      list(width = "9%", targets = 6),
                                                      list(width = "9%", targets = 7),
                                                      list(width = "9%", targets = 8),
                                                      list(width = "9%", targets = 9),
                                                      list(width = "9%", targets = 10),
                                                      list(className = "dt-body-right", targets = 1:10),
                                                      list(className = "dt-body-center", targets = 0),
                                                      list(className = "dt-head-center", targets = "_all")),
                                    rowCallback = JS(readLines("www/rowCallback_filled.js"))),
                     caption = tags$caption(style = "caption-side: bottom;",
                                            HTML("<i>Italicized values are estimates.</i>")))
  
  table <- table %>%
    formatCurrency(columns = c("12", "24", "36", "48", "60", "72", "84", "96", "108", "120"),
                   currency = "",
                   interval = 3,
                   digits = 0,
                   mark = ",")
  
  return(table)
  
}