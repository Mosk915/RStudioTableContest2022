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

state_a_triangle_dev <- add_highlighting(state_a_triangle_dev)
state_a_triangle_dev$COLOR_USE <- FALSE

print(state_a_triangle_dev, width = 180)
