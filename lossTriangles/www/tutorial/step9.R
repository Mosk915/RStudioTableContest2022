fill_triangle <- function(data_triangle, factors) {
  
  # iterate through each column in the triangle data frame starting with the 2nd column
  for(i_col in seq_len(ncol(data_triangle) - 1) + 1) {
    # for each cell in the column that is missing, take the value in the column preceding it and multiply it by the appropriate development factor
    data_triangle[[i_col]] <- ifelse(is.na(data_triangle[[i_col]]), round(data_triangle[[i_col - 1]] * factors[[i_col - 1]], 0), data_triangle[[i_col]])
  }
  
  return(data_triangle)
  
}

factors <- state_a_triangle_summ[state_a_triangle_summ$AVG_METHOD == "Two Year",]
losses_filled <- fill_triangle(data_triangle = state_a_triangle, factors = factors)

print(losses_filled, width = 180)
