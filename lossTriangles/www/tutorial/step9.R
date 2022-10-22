fill_triangle <- function(data_triangle, factors) {
  
  for(i_col in 2:ncol(data_triangle)) {
    data_triangle[[i_col]] <- ifelse(is.na(data_triangle[[i_col]]), round(data_triangle[[i_col - 1]] * factors[[i_col - 1]], 0), data_triangle[[i_col]])
  }
  
  return(data_triangle)
  
}

factors <- state_a_triangle_summ[state_a_triangle_summ$AVG_METHOD == "Two Year",]
losses_filled <- fill_triangle(data_triangle = state_a_triangle, factors = factors)

print(losses_filled, width = 180)
