#' Compare elements from 2 sets
#' 
#' Create a 2x2 matrix showing the counts of common and different elements
#' 
#' @param vec1 Vector 1
#' @param vec2 Vector 2
#' 
#' @examples set_compare(1:5, 4:10)
#' 
set_compare <- function(vec1, vec2) {
  list(sum(vec1 %in% vec2), length(setdiff(vec1, vec2)), 
       length(setdiff(vec2, vec1)), sum(vec2 %in% vec1)) %>%
    matrix(nrow = 2, byrow = T)
}

