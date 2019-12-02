confmat <- function(vec1, vec2) {
  list(sum(vec1 %in% vec2), length(setdiff(vec1, vec2)), 
       length(setdiff(vec2, vec1)), sum(vec2 %in% vec1)) %>%
    matrix(nrow = 2, byrow = T)
}

