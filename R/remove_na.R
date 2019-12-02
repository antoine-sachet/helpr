#' Remove rows NAs from selected columns
#'
#' Remove rows having NA in one of the selected columns. Suitable for piping. 
#' Compatible with dply's select helpers.
#' @param df Data.frame to process
#' @param ... Column selection passed to `dplyr::select`
#'
#' @importFrom dplyr select
#' @importFrom stats complete.cases
#' @seealso keep_na, keep_na_rows
#' 
#' @examples 
#' 
#' df <- data.frame(a = 1:10, b = c(1:3, na, 1:6))
#' remove_na()
#' 
#' @export
remove_na <- function(df, ...) {
  df[stats::complete.cases(dplyr::select(df, ...)),]
}


#' Keep rows with NAs in selected columns
#'
#' Keep only rows with at least one NA in one of the selected columns. 
#' Suitable for piping. Compatible with dply's select helpers.
#'
#' @param df Data.frame to process
#' @param ... Column selection passed to `dplyr::select`
#'
#' @importFrom dplyr select
#' @importFrom stats complete.cases
#' @export
keep_na <- function(df, ...) {
  df[!stats::complete.cases(dplyr::select(df, ...)),]
}


#' Keep only rows containing at least one NA
#'
#' Useful for quick NA checks. Equivalent to `keep_na(df, everything())`
#'
#' @param df Data.frame to process
#'
#' @importFrom stats complete.cases
#' @export
keep_na_rows <- function(df) {
  df[!stats::complete.cases(df),]
}





