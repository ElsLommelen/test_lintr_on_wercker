#' @title Selecteert indicatoren LSVI op basis van de opgegeven parameters
#'
#' @description Some description
#'
#' @param Connectie Connection to the SQLite database in the package
#'
#' @return A dataframe
#'
#' @export
#'
#'
#' @importFrom assertthat assert_that
#'
#'
selecteerIndicatoren <-
  function(Connectie = connecteerMetLSVIdb()) {

    #this function call only gives a warning in lintr if this comment precedes
    assert_that(inherits(Connectie, "DBIConnection"))
  }
