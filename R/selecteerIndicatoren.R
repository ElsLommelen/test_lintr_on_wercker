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
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#'
#'
selecteerIndicatoren <-
  function(Testvariable = FALSE,
           Connectie = connecteerMetLSVIdb()) {

    #this function call only gives a warning in lintr if this comment precedes
    assert_that(inherits(Connectie, "DBIConnection"))

    #A simple selection query
    query <-
      "SELECT VersieLSVI FROM Versie"

    #dbGetQuery does not give a warning
    Selectiegegevens <- dbGetQuery(Connectie, query)

    return(Selectiegegevens)

  }
