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

    #A CTE query
    query <-
      "WITH Habitatselectie
      AS
      (
        SELECT Ht1.Id AS HabitattypeId, Ht1.Id AS HabitatsubtypeId
        FROM Habitattype AS Ht1
        WHERE Ht1.ParentId IS NULL
      UNION ALL
        SELECT Habitatselectie.HabitattypeId, Ht2.Id AS HabitatsubtypeId
        FROM Habitatselectie INNER JOIN Habitattype AS Ht2
        ON Habitatselectie.HabitatsubtypeId = Ht2.ParentId
      )
      SELECT Ht1.Code AS Habitattype,
          Ht2.Code AS Habitatsubtype
      FROM Habitatselectie
        INNER JOIN Habitattype Ht1
          ON Habitatselectie.HabitattypeId = Ht1.Id
        INNER JOIN Habitattype Ht2
          ON Habitatselectie.HabitatsubtypeId = Ht2.Id"

    #dbGetQuery does not give a warning
    Selectiegegevens <- dbGetQuery(Connectie, query)

    return(Selectiegegevens)

  }
