#' @title Selecteert indicatoren LSVI op basis van de opgegeven parameters
#'
#' @description Some description
#'
#' @param Connectie Connection to the SQLite database in the package
#' @param Testvariable just to use assert_that
#'
#' @return A dataframe
#'
#' @export
#'
#'
#' @importFrom DBI dbGetQuery
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
#'
selecteerIndicatoren <-
  function(Testvariable = FALSE,
           Connectie = connecteerMetLSVIdb()) {

    #this function call does not give a warning in lintr
    assert_that(inherits(Connectie, "DBIConnection"))

    #this function call gives a warning in lintr
    assert_that(is.logical(Testvariable))

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

    #dbGetQuery and dplyr functions do not give a warning
    Selectiegegevens <-
      dbGetQuery(Connectie, query) %>%
      mutate(
        Habitattype =
          ifelse(
            rep(is.numeric(.data$Habitattype), length(.data$Habitattype)),
            as.character(.data$Habitattype),
            .data$Habitattype
          ),
        Habitatsubtype =
          ifelse(
            rep(is.numeric(.data$Habitatsubtype), length(.data$Habitatsubtype)),
            as.character(.data$Habitatsubtype),
            .data$Habitatsubtype
          )
      )

    return(Selectiegegevens)

  }
