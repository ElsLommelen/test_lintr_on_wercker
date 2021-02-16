#' @title Connecteer met de databank met LSVI-indicatoren in het package
#'
#' @description Deze functie blabla
#'
#' @return Deze functie geeft een open odbc-connectie naar de SQLite-databank
#' in de installatie-file van het package.
#'
#' @examples
#' library(testlintr)
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#'
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#'

connecteerMetLSVIdb <- function() {

  ConnectieLSVIhabitats <-
    dbConnect(
      drv = SQLite(),
      dbname =
        system.file("databank/LSVIHabitatTypes.sqlite", package = "testlintr"),
      encoding = "UTF-8"
    )

  return(ConnectieLSVIhabitats)
}
