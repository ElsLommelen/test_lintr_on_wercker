#' @title Selecteert indicatoren LSVI op basis van de opgegeven parameters
#'
#' @description Deze hulpfunctie selecteert de indicatoren die gebruikt worden
#' voor de bepaling van de Lokale Staat van Instandhouding voor de opgegeven
#' parameters.  Ze is bedoeld om te gebruiken als bouwsteen in andere functies
#' waar de gegevens voor bijvoorbeeld een welbepaalde versie of welbepaalde
#' habitattypes geselecteerd moeten kunnen worden.
#'
#' @param ConnectieLSVIhabitats Connectie met de databank met indicatoren voor
#' de LSVI van habitats, in te stellen d.m.v. functie connecteerMetLSVIdb.
#' @param HabitatnamenToevoegen Moeten de namen van de habitattypen en
#' habitatsubtypen toegevoegd worden als extra kolommen?  (Bij FALSE worden
#' enkel de habitatcodes toegevoegd, niet de volledige namen.)
#'
#' @return Deze functie geeft een tabel met velden Versie, Habitattype,
#' Habitatsubtype, Criterium, Indicator, Indicator_habitatID, TaxongroepId en
#' Indicator_beoordelingID.
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
  function(HabitatnamenToevoegen = FALSE,
           ConnectieLSVIhabitats = NULL) {

    if (is.null(ConnectieLSVIhabitats)) {
      if (exists("ConnectiePool")) {
        ConnectieLSVIhabitats <- get("ConnectiePool", envir = .GlobalEnv)
      }
    }
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren. Maak een connectiepool met maakConnectiePool of geef een connectie mee met de parameter ConnectieLSVIhabitats." #nolint
    )

    assert_that(is.logical(HabitatnamenToevoegen))

    #eerst de selectiegegevens ophalen en de nodige gegevens uit tabel
    #Indicator_habitat, query samenstellen op basis van parameters
    query <-
      sprintf(
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
        SELECT Versie.VersieLSVI AS Versie, Ht1.Code AS Habitattype,
            Ht2.Code AS Habitatsubtype,
            Criterium.Naam AS Criterium, Indicator.Naam AS Indicator,
            Indicator_habitat.Id AS Indicator_habitatID,
            Indicator_habitat.TaxongroepId,
            IndicatortabellenKoppeling.Indicator_beoordelingId
              AS Indicator_beoordelingID
        FROM Habitatselectie
          INNER JOIN Habitattype Ht1
            ON Habitatselectie.HabitattypeId = Ht1.Id
          INNER JOIN Habitattype Ht2
            ON Habitatselectie.HabitatsubtypeId = Ht2.Id
          INNER JOIN Habitatgroep ON Ht1.HabitatgroepId = Habitatgroep.Id
        INNER JOIN (((Indicator_habitat
        INNER JOIN
          (Indicator INNER JOIN Criterium
            ON Indicator.CriteriumID = Criterium.Id)
        ON Indicator_habitat.IndicatorID = Indicator.Id)
        INNER JOIN Versie ON Indicator_habitat.VersieID = Versie.Id)
        LEFT JOIN IndicatortabellenKoppeling
        ON Indicator_habitat.Id =
          IndicatortabellenKoppeling.Indicator_habitatId)
        ON Habitatselectie.HabitatsubtypeId = Indicator_habitat.HabitattypeID"
      )

    Selectiegegevens <-
      dbGetQuery(ConnectieLSVIhabitats, query) %>%
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
