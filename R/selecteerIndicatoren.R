#' @title Just a test
#'
#' @description Some description
#'
#' @param Teststring Just a string to test
#'
#' @return A dataframe
#'
#' @export
#'
#'
#' @importFrom assertthat assert_that is.string
#'
#'
selecteerIndicatoren <-
  function(Teststring = "string") {

    #this function call only gives a warning in lintr if this comment precedes
    assert_that(is.string(Teststring))
  }
