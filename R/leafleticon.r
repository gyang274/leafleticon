#------------------------------------------------------------------------------#
#------------------------- leafleticon::leafleticon.r -------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ icon ------------------------------------#
#------------------------------------------------------------------------------#

#' ik_create_icon_db
#' @description
#' create an icon db for marking on leaflet map
#' @details
#' icons code (nm): building, house, package, truck
#' icons file (fl): building, house, package, truck
#' color code (cc): info, success, warning, danger, primary, purple and grey.
#' @return
#' icon list: <icons code>_<color code>
ik_create_icon_db <- function() {

  an_icon_nm <- c("building", "house", "package", "truck", "adduser", "reviewer")

  an_icon_fl <- c("building", "house", "package", "truck", "adduser", "reviewer")

  an_icon_cc <- c("info", "success", "warning", "danger", "primary", "purple", "grey")

  an_icon <- leafleticon::ik_create_icon_list(an_icon_nm, an_icon_fl, an_icon_cc)

  return( an_icon )

}

ik_create_icon_list <- function(ik_nm, ik_fl, ik_cc) {

  ik_ll <- data.table::CJ(nm = ik_nm, cc = ik_cc) %>%
    dplyr::mutate(
      fl = plyr::mapvalues(nm, from = ik_nm, to = ik_fl, warn_missing = FALSE)
    ) %>%
    dplyr::mutate(
      ik = purrr::pmap(list(nm, fl, cc), leafleticon::ik_create_icon_list_single)
    ) %>%
    `class<-`(c("data.table", "data.frame"))

  eval(parse(text = paste0(
    'ik <- list(', paste0(paste0(
      ik_ll[["nm"]], '_', ik_ll[["cc"]], ' = ', 'ik_ll[nm == "', ik_ll[["nm"]], '" & ', 'cc == "', ik_ll[["cc"]], '", ik, with = TRUE][[1]]'
    ), collapse = ", "), ')'
  )))

  attr(ik, "class") <- "leaflet_icon_set"

  return(ik)

}

ik_create_icon_list_single <- function(nm, fl, cc) {

  eval(parse(text = paste0(

    'ik <- leaflet::makeIcon(',

    'iconUrl = system.file("icon/', cc, '/', fl, '-128.png", package = "leafleticon"), ',

    'iconRetinaUrl = system.file("icon/', cc, '/', fl, '-512.png", package = "leafleticon"), ',

    'iconWidth = 24, iconHeight = 24',

    ')'

  )))

  return(ik)

}

#------------------------------------------------------------------------------#
