#' @importFrom stringr str_detect str_remove
#' @importFrom readxl excel_sheets
#' @importFrom tools file_ext
get.organisms <- function(datafile = "") {
  org <- list(species = c("host", "parasite"), substrates = "substrate")
  if(datafile != "") {
    if(tools::file_ext(datafile) %in% c("xls","xlsx")){
      sheets <- readxl::excel_sheets(datafile)
      species <- stringr::str_remove(
        sheets[stringr::str_detect(sheets, "future\\.")],
        "future\\.")
      substrates <- unique(stringr::str_remove(
        sheets[stringr::str_detect(sheets, paste("\\.", species, sep = "", collapse = "|")) &
        !stringr::str_detect(sheets, paste(c("future", species), "\\.", sep = "", collapse = "|"))],
        "\\..*"))
      org <- list(species = species, substrates = substrates)
    }
  }
  org
}
###########################################################################################
get.species <- function( community, species ) {
  if( missing( species ))
    return( names( community$pop ))
  if( is.numeric( species ))
    species <- names( community$pop )[species]
  if( is.null( species ) || !species %in% names( community$pop ))
    return( NULL )
  
  ans <- community$pop[[species]]
  if (is.list(ans) && !is.matrix(ans) && !is.data.frame(ans) && !is.null(ans$org)) {
    ans <- ans$org
  }
  if (is.matrix(ans) || is.data.frame(ans)) {
    if (!"up" %in% rownames(ans) && (is.null(colnames(ans)) || colnames(ans)[1] != "dummy")) {
      dummy <- ans[, 1, drop = FALSE]
      colnames(dummy) <- "dummy"
      return(cbind(dummy, ans))
    }
  }
  ans
}
###########################################################################################
get.species.element <- function( community, species, rows, cols )
  community$pop[[species]][rows,cols]
###########################################################################################
put.species <- function( community, species, value )
{
  community$pop[[species]] <- value
  community
}
###############################################################################
put.individual <- function( community, species, individual,
                           id = get.base( community, species ))
{
  community$pop[[species]][,id] <- individual
  community
}  
###############################################################################
get.individual <- function( community, species,
                           id = get.base( community, species ))
  community$pop[[species]][,id]
###############################################################################
get.base <- function( community, species )
  community$pop[[species]]["up",1]
