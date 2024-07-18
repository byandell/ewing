###########################################################################################
## System files
###########################################################################################
#' @importFrom readxl read_xls read_xlsx
#' @importFrom utils data
#' @importFrom tools file_ext
my.read <- function(dataname, stringsAsFactors = TRUE)
{
  switch(tools::file_ext(dataname),
         "txt" =, "tsv" = read.table(dataname, stringsAsFactors = stringsAsFactors),
         "csv" = read.csv(dataname, stringsAsFactors = stringsAsFactors),
         "xls" = readxl::read_xls(dataname),
         "xlsx" = readxl::read_xlsx(dataname))
}
###########################################################################################
my.eval <- function(species, extension, element, checkdata = FALSE )
{
  if( !missing( extension ))
    species <- paste( species, extension, sep = ".")
  if( exists( species )) 
    organism <- get( species )
  else {
    if( checkdata ) {
      organism <- utils::data( list = species )
      if( organism == species )
        organism <- NULL
    }
    else
      organism <- NULL
  }
  if(!( missing(element) | is.null( organism )))
    organism <- organism[[element]]
  organism
}
###########################################################################################
mydata <- function( dataname, package, restart = FALSE, messages = TRUE )
{
  edata <- exists( dataname )
  if( restart & edata ) {
    remove( list = dataname, pos = 1 )
    edata <- !edata
  }
  if( !edata ) {
    utils::data( list = dataname, package = eval( package ))
    if(messages) {
      cat( "Data", dataname, "loaded\n" )
    }
  }
  else
    if(messages) {
      cat( "Data", dataname, "already loaded\n" )
    }
} 
