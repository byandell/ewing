###########################################################################################
putCount <- function( community, append = FALSE )
{
  species <- get.species( community )
  
  countage <- getCount( community,, "countage")
  countsub <- getCount( community,, "countsub")
  countbase <- getCount( community,, "base")
  
  cnames <- c("step", "time", "future",
              paste( "count", 
                     seq( max( unlist( lapply( countage, length )) +
                                 unlist( lapply( countsub, length )))),
                     sep = "" ))

  cnums <- list()
  for( i in species ) {
    cnums[[i]] <- c(
      0,
      get.species.element( community, i, c("time","stage"), countbase[i] ),
      countage[[i]],
      countsub[[i]])
  }

  file <- getCount( community,, "file" )
  if(!is.null(file)) {
    if( !( file.exists( file ) & append ))
      cat( "species", cnames, "\n", file = file )
    for( i in species ) {
      cat( i, cnums[[i]], "\n", file = file, append = TRUE )
    }
    community
  } else {
    if(append) {
      counts <- getCount( community,, "counts")
    } else {
      counts <- NULL
    }
    newcounts <- matrix(NA, length(species), length(cnames),
                       dimnames = list(species, cnames)) 
    for(i in species) {
      newcounts[i, seq_along(cnums[[i]])] <- cnums[[i]]
    }
    newcounts <- tibble::tibble(
      data.frame(
        species = species,
        newcounts))
    counts <- dplyr::bind_rows(
      counts,
      newcounts)
    
    setCount( community,, list(counts = counts))
  }
}
###########################################################################################
writeCount <- function( community, species, time, future, countage, countsub)
{
  nstep <- getCount( community,, "step" )
  
  cnums <- c(nstep, time, future, countage, countsub)
  
  file <- getCount( community,, "file" )
  if(!is.null(file)) {
    cat( species, cnums, "\n", file = file, append = TRUE )
    
    community
  } else {
    counts <- getCount( community,, "counts")
    cnames <- names(counts)[-1]
    
    newcounts <- matrix(NA, 1, length(cnames),
                          dimnames = list(species, cnames)) 
    newcounts[1, seq_along(cnums)] <- cnums
    
    newcounts <- tibble::tibble(
      data.frame(
        species = species,
        newcounts))
    counts <- dplyr::bind_rows(
      counts,
      newcounts)
    
    setCount( community,, list(counts = counts))
  }
}
###########################################################################################
#' @export
#' @importFrom utils read.table
#' 
readCount <- function( community, species = unique(counts$species) )
{
  file <- getCount( community,, "file" )
  if(!is.null(file)) {
    counts <- utils::read.table( file, header = TRUE, fill = TRUE )
  } else {
    counts <- getCount( community,, "counts")
  }
  
  count <- list()
  for( i in species ) {
    colnames <- c( levels( getOrgFuture( community, i, "ageclass" )),
                   levels( getOrgInteract( community,, i, "substrate" )))
    count[[i]] <- as.matrix( counts[ counts$species == i, seq( 2, 4 + length( colnames )) ] )
    dimnames( count[[i]] ) <- list( count[[i]][,"step"],
                                    c( "step", "time", "future", colnames ))
  }
  count
}

