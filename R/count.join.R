count.join <- function( ... )
{
  x <- list( ... )
  numnum <- list()
  for( i in seq( length( x ))) {
    for( j in names( x[[i]] )) {
      if( is.null( numnum[[j]] ))
        numnum[[j]] <- x[[i]][[j]]
      else
        numnum[[j]] <- cbind( numnum[[j]], x[[i]][[j]] )
    }
  }
  numnum
}
