###########################################################################################
## five parameter visualization
## These are interactive routines to visualize changing relation of time to mean value.
##
## five.show: export
## five.plot: export
## five.make: used in five.switch, five.find, five.show
## five.switch: used in five.find, five.show, five.plot
## five.find: used in five.show
## five.lines: not used (list with same name elsewhere)
###########################################################################################
five.make <- function( fit = gencurve$fit,
                       dispersion=10, location=100, intensity=1,
                       truncation = 0, rejection = 1,
                       fivenum = list( dispersion, location, intensity, truncation, rejection ),
                       u = seq( 0.01, 0.99, by = 0.01 ))
{
  G <- function(x,eps=.01)
  {
    x <- pmax( eps, pmin( 1-eps, x ))
    -log(1-x)
  }
  n <- prod( unlist( lapply( fivenum, length )))
  organism <- matrix( u, length( u ), n + 1 )
  orgnames <- "prob"
  j <- 1
  for( it in truncation ) for( ii in intensity ) {
    tmpp <- stats::predict(fit$invmvalue, x = ( G(it)+G(u))/ii)
    nay <- is.na( tmpp$y )
    if( any ( nay ))
      tmpp$y[nay] <- spline.extrapolate( fit$meanvalue, fit$invmvalue,
                                         tmpp$x[nay] )
    for( ir in rejection ) {
      yr <- tmpp$y
      yr[ u >= ir ] <- NA
      for( id in dispersion ) for( il in location ) {
        orgnames <- c( orgnames, paste( round( c(
          dispersion, location, intensity,
          truncation, rejection ), 2 ), collapse = ":" ))
        j <- j + 1
        organism[,j] <- id * yr + il
      }
    }
  }
  dimnames( organism ) <- list( NULL, orgnames )
  organism
}
###########################################################################################
five.switch <- function( fit, pick, vals )
{
  switch( pick,
          dispersion = 
            five.make( fit, dispersion = vals ),
          location = 
            five.make( fit, location = vals ),
          intensity = 
            five.make( fit, intensity = 1 / vals ),
          truncation = 
            five.make( fit, truncation = vals ),
          rejection = 
            five.make( fit, rejection = vals ))
}
###########################################################################################
five.find <- function( fit = gencurve$fit, pick, vals, goal = .9,
                       refmean = mean( five.make( fit )[,2], na.rm = TRUE ),
                       tol = 1e-8, printit = FALSE )
{
  answer <- 0
  vals <- seq( min( vals ), max( vals ), length = 3 )
  tmp <- apply( five.switch( fit, pick, vals )[,-1], 2, mean, na.rm = TRUE ) / refmean
  if( is.na( tmp[2] ))
    return( NA )
  if( max( tmp, na.rm = TRUE ) < goal | min( tmp, na.rm = TRUE ) > goal )
    return( NA )
  ## binary search
  while( abs( answer - goal ) > tol & !is.na( tmp[2] )) {
    if( printit )
      cat( vals[2], tmp[2], "\n" )
    
    if( tmp[2] < goal ) {
      tmp[1] <- tmp[2]
      vals[1] <- vals[2]
      vals[2] <- mean( vals[2:3] )
    }
    else {
      tmp[3] <- tmp[2]
      vals[3] <- vals[2]
      vals[2] <- mean( vals[1:2] )
    }
    answer <- tmp[2] <- mean( five.switch( fit, pick, vals[2] )[,2], na.rm = TRUE ) /
      refmean
  }
  vals[2]
}
###########################################################################################
#' @export
five.show <- function( fit = spline.meanvalue(), goal = .9,
                       tol = 1e-5, legend.flag = 1, cex = 0.5, ylim = ylims, prefix = "" )
{
  fives <- c("dispersion","location","intensity","truncation","rejection")
  five.range <- list( dispersion = c(0,100), location = c(0,1000),
                      intensity = c(.1,100), truncation = c(0,1), rejection = c(0,1) )
  
  cat( paste( "goal = ", round( goal * 100 ), "%\n", sep = "" )) 
  tol <- c( rep( tol, 4 ), .01 )
  names( tol ) <- fives
  five.lines <- list()
  ref <- five.make( fit )
  ylims <- range( ref[,2], na.rm = TRUE )
  refmean <- mean( ref[,2], na.rm = TRUE )
  for( pick in fives ) {
    cat( pick, ": " )
    vals <- five.find( fit, pick, five.range[[pick]], goal = goal,
                       tol = tol[pick], refmean = refmean )
    if( pick == "intensity" )
      cat(1/vals, "\n")
    else
      cat(vals, "\n")
    if( !is.na( vals )) {
      tmp <- five.switch( fit, pick, vals )
      five.lines[[pick]] <- tmp[,2]
      ylims <- range( ylims, tmp[,2], na.rm = TRUE )
    }
  }
  plot(c(0,1),ylim, type="n", xlab = "", ylab = "" )
  graphics::mtext( "probability", 1, 2 )
  graphics::mtext( "time", 2, 2 )
  if( goal > 1 )
    main <- paste( prefix, round( 100 * ( goal - 1 ), 1 ), "% time extension", sep = "" )
  else
    main <- paste( prefix, round( 100 * ( 1 - goal ), 1 ), "% time reduction", sep = "" )
  graphics::mtext( main, 3, 1 )
  graphics::lines( ref[,1], ref[,2], lty = 3, lwd = 1 )
  graphics::abline( h = refmean * c( 1, goal[1] ), col = c("black","blue"), lty = c(1,3) )
  col <- c("blue","red","green","aquamarine","black")
  lty <- c(2,4,5,6,1)
  names( col ) <- names(lty) <- fives
  for( i in names( five.lines ))
    graphics::lines( tmp[,1], five.lines[[i]], lty = lty[i], lwd = 1, col = col[i] )
  switch( 1 + legend.flag,
          graphics::legend( 0, ylim[2], names( five.lines ),
                            lty = lty[ names( five.lines ) ],
                            col = col[ names( five.lines ) ], cex = cex ),
          graphics::legend( 1, ylim[1], names( five.lines ), xjust = 1, yjust = 0,
                            lty = lty[ names( five.lines ) ],
                            col = col[ names( five.lines ) ], cex = cex ))
  invisible( list( ref = ref, lines = five.lines ))
}
###########################################################################################
#' @export
five.plot <- function(gencurve = spline.meanvalue(), fit = gencurve$fit,
                      pick, vals, ylim = ylims)
{
  tmpx <- seq(0.01,.99,by=.01)
  G <- function(x,eps=.01)
  {
    x <- pmax( eps, pmin( 1-eps, x ))
    -log(1-x)
  }
  tmp <- stats::predict( fit$invmvalue, x = G(tmpx))
  ylims <- range( tmp$y, na.rm = TRUE )
  tmp <- five.switch( fit, pick, vals )
  plot( 0:1, ylim, type = "n",
        xlab = "probability", ylab = "time" )
  graphics::title( main = pick )
  graphics::lines( tmp[,1], tmp[,2], lty = 3 )
  for( i in 3:ncol( tmp ))
    graphics::lines( tmp[,1], tmp[,i], lty = 1 )
}
###########################################################################################
five.lines <- function( invmvalue = gencurve,
                        dispersion=10, location=100, intensity=.5,
                        truncation = .25, rejection = .75,
                        u = seq(0.01,.99,by=.01), lty = 1 )
{
  G <- function(x,eps=.01)
  {
    x <- pmax( eps, pmin( 1-eps, x ))
    -log(1-x)
  }
  for( it in truncation ) for( ii in intensity )
  {
    tmpp <- stats::predict(invmvalue$fit$inv, x = ( G(it)+G(u))/ii)
    for( ir in rejection )
    {
      yr <- tmpp$y
      yr[ u >= rejection ] <- NA
      for( id in dispersion ) for( il in location )
        lines( u, id * yr + il, lty = lty )
    }
  }
}
