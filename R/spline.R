## $Id: spline.R,v 1.0 2002/12/09 yandell@stat.wisc.edu Exp $
##
## Functions for Bland Ewing's modeling.
##
##     Copyright (C) 2000,2001,2002 Brian S. Yandell.
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
###########################################################################################
## init.meanvalue( organism, stage )
## spline.meanvalue( x, y )
## spline.meanvalue( data = data )
##
## five.show( )
## five.plot( )
##
###########################################################################################
# Curve Designing routines -- under development
###########################################################################################


#' design mean value curves for future events in Ewing simulation models
#' 
#' Uses interactive spline tool to design curves.
#' 
#' 
#' @param community object with population data by species
#' @param species name of species
#' @param event name of future event
#' @param data data if available
#' @author Brian S. Yandell, \email{yandell@@stat.wisc.edu}
#' @seealso \code{\link{init.simulation}}, \code{\link{future.events}}
#' @references \url{www.stat.wisc.edu/~yandell/ewing}
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' future.meanvalue( community, "host", "first.instar" )
#' }
#' 
#' @export future.meanvalue
#' @importFrom splines backSpline interpSpline splineKnots
#' @importFrom stats coef predict qbinom
#' @importFrom graphics abline axis legend lines locator mtext par points text title
#' 
future.meanvalue <- function( community, species, event = future$current[1],
                             data )
{
  future <- getOrgFuture( community, species )
  mvalue <- getOrgMeanValue( community, species )[[event]]
  if( missing( data )) {
    if( !is.null( mvalue )) {
      mvalue <- stats::predict( mvalue$meanvalue, mvalue$meanvalue$knots )
      mvalue <- spline.meanvalue( mvalue$x, mvalue$y )$fit
    }
    else
      mvalue <- spline.meanvalue( )$fit
  }
  else
    mvalue <- spline.meanvalue( data = data )$fit
  setOrgMeanValue( community, species, event, mvalue )
}
###########################################################################################
spline.meanvalue <- function( x = xinit, y = yinit, data, nspline = 8,
  xy = data.frame( x = x, y = y ),
  tol = 1e-5, n = 1 )
{
  is.data <- !missing( data )
  if( !is.data ) {
    tmp <- - log( 1 - seq( 0, 1 - exp( -5 ), length = nspline ))
    if( missing( x ))
      xinit <- tmp
    else
      xinit <- x
    if( missing( y ))
      yinit <- tmp
    else
      yinit <- y
    data <- NULL
  }
  else {
    xinit <- sort( data )
    ndata <- length( data )
    yinit <- seq( ndata ) / ( 1 + ndata )
    choose <- round( seq( 1, ndata, length = nspline ))
    xinit <- xinit[choose]
    yinit <- - log( 1 - yinit[choose] )
  }
  fs <- list( probability = function( x ) { - log( 1 - x ) } )
  finvs <- list( probability = function( x ) { 1 - exp( - x ) } )
  for( i in c("mean value","rate","density") )
    fs[[i]] <- finvs[[i]] <- function( x ) x

  ## plot curve and surrounding axes
  graphics::par( mfrow = c(1,1), mar = rep(4.1,4))
  plotit <- function( xy, fig = "mean value", fit = splines::interpSpline( xy$x, xy$y ))
  {
    switch( fig, {
        y <- xy$y
        ylim <- range(c(0,y))
      },
      probability = {
        y <- 1 - exp( - xy$y )
        ylim <- range(c(0,y))
      },
      rate = {
        y <- spline.rate( fit, xy$x )$y
        tmp <- spline.rate( fit )
        ylim <- range(c(0,tmp$y))
      },
      density = {
        y <- spline.rate( fit, xy$x )$y * exp( - stats::predict( fit, xy$x )$y )
        tmp <- spline.rate( fit )
        tmp$y <- tmp$y * exp( - stats::predict( fit )$y )
        ylim <- range(c(0,tmp$y))
      }
    )
    plot(xy$x,y,xlim=1.25*range(c(0,xy$x)), ylim = ylim,
      type="n", xlab = "", ylab = "" )
    graphics::points( xy$x, y, lwd = 4 )
    graphics::title( fig )
    graphics::mtext( "time", 1, 2 )
    graphics::mtext( fig, 2, 2 )
    graphics::abline( v = max( xy$x ), lty = 2 )

    switch( fig, {
        if( fig == "probability" ) {
          tmp <- c(.1,.2,.5,1:10)
          ltmp <- 1-exp(-tmp)
          graphics::mtext( "mean value", 4, 2 )
        }
        else {  
          tmp <- c(seq(0,.9,,by=.1),.95,.98,.99,.999)
          ltmp <- -log(1-tmp)
          graphics::mtext( "probability", 4, 2 )
        }
        usr <- graphics::par("usr")
        s <- ltmp <= usr[4] & ltmp >= usr[3]  
        tmpar <- graphics::par( cex = .75 )
        graphics::axis(4,ltmp[s],tmp[s])
        graphics::par( tmpar )
        summaryshow( xy, fit, "white" )
        tmp <- curve.plot( xy, n = n, action = "refresh", fit = fit,
          f = fs[[fig]], finv = finvs[[fig]] )
        summaryshow( xy, tmp$fit )
      },
      rate =, density = {
        graphics::lines( tmp$x, tmp$y )
        summaryshow( xy, fit )
      }
    )
  }
  ## place commands along right strip of plot, highlighting current command
  plotcmd <- function( ans, fig, cmds, cmdlocs, usr, col = "green", rest = "black",
    data = FALSE )
  {
    ans <- c( ans, fig )
    if( data )
      ans <- c( ans, "data" )
    tmp <- is.na( match( cmds, ans ))
    if( any( tmp ))
      graphics::text( rep(usr[2],sum(tmp)), cmdlocs[tmp], cmds[tmp], col = rest, adj = 1 )
    if( any( !tmp ))
      graphics::text( rep(usr[2],sum(!tmp)), cmdlocs[!tmp], cmds[!tmp], col = col, adj = 1 )
  }

  fig <- "mean value"
  newans <- ans <- "replace"

  graphics::par( mar = c(4.1,4.1,3.1,4.1),omi=rep(.25,4))
  fit <- splines::interpSpline( xy$x, xy$y )
  sums <- plotit( xy, fig, fit )

  cmds <- c("refresh","add","delete","replace","rescale","shrink to 1","finish","restart",
    "","data","mean value","probability","rate","density")
  newlocs <- function( cmds, data = FALSE, usr )
  {
    if( !data )
       cmds <- cmds[ cmds != "data" ]
    n <- length( cmds )
    blank <- seq( n )[ cmds == "" ]
    tmp <- diff(usr[3:4]) / 20
    tmp <- c( usr[4] - tmp * seq( blank - 1 ), mean( usr[3:4] ),
      usr[3] + tmp * seq( n - blank ))
    names( tmp ) <- cmds
    tmp
  }
  usr <- graphics::par("usr")
  cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
  cmds <- names( cmdlocs )
  use.data <- FALSE
  plotcmd( ans, fig, cmds, cmdlocs, usr )
  rescale.data <- 1
  repeat {
    ## get command from plot using cursor
    z <- graphics::locator(1,"n")
    if( z$x > max( xy$x )) {
      z <- abs(z$y - cmdlocs )
      newans <- cmds[z==min(z)][1]
      switch( newans,
        finish =, refresh = {
          sums <- plotit( xy, fig, fit )
        },
        data = {
          use.data <- is.data & !use.data
          if( is.data & !use.data & match( fig, c("mean value","probability"),
            nomatch = 0 ))
            sums <- plotit( xy, fig, fit )
        },
        "mean value" =, probability =, rate =, density = {
          fig <- newans
          sums <- plotit( xy, fig, fit )
          usr <- graphics::par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        "shrink to 1" = {
          while( abs( sums[1] - 1 ) > tol ) {
            xy$y <- xy$y * sums[1]
            if( is.data )
              rescale.data <- rescale.data * sums[1]
            fit <- splines::interpSpline( xy$x, xy$y )
            sums <- splinesum( xy, fit, tol )
          }
          sums <- plotit( xy, fig, fit )
          usr <- graphics::par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        rescale = {
          cat( "enter new values followed by RETURN key\n" )
          tmpy <- readline( paste( "maximum mean value(",
            round( max( xy$y ), 2 ), "):", sep = "" ))
          if( tmpy != "" ) {
            tmpy <- as.numeric( tmpy ) / max( xy$y )
            xy$y <- tmpy * xy$y
            if( is.data )
              rescale.data <- rescale.data * tmpy
          }
          tmpx <- readline( paste( "maximum time(", 
            round( max( xy$x ), 2 ), "):", sep = "" ))
          if( tmpx != "" ) {
            tmpx <- as.numeric( tmpx ) / max( xy$x )
            xy$x <- tmpx * xy$x
            if( is.data )
              data <- data * tmpx
          }
          fit <- splines::interpSpline( xy$x, xy$y )
          sums <- plotit( xy, fig, fit )
          usr <- graphics::par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        restart = {
          if( is.data )
            rescale.data <- 1
          xy <- data.frame( x = xinit, y = yinit )
          fit <- splines::interpSpline( xy$x, xy$y )
          sums <- plotit( xy, fig, fit )
          usr <- graphics::par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        add =, delete =, replace = {
          ans <- newans
        }
      )
      if( use.data & match( fig, c("mean value","probability"), nomatch = 0 ))
        cdf.lines( data, fig, rescale = rescale.data )
      plotcmd( ans, fig, cmds, cmdlocs, usr, data = use.data )
    }
    else {
      if( is.na( match( fig, c("rate","density") ))) {
        summaryshow( xy, fit, "white", sums )
        fit <- curve.plot( xy, n = n, action = ans, z = z, fit = fit,
          f = fs[[fig]], finv = finvs[[fig]] )
        xy <- fit$xy
        fit <- fit$fit
        sums <- summaryshow( xy, fit )
        ans <- "replace"
        plotcmd( ans, fig, cmds, cmdlocs, usr, data = use.data )
      }
    }
    if( newans == "finish" )
      break
  }
  plotcmd( newans, fig, cmds, cmdlocs, "red", data = use.data )
  summaryshow( xy, fit, "white", sums )
  tmp <- curve.plot( xy, n = n, action = "refresh",
    f = fs[[fig]], finv = finvs[[fig]] )
  tmp$meanvalue <- tmp$fit
  tmp$fit <- NULL
  tmp$invmvalue <- splines::backSpline( tmp$meanvalue )
  sums <- summaryshow( xy, tmp$meanvalue )
  tmp$mean <- sums[1]
  tmp$median <- sums[2]
  tmp
}
###########################################################################################
spline.rate <- function( meanvalue, x )
{
  coeff <- stats::coef( meanvalue )
  for( i in 2:ncol( coeff ))
    coeff[,i-1] <- i * coeff[,i]
  coeff[,ncol( coeff ) ] <- 0
  meanvalue$coefficients <- coeff
  if( missing( x ))
    stats::predict( meanvalue )
  else
    stats::predict( meanvalue, x )
}
###########################################################################################
splinesum <- function( xy, fit = splines::interpSpline( xy$x, xy$y ),
                       log2 = log( 2 ), tol = 1e-5 )
{
  n <- nrow( xy )
  ## mean time to future event (assume linear off end )
  maxx <- xy[n,"x"]
  mean.y <- maxx * mean( exp( - stats::predict( fit )$y )) + xy[1,"x"]
  rate <- spline.rate( fit, maxx )$y
  if( rate > tol )
    mean.y <- mean.y + exp( - xy[n,"y"] ) / rate

  ## median time to future event
  adiff <- diff( stats::coef( fit )[,1] )
  ## undefined if mean value function not monotone
  if( !( all( adiff < 0) || all( adiff > 0 )))
    median.y <- NA
  else {
    invmvalue <- splines::backSpline( fit )
    median.y <- stats::predict( invmvalue, log2 )$y
    if( is.na( median.y ))
      median.y <- spline.extrapolate( fit, invmvalue, log2 )
  }
  c( mean = mean.y, median = median.y )
}
###########################################################################################
summaryshow <- function( xy, fit, col = "black", sums = splinesum( xy, fit ))
{
  tmpar <- graphics::par( col = col )
  graphics::mtext( paste( "mean =", round( sums[1], 2 )), 3, at = graphics::par("usr")[1], adj = 0 )
  medianshow <- if( is.na( sums[2] ))
    "curve not monotone"
  else
    paste( "median =", round( sums[2], 2 ))
  graphics::mtext( medianshow, 3, at = graphics::par("usr")[2] / 1.25, adj = 1 )
  graphics::par( tmpar )
  invisible( sums )
}
###########################################################################################
curve.plot <- function( xy = seq(0,1,by=.25), y = c(0,.1,.5,.9,1),
  z = graphics::locator(1,"n"), n=5, action="add",
  fit = splines::interpSpline( xy$x, xy$y ),
  backfit = TRUE, save.ends = 3, col = c("blue","red"), lwd = 4,
  f = function( x ) x, finv = function( x ) x )
{
  if( !is.list( xy )) {
    xy <- data.frame( x = xy )
    xy$y <- y
  }
  else
    xy <- as.data.frame( xy )
  if( !match( action, c("refresh","finish"), nomatch = 0 )) {
    z <- as.data.frame(z)
    tmp <- z$x > max( xy$x )
    if( any( tmp )) {
      if( all( tmp ))
        return( xy )
      z$x <- z$x[!tmp]
      z$y <- z$y[!tmp]
    }
  }
  remove.points <- function( xy, finv, save.ends = TRUE ) {
    ## find closest point after standardizing distances
    usr <- graphics::par( "usr" )
    tmp <- (( xy$x - z$x ) / diff( usr[1:2] )) ^ 2 +
      (( xy$y - z$y ) / diff( usr[3:4] )) ^ 2
    if( save.ends )
      tmp <- tmp[ - c( 1, nrow( xy )) ]
    tmp <- save.ends + min( seq( tmp )[ tmp == min( tmp ) ] )
    tmpd <- xy[tmp,"y"]
    tmpd <- finv( tmpd )
    graphics::points( xy[tmp,"x"], tmpd, lwd = lwd, col = "white" ) 
    tmp
  }
  for( i in 1:n) {
    switch( action,
      noaction =
        return( xy )
      ,
      add = {
        graphics::points(z$x,z$y, lwd = lwd )
        z$y <- f( z$y )
        xy <- rbind(xy,z)
        xy <- xy[order(xy$x),]
        fit <- splines::interpSpline( xy$x, xy$y )
      },
      replace = {
        graphics::points(z$x,z$y, lwd = lwd )
        z$y <- f( z$y )
        tmp <- remove.points( xy, finv, save.ends == 3 )
        tmpp <- ( tmp > 1 & tmp < length( xy$x ))
        if( save.ends != 2 | tmpp )
          xy$x[tmp] <- z$x
        if( save.ends != 1 | tmpp )
          xy$y[tmp] <- z$y
        fit <- splines::interpSpline( xy$x, xy$y )
      },
      delete = {
        z$y <- f( z$y )
        tmp <- remove.points( xy, finv, save.ends > 0 )
        xy <- xy[-tmp,]
        fit <- splines::interpSpline( xy$x, xy$y )
      },
      refresh =, finish = {
        xy
      }
    )
    tmpp <- stats::predict( fit )
    graphics::lines( tmpp$x, finv( tmpp$y ), col = col[1], lwd = lwd )

    adiff <- diff( stats::coef( fit )[,1] )
    if( backfit & ( all( adiff < 0) || all( adiff > 0 ))) {
      ## backspline (not quite a spline) fit to inverse
      tmpback <- stats::predict( splines::backSpline( fit ))
      graphics::lines( tmpback$y, finv( tmpback$x ), col = col[2], lwd = lwd )
    }
  }
  list( xy = xy[order(xy$x),], fit = fit )
}
###########################################################################################
cdf.lines <- function( data, fig = "mean value", nspline = 8,
  conf = c(50,80,90,95), rescale = 1,
  col = c("green","blue","red","orange") )
{
  rate <- fig == "mean value"
  n <- length( data )
  data <- sort( data )
  prob <- seq( n ) / ( n + 1 )
  f <- - rescale * log( 1 - prob )
  ylab <- "prob"
  if( rate )
    ylab <- "cum rate"
  else
    f <- 1 - exp( -f )
  graphics::lines( data, f, lwd = 2 )
  conf <- conf / 100
  for( i in seq( length( conf ))) {
    ## lower confidence
    tmp <- stats::qbinom( conf[i], n, prob, lower.tail = TRUE ) / ( n + 1 )
    tmp <- 1 - exp( rescale * log( 1 - tmp ))
    tmpna <- is.na( tmp )
    if( any( tmpna ))
      tmp[tmpna] <- f[1]
    if( rate )
      tmp <- - log( 1 - tmp )
    graphics::lines( data, tmp, lty = 2, col = col[i] )
    ## upper confidence
    tmp <- stats::qbinom( conf[i], n, prob, lower.tail = FALSE ) / ( n + 1 )
    tmp <- 1 - exp( rescale * log( 1 - tmp ))
    tmpna <- is.na( tmp )
    if( any( tmpna ))
      tmp[tmpna] <- f[n]
    if( rate )
      tmp <- - log( 1 - tmp )
    graphics::lines( data, tmp, lty = 2, col = col[i] )
  }
}
###########################################################################################
rspline <- function( meantime = 1,
                    fivepar = c(dispersion = 1, location = 0, intensity = 1, truncation = 0,
                      rejection = Inf ),
                    fit = NULL,
                    meanvalue = fit$meanvalue, invmvalue = fit$invmvalue,
                    span = Inf )
{
  ## dispersion = a, location = b, intensity = c
  ## truncation = -log(1-d), rejection = -log(1-e)

  ## y = a M^-1( G(d)+cV ) + b  if 1 - exp( -cV ) < G(e)
  ## y = span        if 1 - exp( -cV ) >= G(e)

  ## this is not quite right for truncation, as we know event happened before b
  ## if 1-exp(-cV) < d, but I am not sure how to pass that information along yet

  default <- is.null( meanvalue )

  ##          V ~ exp(1)
  ## intensity:      V/c
  ## truncation:      (G(d)+V)/c
  rate <- ( fivepar["truncation"] + rexp( 1 )) / fivepar["intensity"]

  ## mean value inverse:    M^-1( G(d)+V/c )
  if( default )
    y <- rate
  else {
    y <- stats::predict( invmvalue, rate )$y
    ## kludge to linearly extrapolate beyond cubic spline fit
    if( is.na( y ))
      y <- spline.extrapolate( meanvalue, invmvalue, rate )
  }

  ## dispersion and location:  y = a M^-1( cV ) + b
  y <- meantime * fivepar["dispersion"] * y + fivepar["location"]

  ## rejection:      y >= e? then set y to span
  if( y > fivepar["rejection"] )
    y <- span

  y
}
###########################################################################################
spline.extrapolate <- function( meanvalue, invmvalue, x )
{
  ## linear extrapolation of inverse spline beyond upper end
  coeff <- stats::coef( invmvalue )
  nr <- nrow( coeff ) - 1
  xknot <- splines::splineKnots( invmvalue )[nr+(0:1)]
  yknot <- splines::splineKnots( meanvalue )[nr+1]
  tmpr <- ( xknot[2] - xknot[1] )
  slope <- coeff[nr,2] + tmpr * ( 2 * coeff[nr,3] + tmpr * 3 * coeff[nr,4] )
  yknot + slope * ( x - xknot[2] )
}
###########################################################################################
## five parameter visualization
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
five.show <- function( fit = gencurve, goal = .9,
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
five.plot <- function( gencurve = spline.meanvalue(), fit = gencurve$fit, pick, vals, ylim = ylims )
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
five.oplot <- function( invmvalue = gencurve,
        dispersion=10, location=100, intensity=.5,
        truncation = .25, rejection = .75,
        ylim = ylims,
        u = seq(0.01,.99,by=.01))
{
        G <- function( u ) -log( 1 - u )
        tmp0 <- stats::predict(invmvalue$fit$inv,x=G(u))
        tmpp <- stats::predict(invmvalue$fit$inv,x=(G(truncation)+G(u))/intensity)
        tmpp$y[ u >= rejection ] <- NA
        ylims <- dispersion * range( tmp0$y, tmpp$y, na.rm = TRUE ) + location
        plot( c(0,1), ylim,
                type = "n",
                ylab = "time", xlab = "probability" )
        graphics::lines( u, dispersion * tmp0$y + location, lty = 3 )
        graphics::lines( u, dispersion * tmpp$y + location, lty = 1 )
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
###########################################################################################
### spline.design() is a prototype for designing spline curves
### Ultimately, pieces of spline.design, spline.temp() and spline.meanvalue()
### will be pulled out as subroutines to reduce code overlap
###########################################################################################
spline.design <- function (y = yinit, x = xinit, nspline = 8, xy = data.frame(x = x, 
    y = y), n = 1, horizontal = FALSE) 
{
    is.data <- !missing(y)
    if (is.data) {
        data <- y
        if (missing(x)) 
            x <- as.numeric(names(y))
        datax <- x
        ndata <- length(data)
        choose <- round(seq(1, ndata, length = nspline))
        xinit <- x <- x[choose]
        yinit <- y <- y[choose]
    }
    else {
        tmp <- seq(0, nspline - 1)
        if (missing(x)) 
            xinit <- tmp
        else xinit <- x
        if (missing(y)) 
            yinit <- rep(50, nspline)
        else yinit <- y
    }

  ## plot curve and surrounding axes
  graphics::par( mfrow = c(1,1), mar = rep(4.1,4))
  plotit <- function( xy, fig = "temp", fit = splines::interpSpline( xy$x, xy$y ),
    horizontal = FALSE, strip = .25, margin = .1 )
  {
    switch( fig, {
        y <- xy$y
        ylim <- range(y)
      }
    )
    xlim <- range(xy$x)
    xlim <- xlim + c(-1,1) * margin * diff( xlim )
    if( horizontal ) {
      if( diff( ylim ) == 0 )
        ylim <- ylim * c(.75,1.25)
      separator <- ylim[2]
      ylim[2] <- ylim[2] + strip * diff( ylim )
    }
    else {
      separator <- xlim[2]
      xlim[2] <- xlim[2] + strip * diff( xlim )
    }
    axt <- c("n","s")
    tmpar <- graphics::par( xaxt = axt[1+horizontal], yaxt = axt[2-horizontal] )
    plot( xy$x, y, xlim = xlim, ylim = ylim, type="n", xlab = "", ylab = "" )
    graphics::par( xaxt = "s", yaxt = "s" )
    graphics::points( xy$x, y, lwd = 4 )
    graphics::title( fig )
    graphics::mtext( "time", 1, 2 )
    graphics::mtext( fig, 2, 2 )
    if( horizontal ) {
      p <- pretty( c(ylim[1],separator) )
      graphics::axis( 2, p[ p <= separator ] )
      graphics::abline( h = separator, lty = 2 )
    }
    else {
      p <- pretty( c(xlim[1],separator) )
      graphics::axis( 1, p[ p <= separator ] )
      graphics::abline( v = separator, lty = 2 )
    }
    curve.plot( xy, n = n, action = "refresh", fit = fit, backfit = FALSE,
      save.ends = 0 )
    separator
  }
  ## place commands along right strip of plot, highlighting current command
  plotcmd <- function( ans, fig, cmds, cmdlocs, usr, col = "green", rest = "black",
    horizontal = TRUE )
  {
    ans <- c( ans, fig )
    tmp <- is.na( match( cmds, ans ))
    if( any( tmp )) for( i in unique( cmdlocs$adj )) {
      tmpi <- tmp & i == cmdlocs$adj
      if( any( tmpi ))
        graphics::text( cmdlocs$x[tmpi], cmdlocs$y[tmpi], cmds[tmpi], col = rest, adj = i )
    }
    if( any( !tmp )) for( i in unique( cmdlocs$adj )) {
      tmpi <- !tmp & i == cmdlocs$adj
      if( any( tmpi ))
        graphics::text( cmdlocs$x[tmpi], cmdlocs$y[tmpi], cmds[tmpi], col = col, adj = i )
    }
  }
  cmds <- c("add","delete","replace","","finish","restart","refresh","rescale",
    "","data","temp")
  newlocs <- if( horizontal )
    function( cmds, data = FALSE, usr )
    {
      if( !data )
        cmds <- cmds[ cmds != "data" ]
      n <- length( cmds )
      blank <- seq( n )[ cmds == "" | cmds == " " ]
      tmp <- diff(usr[3:4]) / 20
      m <- mean( usr[1:2] )
      y <- usr[4] + 0.5 * tmp - c( tmp * seq( blank[1] - 1 ), 0,
        tmp * seq( blank[2] - blank[1] - 1 ), 0,
        tmp * seq( n - blank[2] ))
      x <- c( rep( usr[1], blank[1] - 1 ), mean( m, usr[1] ),
        rep( m, blank[2] - blank[1] - 1 ), mean( m, usr[2] ),
        rep( usr[2], n - blank[2] ))
      adj <- c( rep( 0, blank[1] ),
        rep( 0.5, blank[2] - blank[1] ),
        rep( 1, n - blank[2] ))
      tmp <- data.frame( x = x, y = y, adj = adj )
      cmds[blank[2]] <- " "
      row.names( tmp ) <- cmds
      tmp
    }
  else
    function( cmds, data = FALSE, usr )
    {
      if( !data )
        cmds <- cmds[ cmds != "data" ]
      n <- length( cmds )
      blank <- seq( n )[ cmds == "" ]
      tmp <- diff(usr[3:4]) / 20
      m <- mean( usr[3:4] )
      tmp <- c( usr[4] - tmp * seq( blank[1] - 1 ),
        mean( m, usr[4] ),
        m + tmp * ( seq( blank[1] + 1, blank[2] - 1 ) - mean( blank )),
        mean( m, usr[3] ),
        usr[3] + tmp * seq( n - blank[2] ))
      tmp <- data.frame( x = rep( usr[2], n ), y = tmp, adj = rep( 1, n ))
      cmds[blank[2]] <- " "
      row.names( tmp ) <- cmds
      tmp
    }

  fig <- "temp"
  newans <- ans <- "replace"

  graphics::par( mar = c(4.1,4.1,3.1,4.1),omi=rep(.25,4))
  fit <- splines::interpSpline( xy$x, xy$y )
  separator <- plotit( xy, fig, fit, horizontal )

  usr <- graphics::par("usr")
  cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
  cmds <- row.names( cmdlocs )
  plotcmd( ans, fig, cmds, cmdlocs, usr )
  use.data <- FALSE
  rescale.data <- 1
  repeat {
    ## get command from plot using cursor
    z <- graphics::locator(1,"n")
    if(( !horizontal & z$x > separator ) | ( horizontal * z$y > separator )) {
      if( horizontal ) { # need to look at both z&y 
        x <- abs( z$x - cmdlocs$x )
        x <- x == min( x )
        newans <- cmds[x]
        z <- abs( z$y - cmdlocs$y )[x]
        newans <- newans[ z == min( z ) ][1]
      }
      else {
        z <- abs(z$y - cmdlocs$y )
        newans <- cmds[ z == min( z ) ][1]
      }
      switch( newans,
        finish =, refresh = {
          separator <- plotit( xy, fig, fit, horizontal )
          usr <- graphics::par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        data = {
          use.data <- is.data & !use.data
          if( is.data & !use.data )
            plotit( xy, fig, fit, horizontal )
        },
        temp = {
          fig <- newans
          separator <- plotit( xy, fig, fit, horizontal )
          usr <- graphics::par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        rescale = {
          cat( "enter new values followed by RETURN key\n" )
          tmpy <- readline( paste( "maximum ", fig, "(",
            round( max( xy$y ), 2 ), "):", sep = "" ))
          if( tmpy != "" ) {
            tmpy <- as.numeric( tmpy ) / max( xy$y )
            xy$y <- tmpy * xy$y
            if( is.data )
              rescale.data <- rescale.data * tmpy
          }
          tmpx <- readline( paste( "maximum time(", 
            round( max( xy$x ), 2 ), "):", sep = "" ))
          if( tmpx != "" ) {
            tmpx <- as.numeric( tmpx ) / max( xy$x )
            xy$x <- tmpx * xy$x
          }
          fit <- splines::interpSpline( xy$x, xy$y )
          separator <- plotit( xy, fig, fit, horizontal )
          usr <- graphics::par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        restart = {
          if( is.data )
            rescale.data <- 1
          xy <- data.frame( x = xinit, y = yinit )
          fit <- splines::interpSpline( xy$x, xy$y )
          separator <- plotit( xy, fig, fit, horizontal )
          usr <- graphics::par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        add =, delete =, replace = {
          ans <- newans
        }
      )
      if( use.data ) {
        rx <- range( xy$x )
        dx <- range( datax )        
        graphics::lines( rx[1] + ( datax - dx[1] ) * diff( rx ) / diff( dx ), data * rescale.data )
      }
      plotcmd( ans, fig, cmds, cmdlocs, usr )
    }
    else {
      fit <- curve.plot( xy, n = n, action = ans, z = z, fit = fit, backfit = FALSE,
        save.ends = 0 )
      xy <- fit$xy
      fit <- fit$fit
      ans <- "replace"
      plotcmd( ans, fig, cmds, cmdlocs, usr )
    }
    if( newans == "finish" )
      break
  }
  plotcmd( newans, fig, cmds, cmdlocs, "red" )
  tmp <- curve.plot( xy, n = n, action = "refresh", backfit = FALSE, save.ends = 0 )
  tmp
}
