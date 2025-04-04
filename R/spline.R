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
## Support routines for future.meanvalue
###########################################################################################
#' @importFrom stats coef predict qbinom
#' @importFrom graphics abline axis legend lines locator mtext par points text
#'             title
#' @importFrom splines interpSpline splineKnots
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
spline.deriv <- function( s )
{
  s$coefficients <- s$coefficients[,-1]
  for( i in seq( 2, ncol( s$coefficients )))
    s$coefficients[,i] <- s$coefficients[,i] * i
  s
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
            tmpy <- suppressWarnings(as.numeric( tmpy )) / max( xy$y )
            xy$y <- tmpy * xy$y
            if( is.data )
              rescale.data <- rescale.data * tmpy
          }
          tmpx <- readline( paste( "maximum time(", 
            round( max( xy$x ), 2 ), "):", sep = "" ))
          if( tmpx != "" ) {
            tmpx <- suppressWarnings(as.numeric( tmpx )) / max( xy$x )
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
