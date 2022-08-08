## $Id: temp.R,v 1.0 2002/12/09 yandell@stat.wisc.edu Exp $
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
## initTemp( community, lo.hour, hi.hour )	### creates Temperature object
## activeTemp( community, lo.hour, hi.hour )	### updates Temperature object
## checkTime( ) # check if activeTemp needs updating to cover time interval
##
## temp.design( )
## temp.plot( )
###########################################################################################


#' graphical adjustment of daily high and low temperature
#' 
#' Graphical interface updates daily high and low temperatures, along with
#' rescaling the days and temperature range.
#' 
#' 
#' @aliases temp.design temp.plot
#' @param community object with population data by species
#' @param nspline number of spline nodes
#' @param n number of steps to perform
#' @param horizontal use horizontal orientation if TRUE
#' @param col colors of \code{low} and \code{high} temperature ranges
#' @param lo.hour lower limit of hours to plot
#' @param hi.hour upper limit of hours to plot
#' @param length number of interpolation points for plot
#' @param derivative show derivative of temperature
#' @param ... other arguments to pass to plot
#' @param printit print out hours and temperatures if \code{TRUE}
#' @author Brian S. Yandell
#' @seealso \code{\link{TemperatureBase}}
#' @references See \url{www.stat.wisc.edu/~yandell/ewing}.
#' @keywords utilities
#' @examples
#' 
#' \dontrun{
#' temp.design()
#' temp.plot()
#' }
#' 
#' @export temp.design
#' @importFrom splines interpSpline
#' @importFrom stats predict
#' 
temp.design <- function( community, nspline = 8, n = 1, horizontal = TRUE,
  col = c(low="blue",high="red") )
{
  low <- getTemp( community, "Low" )
  high <- getTemp( community, "High" )
  is.data <- !is.null( low )
  if( !is.data ) {
    tmp <- seq( 0, 60, length = nspline )
    low <- interpSpline( tmp, 60 + 0.125 * tmp + sin( 0.25 * tmp ))
    high <- interpSpline( tmp, 70 + 0.15 * tmp + sin(( pi / 8 ) + 0.225 * tmp ))
  }
  ## plot curve and surrounding axes
  par( mfrow = c(1,1), mar = rep(4.1,4))
  cat( "Switch to Graphic Screen to Adjust High and Low Temperatures\n" )

  plotit <- function( low, high, fig = fig, horizontal = FALSE, strip = .25, margin = 0 )
  {
    lowpred <- stats::predict( low, low$knots )
    highpred <- stats::predict( high, high$knots )
    ylim <- range( c( lowpred$y, highpred$y ))
    xlim <- range( c( lowpred$x, highpred$x))
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
    tmpar <- par( xaxt = axt[1+horizontal], yaxt = axt[2-horizontal] )
    plot( xlim, ylim, xlim = xlim, ylim = ylim, type="n", xlab = "", ylab = "" )
    par( xaxt = "s", yaxt = "s" )
    title( fig )
    mtext( "day", 1, 2 )
    mtext( "temp", 2, 2 )
    if( horizontal ) {
      p <- pretty( c(ylim[1],separator) )
      axis( 2, p[ p <= separator ] )
      abline( h = separator, lty = 2 )
    }
    else {
      p <- pretty( c(xlim[1],separator) )
      axis( 1, p[ p <= separator ] )
      abline( v = separator, lty = 2 )
    }
    points( highpred$x, highpred$y, lwd = 4 )
    curve.plot( highpred, n = n, action = "refresh", fit = high, backfit = FALSE,
      save.ends = 3, col = "red", lwd = 2 * ( 1 + ( fig == "high" )))
    points( lowpred$x, lowpred$y, lwd = 4 )
    curve.plot( lowpred, n = n, action = "refresh", fit = low, backfit = FALSE,
      save.ends = 3, col = "blue", lwd = 2 * ( 1 + ( fig == "low" )))
    separator
  }
  ## place commands along right strip of plot, highlighting current command
  plotcmd <- function( ans, fig, cmds, cmdlocs, usr, col = "green", rest = "black",
    horizontal = TRUE, data = FALSE )
  {
    ans <- c( ans, fig )
    if( data )
      ans <- c( ans, "data" )
    tmp <- is.na( match( cmds, ans ))
    if( any( tmp )) for( i in unique( cmdlocs$adj )) {
      tmpi <- tmp & i == cmdlocs$adj
      if( any( tmpi ))
        text( cmdlocs$x[tmpi], cmdlocs$y[tmpi], cmds[tmpi], col = rest, adj = i )
    }
    if( any( !tmp )) for( i in unique( cmdlocs$adj )) {
      tmpi <- !tmp & i == cmdlocs$adj
      if( any( tmpi ))
        text( cmdlocs$x[tmpi], cmdlocs$y[tmpi], cmds[tmpi], col = col, adj = i )
    }
  }
  cmds <- c("add","delete","replace","rescale","","finish","refresh","restart",
    "","data","high","low")
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
      blank <- seq( n )[ cmds == "" | cmds == " " ]
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

  newans <- ans <- "replace"
  par( mar = c(4.1,4.1,3.1,4.1),omi=rep(.25,4))
  fig <- "low"
  fit <- low
  separator <- plotit( low, high, fig, horizontal )

  usr <- par("usr")
  cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
  cmds <- row.names( cmdlocs )
  use.data <- FALSE
  plotcmd( ans, fig, cmds, cmdlocs, usr, data = use.data )
  rescale.data <- c( range( stats::predict( low, low$knots )$y,
    stats::predict( high, high$knots )$y ), range( low$knots, high$knots ))
  repeat {
    ## get command from plot using cursor
    z <- locator(1,"n")
    if(( !horizontal & z$x > separator ) | ( horizontal & z$y > separator )) {
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
          separator <- plotit( low, high, fig, horizontal )
          usr <- par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        data = {
          use.data <- is.data & !use.data
          if( is.data & !use.data )
            plotit( low, high, fig, horizontal )
        },
        high =, low = {
          if( newans != fig )
            fig <- newans
          separator <- plotit( low, high, fig, horizontal )
          usr <- par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        rescale = {
          tmpcmds <- cmds
          tmpans <- tmpcmds[ cmds == "rescale" ] <-
            "rescale: Switch to Character Screen"
          plotcmd( tmpans, fig, tmpcmds, cmdlocs, usr, data = use.data )
          ry <- range( c( stats::predict( low, low$knots )$y, stats::predict( high, high$knots )$y ))
          cat( "\nEnter new minimum/maximum followed by RETURN key\n" )
          newry <- ry
          show <- c("minimum","maximum")
          change <- FALSE
          for( i in 1:2 ) {
            tmpy <- readline( paste( show[i], " temp (",
              round( ry[i], 2 ), "):", sep = "" ))
            tmpy <- if( tmpy == "" ) NA
              else as.numeric( tmpy )
            if( is.na( tmpy ))
              tmpy <- ry[i]
            else
              change <- TRUE
            rescale.data[i] <- tmpy
          }
          tmp <- range( c( low$knots, high$knots ))
          for( i in 1:2 ) {
            tmpx <- readline( paste( show[i], " time (", 
              round( tmp[i], 2 ), "):", sep = "" ))
            tmpx <- if( tmpx == "" ) NA
            else as.numeric( tmpx )
            if( is.na( tmpx ))
              tmpx <- tmp[i]
            else
              change <- TRUE
            rescale.data[2+i] <- tmpx
          }
          cat( "Switch to Graphic Screen to Adjust High and Low Temperatures\n" )
          if( change ) {
            tmp <- rescale.temp( low, high, rescale.data[1:2],
                                rescale.data[3:4], ry )
            low <- tmp$low
            high <- tmp$high
          }
          separator <- plotit( low, high, fig, horizontal )
          usr <- par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        restart = {
          low <- getTemp( community, "Low" )
          high <- getTemp( community, "High" )
          if( is.data )
            rescale.data <- c( range( stats::predict( low, low$knots )$y,
              stats::predict( high, high$knots )$y ), range( low$knots, high$knots ))
          separator <- plotit( low, high, fig, horizontal )
          usr <- par("usr")
          cmdlocs <- newlocs( cmds, data = is.data, usr = usr)
        },
        add =, delete =, replace = {
          ans <- newans
        }
      )
      if( use.data ) {
        tmp <- rescale.temp( getTemp( community, "Low" ), getTemp( community, "High" ),
          rescale.data[1:2], rescale.data[3:4] )
        for( i in c("low","high") ) {
          datax <- tmp[[i]]$knots
          lines( datax, stats::predict( tmp[[i]], datax )$y )
        }
      }
      plotcmd( ans, fig, cmds, cmdlocs, usr, data = use.data )
    }
    else { # modify the curve knots
      fit <- get( fig )
      if( z$x >= min( fit$knots ) & z$x <= max( fit$knots )) {
        fit <- curve.plot( as.data.frame( stats::predict( fit, fit$knots )), n = n, action = ans,
          z = z, fit = fit, backfit = FALSE, save.ends = 2, col = col[fig] )
        assign( fig, fit$fit )
      }
      ans <- "replace"
      plotcmd( ans, fig, cmds, cmdlocs, usr, data = use.data )
    }
    if( newans == "finish" )
      break
  }
  plotcmd( newans, fig, cmds, cmdlocs, "red", data = use.data )
  community <- setTemp( community, "Low", low )
  community <- setTemp( community, "High", high )
  activeTemp( community )
}
###########################################################################################
rescale.temp <- function( low, high, newry = ry, newrx = knotrange,
  ry = range( c( stats::predict( low, low$knots )$y, stats::predict( high, high$knots )$y )))
{
  if( max( abs( ry - newry )) > 0 ) {
    tmpy <- diff( newry ) / diff( ry )
    low$coefficients <- low$coefficients * tmpy
    high$coefficients <- high$coefficients * tmpy
    tmpy <- newry[1] - ry[1] * tmpy
    low$coefficients[,1] <- tmpy + low$coefficients[,1]
    high$coefficients[,1] <- tmpy + high$coefficients[,1]
  }
  rx <- range( low$knots, high$knots )
  if( max( abs( rx - newrx )) > 0 ) {
    newx <- diff( newrx ) / diff( rx )
    low$knots <- newrx[1] + newx * low$knots
    high$knots <- newrx[1] + newx * high$knots
    tmp <- 1
    for( i in seq( 2, ncol( low$coefficients ))) {
      tmp <- tmp * newx
      low$coefficients[,i] <- low$coefficients[,i] / tmp
      high$coefficients[,i] <- high$coefficients[,i] / tmp
    }
  }
  list( low = low, high = high )
}
###########################################################################################
### Temperature data structure
### Min	minimum temperature for degree-day computations
### Time	list of hours when temperature changes through a day
### Base	list of temperature shifts through a day
###		list names indicate first day Hour and Base applies
### Low	spline fit for daily low temperatures
### High	spline fit for daily high temperatures
### DegreeDay	spline fit (hour->DD) for currently active days
### Hour		ramped backspline fit (DD->hour) for currently active days
### Idea is that DegreeDay and Hour are updated whenever a future event is scheduled
### past the latest future event already scheduled. At that time, the first
### and last time of fit are both adjusted. This can be done without rebuilding
### the whole spline fit by appropriate adjustment of knots and coefficents.
### Steps:	(1) combine Hour, Low and High to get knots
###		(2) build spline coefficients
###########################################################################################
temp.spline <- function( community, hour, temp, start = 0,
  mintemp = getTemp( community, "Min" ), cumulative = TRUE,
  mult = getTemp( community, "Unit" ) )
{
  if( is.data.frame( hour ) & missing( temp )) {
    temp <- hour$temp
    hour <- hour$hour
  }
  ## drop low value if ones on either side are low
  low <- temp <= mintemp
  toolow <- low & c(FALSE,low[-length(low)]) & c(low[-1],FALSE)
  hour <- hour[!toolow]
  temp <- temp[!toolow]
  lh <- length( hour )
  ## expand single low in middle to two
  low <- temp <= mintemp
  toolow <- low & c(FALSE,!low[-length(low)]) & c(!low[-1],FALSE)
  if( any( toolow )) {
    hour <- c(hour,hour[toolow])
    temp <- c(temp,temp[toolow])
    temp <- temp[ order( hour ) ]
    hour <- sort( hour )
  }
  ## truncate on left
  low <- temp <= mintemp & c(temp[-1] > mintemp,FALSE)
  low1 <- c(FALSE,low[-lh])
  if( any( low ))
    hour[low] <- hour[low] + ( hour[low1] - hour[low] ) * ( mintemp - temp[low] ) /
      ( temp[low1] - temp[low] )
  ## truncate on right
  low <- temp <= mintemp & c(FALSE,temp[-lh] > mintemp)
  low1 <- c(low[-1],FALSE)
  if( any( low ))
    hour[low] <- hour[low] + ( hour[low1] - hour[low] ) * ( mintemp - temp[low] ) /
      ( temp[low1] - temp[low] )
  temp[ temp < mintemp ] <- mintemp
  temp <- temp[ !duplicated( hour ) ]
  hour <- unique( hour )

  ## linear interpolating spline for temp
  lh <- length( hour ) - 1
  h0 <- 1:lh
  aa <- ( temp[1+h0] - temp[h0] ) / ( hour[1+h0]-hour[h0] )
  aa <- c( aa, aa[lh] )
  bb <- temp - mintemp
  s <- list( knots = hour, coefficients = cbind( bb,aa,0,0) )
  dimnames( s$coefficients ) <- list( paste( floor( hour / mult ), round( hour %% mult ),
    sep = "." ), c("const","linear","quad","cubic") )
  attr(s,"formula") <- temp ~ hour
  class( s ) <- c("npolySpline","polySpline","spline")
  if( !cumulative )
    return( s )

  ## quadratic spline for cumulative temp
  aa <- s$coefficients[,3] <- s$coefficients[,2] / 2 / mult / mult
  lh <- length( aa ) - 1
  h0 <- 1:lh
  h1 <- 1 + h0
  hour <- s$knots
  bb <- s$coefficients[,2] <- s$coefficients[,1] / mult
  cc <- ( hour[h1] - hour[h0] ) * ( bb[h0] + aa[h0] *
    ( hour[h1] - hour[h0] ))
  s$coefficients[,1] <- start + c( 0, cumsum( cc ))
  s
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
temp.plot <- function( community, lo.hour = s$knots[1], hi.hour = max( s$knots ),
                      length = 201,
                      col = NULL, derivative = FALSE, ..., printit = FALSE )
{
  s <- getTemp( community, "DegreeDay" )
  x <- seq( lo.hour, hi.hour, length = length )
  ## make sure to pick up knots to plot in this region
  x <- unique( sort( c( x, s$knots[ s$knots >= lo.hour & s$knots <= hi.hour ] )))
  ylab <- "degree-days"
  if( derivative ) {
    ylab <- "degrees above min"
    s$coefficients <- s$coefficients[,-1]
    for( i in seq( 2, ncol( s$coefficients )))
      s$coefficients[,i] <- s$coefficients[,i] * i
  }
  y <- stats::predict( s, x )$y
  plot( x / getTemp( community, "Unit" ), y, type = "l", xlab = "day",
    ylab = ylab, ... )
  if( printit )
    print( cbind( hour, stats::predict( s, hour )$y ))
  if( !is.null( col ))
    points( s$knots, coef(s)[,1], col = col )
  if( !derivative ) {
    s <- getTemp( community, "Hour" )
    x <- seq( min( y ), max( y ), length = length )
    tmp <- stats::predict( s, x )
    lines( tmp$y / getTemp( community, "Unit" ), tmp$x, col = "blue" )
  }
}
###########################################################################################
temp.lines <- function( s, mult = 24, col = "red" )
{
  x <- seq( s$knots[1], max( s$knots ), length = 51 )
  x <- unique( sort( c( x, s$knots )))
  p <- stats::predict( s, x )
  lines( p$x, p$y, col = col )
  if( !is.null( col ))
    points( s$knots / mult, coef(s)[,1], col = col )
}
###########################################################################################
temp.repeat <- function( community, period = range( days ))
{
  lodays <- range( getTemp( community, "Low" )$knots )
  hidays <- range( getTemp( community, "High" )$knots )
  days <- c( max( lodays[1], hidays[1] ), min( lodays[2], hidays[2] ))
  lodays <- period[1] >= days[1] & period[1] <= days[2]
  if( lodays )
    days[1] <- period[1]
  hidays <- period[2] <= days[2] & period[2] >= days[1]
  if( hidays )
    days[2] <- period[2]
  if( !( lodays & hidays )) {
    stop( paste( "\nSimulation period is outside of Temperature days:\n period =",
      paste( round( period ), collapse = "," ), "; days =",
      paste( round( days ), collapse = "," ), "\nNeed to run temp.design() and start over!" ))
  }
  period <- days

  days <- seq( days[1], days[2] )
  low <- stats::predict( getTemp( community, "Low" ), days )$y
  high <- stats::predict( getTemp( community, "High" ), days )$y
  periods <- c( as.numeric( names( getTemp( community, "Time" ))), Inf )
  temps <- hours <- numeric( )
  period[2] <- period[2] + 1
  for( i in seq( length( periods ) - 1 )) {
    day <- max( period[1], periods[i] )
    this.period <- days >= day & days < min( period[2], periods[i+1] )
    n <- sum( this.period )
    if( n ) {
      if( !is.null( getTemp( community, "Time", i ))) {
        time <- getTemp( community, "Time", i )
        base <- getTemp( community, "Base", i )
      }
      h <- length( time )
      lotemp <- rep( low[this.period], rep(h,n) )
      hitemp <- rep( high[this.period], rep(h,n) )
      temp <- ( base - min( base )) / diff( range( base ))
      temps <- c( temps, rep( temp, n ) * ( hitemp - lotemp ) + lotemp )

      hours <- c( hours, rep( time, n ) + rep( getTemp( community, "Unit" ) *
        seq( day, day + n - 1 ), rep(h,n) ))
    }
  }
  day <- days == period[2]
  if( any( day )) {
    temps <- c( temps, low[day] + temp[1] * ( high[day] -
      getTemp( community, "Low" )[day] ))
    hours <- c( hours, getTemp( community, "Unit" ) * period[2] )
  }
  data.frame( hour = hours, temp = temps )
}
##########################################################################################
showTemp <- function( community )
{
  cat( "Temperature set for days",
      paste( range( getTemp( community, "Low" )$knots ), collapse = " to " ), "\n" )
  cat( "Daily low temperature range:",
      paste( round( range( stats::predict( getTemp( community, "Low" ))$y )),
            collapse = " to " ), "\n" )
  cat( "Daily high temperature range:",
      paste( round( range( stats::predict( getTemp( community, "High" ))$y )),
            collapse = " to " ), "\n" )
  if( !is.null( getTemp( community, "DegreeDay" ) ))
    cat( "Active temperature range:",
        paste( round( range( getTemp( community, "DegreeDay", "knots" ) )),
              collapse = " to " ), "\n" )
  cat( "Run temp.design() to adjust temperature range\n" )
}
##########################################################################################
activeTemp <- function( community,
                       lo.hour = min( getTemp( community, "DegreeDay", "knots" ) ),
                       hi.hour = max( getTemp( community, "DegreeDay", "knots" ) ),
                       degreeday )
{
  unit <- getTemp( community, "Unit" )
  if( missing( hi.hour )) {
    hi.hour <- if( length( lo.hour ) > 1 )
      lo.hour[2]
    else
      lo.hour + unit
  }
  ## period is in units of hours
  period <- c( floor( lo.hour[1] / unit ), ceiling( hi.hour / unit ))
  if( missing( degreeday )) {
    degreeday <- if( is.null( getTemp( community, "DegreeDay" ) ))
      0
    else
      getDegreeDay( community, lo.hour[1] )
  }
  if( is.na( degreeday ))
    degreeday <- 0
  community <- setTemp( community, "DegreeDay",
                       temp.spline( community, temp.repeat( community, period ),
                                   start = degreeday ))
  community <- setTemp( community, "Hour",
                       break.backSpline( getTemp( community, "DegreeDay" )))

  tmp <- period * unit
  names( tmp ) <- c("lo.hour","hi.hour")
  print( tmp )
  community
}
###########################################################################################
updateTemp <- function( community,
                       period = range( getTemp( community, "DegreeDay", "knots" ) ))
{
  ## period is in units of hours
  first <- period[1]
  last <- period[2]

  s <- getTemp( community, "DegreeDay" )
  knots <- s$knots
  ## drop earlier times that are now in the past
  drop <- sum( knots < first ) - 1
  nk <- length( knots )
  change <- drop > 0
  if( change ) {
    s$knots <- knots[ - seq( drop ) ]
    s$coefficients <- s$coefficients[ - seq( drop ), ]
  }
  ## add new days to include last
  period <- ceiling( c( knots[nk], last ) / getTemp( community, "Unit" ) )
  if( period[1] <= period[2] ) {
    change <- TRUE
    news <- temp.spline( community, temp.repeat( community, period ),
      start = getHour( getTemp( community, "Unit" ) * period[1] ))
    nk <- length( s$knots )
    if( s$knots[nk] == news$knots[1] ) {
      s$knots <- s$knots[-nk]
      s$coefficients <- s$coefficients[-nk,]
    }
    else
      s$coefficients[nk,] <- news$coefficients[1,]
    s$knots <- c( s$knots, news$knots )
    s$coefficients <- rbind( s$coefficients, news$coefficients )    
  }
  if( change ) {
    community <- setTemp( community, "DegreeDay", s )
    community <- setTemp( community, "Hour", break.backSpline( s ))
  }
  community
}
###########################################################################################
ramp.backSpline <- function( s )
{
  ## ramped backspline is a trick to get backSpline when curve is flat in spots
  ## if tmp <- stats::predict( ramp.backSpline( s ))
  ## then plot tmp$y versus tmp$x-tmp$y to "recover" original curve.
  ## problem is that one cannot recover particular x this way!

  s$coefficients[,1] <- s$coefficients[,1] + s$knots
  s$coefficients[,2] <- s$coefficients[,2] + 1
  backSpline( s )
}
###########################################################################################
break.backSpline <- function( tmp )
{
  ## alternative to ramped backSpline that first removes flat regions (slope 0)
  ## problem remains that leftover may still have slope 0 at a point
  ## this can cause anomolous results!

  ## find flat regions and cut out
  tmpc <- diff(tmp$coefficients[,1]) == 0 
  tmpk <- cumsum( diff(tmp$knots) * ( tmpc ))
  tmpk <- c(tmpk,tmpk[length(tmpk)])
  tmp$knots <- tmp$knots - tmpk
  tmpc <- c(!tmpc,TRUE)
  tmp$knots <- tmp$knots[tmpc]
  tmp$coefficients <- tmp$coefficients[tmpc,]
  tmp$breaks
  nb <- dim( tmp$coefficients )

  ## kludge for backspline: cannot handle slope of zero
  tmpn <- tmp$coefficients[,2] == 0
  if( any( tmpn )) {
    tmpn <- seq( tmpn )[ tmpn ]
    tmp$coefficients[ tmpn, 2:nb[2] ] <-
      ( tmp$coefficients[ pmin( nb[1], tmpn + 1 ), 2:nb[2] ] +
       tmp$coefficients[ pmax(     1, tmpn - 1 ), 2:nb[2] ] ) / 2
  }

  ## back spline
  tmpb <- backSpline(tmp )

  if( any( tmpc )) {
    ## shift back spline based on breaks
    tmpb$coefficients[,1] <- tmpb$coefficients[,1] + tmpk[tmpc]
  }
  tmpb
}
###########################################################################################
getDegreeDay <- function( community, hour )
{
  stats::predict( getTemp( community, "DegreeDay" ), hour )$y
}
###########################################################################################
getHour <- function( community, dd )
{
  stats::predict( getTemp( community, "Hour" ), dd )$y
}
###########################################################################################
getTime <- function( community, species, x )
{
  if( is.na(x)){
    cat("getTime missing value\n")
    browser()
  }
  switch( getOrgFeature( community, species, "units" ),
    ## organisms on hour basis assumed to be active only 6am-6pm
    hr = getDegreeDay( community,
      ( getTemp( community, "Unit" ) / 2 + x + floor( x / getTemp( community, "Unit" ) )) / 2 ),
    DD = x )
}
###########################################################################################
checkTime <- function( community, x, base, units )
{
## NOTE: Sometimes base can be negative!! (reset to 0)
  if( units == "hr" ) {
    x <- max( x )
#    print( c( base = base, x = x, knots = range( getTemp( community, "DegreeDay", "knots" ))))
    if( x > max( getTemp( community, "DegreeDay", "knots" )))
      community <- activeTemp( community, max( base, 0 ),
                              x + getTemp( community, "Unit" ) )
  }
  community
}
###########################################################################################
transTime <- function( community, org1name, org2name, x,
                      unit1 = getOrgFeature( community, org1name, "units" ),
                      unit2 = getOrgFeature( community, org2name, "units" ))
{
  if( is.na(x)){
    cat("transTime missing value\n")
    browser()
  }
  if( unit1 == unit2 )
    return( x )
  switch( unit1,
    ## organisms on hour basis assumed to be active only 6am-6pm
    hr = getDegreeDay( community,
      ( getTemp( community, "Unit" ) / 2 + x + floor( x / getTemp( community, "Unit" ))) / 2 ),
    DD = getHour( x ))
}
###########################################################################################
### To do:
### 1. interactive designer for hourly temp fluctation over one day
### 2. interactive designer for daily lows and highs over season: temp.design() DONE
### 3. check future event trees hour vs. DD
### 4. schedule interaction events with hour-DD translation
### 5. make aphytis dormant at night
###########################################################################################
