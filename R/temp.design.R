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
#' 
#' \dontrun{
#' temp.design()
#' temp.plot()
#' }
#' 
#' 
#' @export temp.design
#' @importFrom stats predict
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
