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
#' 
#' \dontrun{
#' future.meanvalue( community, "host", "first.instar" )
#' }
#' 
#' 
#' @export future.meanvalue
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
