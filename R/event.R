## $Id: event.R,v 1.0 2002/12/11 yandell@stat.wisc.edu Exp $
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
###############################################################################
##
## event.birth( community, species )
## event.attack( community, species )
##
## Issues to resolve:
## 2. search strategy for predator/parasite/parasitoid
## 3. generic calls to do.x
###############################################################################
#' @importFrom stats runif
event.birth <- function( community, species )
{
  ## now only one offspring at a time,
  ## but could depend on individual
  offspring <- get.offspring( community, species)
  ## update parent based on anticipated offspring
  community <- parent.birth( community, species, offspring )
  if( offspring ) {
    ## get new births
    newbirths <- get.birth( community, species, offspring )
    ## merge births into community
    community <- set.birth( community, species, newbirths )
  }
  community
}
###############################################################################
parent.birth <- function( community, species, offspring )
{
  ## get individual record
  individual <- get.individual( community, species )
  individual["offspring"] <- individual["offspring"] - offspring
  ## female starves when egg load depleted (less than or equal to 0)
  if( individual["offspring"] <= 0 )
    individual["stage"] <- set.future( community, species, "starved" )
  put.individual( community, species, individual )
}
###############################################################################
get.birth <- function( community, species, offspring )
{
  ## get individual record
  individual <- get.individual( community, species )
  ## matrix of new offspring for community
  newbirths <- matrix( individual, length( individual ), offspring )
  dimnames( newbirths ) <- list( names( individual ), NULL )
  if( offspring ) {
    ## assumes newborn is stage 1, and next stage is 2
    newbirths["stage",] <- 1
    newbirths["future",] <- 2
    ## set up as unlinked node for leftist tree
    newbirths[c("dist","left","right","up"),] <- 1
    ## disperse offspring across substrate types
    newbirths <- event.move( community, species, newbirths )
  }
  if( getCount( community,, "debug" ))
    cat( round( individual["time"] ),
        getOrgFeature( community, species, "units" ),
        ":", species, "offspring", individual["offspring"], "\n" )
  if( offspring ) {
    ## get future events for new organisms
    get.future( community, species, newbirths)
  }
  else
    community
}
###############################################################################
get.deplete <- function( community, species )
{
  ## get individual record of attacker
  individual <- get.individual( community, species )
  ## Deplete reserves based on time spent searching for host.
  individual["offspring"] <- individual["offspring"] - 
    ( individual["time"] - individual["location"] ) /
      getOrgFeature( community, species, "deplete" )
  put.individual( community, species, individual )
}
###############################################################################
set.future <- function( community, species, stage )
{
  current <- getOrgFuture( community, species, "current" )
  seq( length( current ))[ current == stage ]
}
###############################################################################
### Interaction Events (only attack for now)
###############################################################################
event.attack <- function( community, species )
{
  ## dyadic event: attack of host by adult parasitoid
  
  ## deplete individual based on time spent searching for host
  community <- get.deplete( community, species )
  ## get individual record of attacker
  individual <- get.individual( community, species )

  ## find a host if parasite has offspring reserve left
  if( individual["offspring"] > 0 ) {
    ## get name of host for attacker
    host <- getOrgFeature( community, species, "attack" )
    ## get attack parasite and event types
    attack <- get.attack( community, species, individual )
    ## find host located on the same substrate
    found <- event.find( community, species, host, attack["event"] )
    if( length( found )) {
      ## host-parasite interaction
      event.parsed <- get( paste( "event", attack["event"], sep="." ))
      community <- event.parsed( community, species, host, found )
      event.parsed <- get( paste( "host", attack["parasite"], sep="." ))
      community <- event.parsed( community, species, host, found )
      individual <- get.individual( community, species )
    }
  }
  if( individual["offspring"] > 0 ) {
    ## parasite moves along substrate
    individual <- event.move( community, species, individual )
  }
  else {
    ## parasite dies if it does not feed enough
    individual["stage"] <- set.future( community, species, "starved" )
  }
  ## put updated individual back in community
  put.individual( community, species, individual )
}
###############################################################################
get.attack <- function( community, species, individual )
{
  ## get parasite type ("ecto" or "endo") and current event ("feed" or "ovip")
  parasite <- getOrgFeature( community, species, "parasite" )
  event <- getOrgFuture( community, species, "current", individual["future"] )
  event <- as.character( event )
  if( parasite=="endo" )
    event <- "ovip"
  else if( individual["offspring"] < 1 ) {
    ## must feed if depleted
    event <- "feed"
  }
  c( event = event, parasite = parasite )
}
###############################################################################
host.ecto <- function( community, species, host, dead )
{
  ## ectoparasites effectively kill their host

  ## get individual doing the attack
  individual <- get.individual( community, species )
  ## get host individual that is attacked
  hostindiv <- get.individual( community, host, dead )
  ## set host time to now, which may involve hr-DD translation
  hostindiv["time"] <- transTime( community, species, host, individual["time"] )
  ## schedule immediate death of host
  hostindiv["future"] <- set.future( community, host, "death" )
  ## update host record in community
  community <- put.individual( community, host, hostindiv, dead )
  ## update leftist tree and mintime
  community <- put.species( community, host,
                           leftist.update( get.species( community, host ), dead ))
  update_mintime( community, host )
}
###############################################################################
host.endo <- function( community, species, host, dead, harm )
{
  ## endoparasites reduces capacity of host (assumed by half here)
  
  ## get host individual that is attacked
  hostindiv <- get.individual( community, host, harm )
  ## schedule harm for hosts (reduce egg capacity by half)
  hostindiv["offspring"] <- floor( hostindiv["offspring"] / 2 )
  if( hostindiv["offspring"] == 0 ) {
    ## get individual doing the attack
    individual <- get.individual( community, species )
    ## set host time to now, which may involve hr-DD translation
    hostindiv["time"] <- transTime( community, species, host, individual["time"] )
    ## schedule immediate death of host
    hostindiv["future"] <- set.future( community, host, "death" )
  }
  ## update host record in community
  community <- put.individual( community, host, hostindiv, harm )
  if( hostindiv["offspring"] == 0 ) {
    ## update leftist tree and mintime
    community <- put.species( community, host,
                             leftist.update( get.species( community, host ), dead ))
    community <- update_mintime( community, host )
  }
  community
}
###############################################################################
event.feed <- function( community, species, host, dead )
{
  ## feed: adult parasite feeds on host

  interact <- get.interact( community, species, host, dead, "feed" )
  ## host-parasite interaction: feeding
  if( interact ) {
    individual <- get.individual( community, species )
    individual["offspring"] <- individual["offspring"] + interact
    community <- put.individual( community, species, individual )
  }
  community
}
###############################################################################
event.ovip <- function( community, species, host, dead, gender=TRUE )
{
  ## ovip: adult lays egg in the host to emerge later as adult
  offspring <- get.offspring( community, species )

  interact <- get.interact( community, species, host, dead, "ovip" )
  ## host-parasite interaction: feeding
  if( interact ) {
    ## update parent individual, depleting energy after egg laying
    community <- parent.birth( community, species, offspring )

    ## get new births
    newbirths <- get.birth( community, species, offspring )
    ## gender preference for offspring
    if( get.interact( community, species, host, dead, "male" ) < stats::runif( 1 ) |
      !gender ){
      ## set offspring for female eggs based on dead host
      newbirths["offspring", ] <- set.offspring( community, species, host, dead )
    }
    else {
      ## produce male and put on queue for immediate death
      newbirths <- set.male( community, species, host, newbirths )
    }
    community <- set.birth( community, species, newbirths )
  }
  community
}
###############################################################################
set.male <- function( community, species, host, newbirths )
{
  ## produce male offspring, which is queued for immediate death
  newbirths["future",1] <- set.future( community, species, "male" )
  newbirths["time",1] <- get.individual( community, species )["time"]
  newbirths
}
