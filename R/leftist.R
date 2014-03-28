## $Id: leftist.R,v 1.0 2002/12/11 yandell@stat.wisc.edu Exp $
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
## This version has list structure for individuals.
## In addition it will keep species separate.
###########################################################################################
## Triply linked leftist trees
###########################################################################################
leftist.tree <- function( x,
  tree = data.frame( root = empty ),
  key = "time",
  empty = c( time = NA, dist = 1, left = 1, right = 1, up = 1 ))
{
  node <- empty
  node[key] <- Inf
  node[c("right","left")] <- 2
  node["dist"] <- 0
  if( missing( tree ))
    names( tree ) <- NULL

  node <- empty
  node[key] <- x[1]
  base <- 2
  tree[[base]] <- node

  for( i in 1 + seq( 2, length( x ))) {
    node <- empty
    node[key] <- x[i-1]
    if( node[key] <= tree[[base]][key] ) {
      ## insert at root
      node["left"] <- base
      tree[[base]]["up"] <- i
      base <- i
      tree[[base]] <- node
    }
    else {
      tree[[i]] <- node
      ## insert in tree (e.g. merge two trees)
      tree <- as.data.frame( leftist.merge( as.matrix( tree ), i, base ))
      base <- tree["up",1]
    }
  }
  tree[[1]][c("right","left","up")] <- base
  tree
}
###########################################################################################
leftist.create <- function( tree, key = "time" )
{
  ## assume 1st element of tree is for base
  ## and keys are in place already
  base <- 2
  tree[c("right","left","up"),] <- 1
  tree["dist",-1] <- 1
  ntree <- ncol( tree )

  for( i in seq( 3, ntree )) {
    if( tree[key,i] <= tree[key,base] ) {
      ## insert at root
      tree["left",i] <- base
      tree["up",base] <- i
      base <- i
    }
    else {
      ## insert in tree (e.g. merge two trees)
      tree <- leftist.merge( tree, i, base )
      base <- tree["up",1]
    }
  }
  ## set 1st element to point to base
  tree[c("right","left","up"),1] <- base
  tree
}
###########################################################################################
leftist.merge <- function( tree, P = 1, Q = 1, R = 1, key = "time" )
{
  while( TRUE ) {
    if( P == 1 ) {
      P <- Q
      Q <- 1
    }
    if( Q == 1 ) {
      D <- tree["dist",P]
      while( R > 1 ) {
        Q <- tree["right",R]
        temp <- tree["left",R]
        if( tree["dist",temp] < D ) {
          D <- tree["dist",temp] + 1
          tree["right",R] <- tree["left",R]
          tree["left",R] <- P
        }
        else {
          D <- D + 1
          tree["right",R] <- P
        }
        tree["up",P] <- R
        tree["dist",R] <- D
        P <- R
        R <- Q
      }
      tree["up",P] <- 1
      tree[c("left","right","up"),1] <- P
      return( tree )
    }
    ## merge two right lists
    if( tree[key,P] <= tree[key,Q] ) {
      temp <- tree["right",P]
      tree["right",P] <- R
      tree["up",R] <- P
      R <- P
      P <- temp
    }
    else {
      temp <- tree["right",Q]
      tree["right",Q] <- R
      tree["up",R] <- Q
      R <- Q
      Q <- temp
    }
  }
  tree["up",1] <- P
  tree[c("left","right","up"),P] <- 1
  tree
}
###########################################################################################
leftist.remove <- function( tree, P )
{
  oldbase <- tree["up",1]
  up <- tree["up",P]

  ## merge the subtrees below P
  tree <- leftist.merge( tree, tree["left",P], tree["right",P] )

  ## reset node to empty
  tree[c("up","left","right","dist"),P] <- 1

  ## return if base node removed
  if( oldbase == P )
    return( tree )

  ## make up node leftist
  if( tree["left",up] == P )
    tree["left",up] <- tree["right",up]
  tree["right",up] <- 1
  tree["dist",up] <- 1

  ## traverse back up the tree to make sure it is leftist to base
  upup <- tree["up",up]
  left <- tree["left",upup]
  right <- tree["right",upup]
  while( tree["dist",left] < tree["dist",right] ) {
    tree["dist",upup] <- tree["dist",left] + 1
    tree["left",upup] <- right
    tree["right",upup] <- left
    upup <- tree["up",upup]
    left <- tree["left",upup]
    right <- tree["right",upup]
  }
  ## merge down and up trees
  leftist.merge( tree, oldbase, tree["up",1] )
}
###########################################################################################
leftist.birth <- function( organism, neworg, free )
{
  newbase <- organism["up",1]
  norganism <- ncol( neworg )
  j <- norganism
  nfree <- free[1]
  while( j > 0 & nfree > 1 ) {
    newbabe <- free[nfree]
    organism[,newbabe] <- neworg[,j]
    organism <- leftist.merge( organism, newbase, newbabe )
    newbase <- organism["up",1]
    nfree <- nfree - 1
    free[1] <- nfree
    j <- j - 1
  }
  if( j > 0 ) {
    norganism <- ncol( organism )
    js <- 1:j
    organism <- cbind( organism, neworg[,js] )
    for( i in norganism + js ) {
      organism <- leftist.merge( organism, newbase, i )
      newbase <- organism["up",1]
    }
  }
  list( tree = organism, base = newbase, free = free )
}
###########################################################################################
leftist.free <- function( free, id )
{
  free[1] <- tmp <- free[1] + 1
  free[tmp] <- id
  free
}
###########################################################################################
leftist.update <- function( tree, P = tree["up",1] )
{
  tree <- leftist.remove( tree, P )
  leftist.merge( tree, tree["up",1], P )
}
