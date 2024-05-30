###########################################################################################
### Organism Features
###########################################################################################
getOrgFeature <- function( community, species, feature = names( OrgFeature ))
{
  OrgFeature <- getOrgInfo( community, "Feature" )
  ## The global Organism$Feature contains static features of all organisms.
  ## Return the feature as numeric if possible, or as character.
  ## Missing value (NA) indicates no such feature is possible for that organism.
  if( missing( species ))
    return( row.names( OrgFeature ))
  f <- OrgFeature[ species, feature ]
  if( length( feature ) == 1 ) {
    if( any( is.na( f )))
      return( NA )
    f <- as.character( f )
  }
  else {
    f <- apply( f, 2, as.character )
  }
  f <- c( unlist( f ))
  ## try to interpret f as numeric
  opwarn <- options( warn = -1 )
  numf <- suppressWarnings(as.numeric( f ))
  options( opwarn )
  if( all( !is.na( numf )))
    f <- numf
  f
}
##########################################################################################
getOrgHosts <- function( community, species,
                         feature = c("offspring","attack","substrate") ###HOST SPECIFIC###
)
{
  # This seems overly complicated and adds substrate; maybe function name is wrong
  f <- unique( getOrgFeature( community, species, feature ))
  # f[match(species, f, nomatch = 0)] # this would only get species
  o <- getOrgFeature( community )
  o[ match( f, o, nomatch = 0 ) ]
}
###########################################################################################
getOrgFuture <- function( community, species, feature, current,
                          future = OrgFuture[[species]] )
{
  OrgFuture <- getOrgInfo( community, "Future" )
  ## The global Organism$Future[[species]] contains future event information.
  ## Return the information as numeric if possible, or as character.
  ## Missing value (NA) indicates no such feature is possible for that organism.
  if( missing( current )) {
    if( missing( feature ))
      return( future )
    future <- future[,feature]
  }
  else {
    if( !is.numeric( current ))
      current <- match( current, future$current, nomatch = 0 )
    if( missing( feature ))
      future <- future[ current, ]
    else
      future <- future[ current, feature ]
  }
  if( is.null( future ))
    return( NA )
  if(is.character( future ))
    future <- as.factor(future)
  future
}
###########################################################################################
get.interact <- function( community, species, host, avail, event )
{
  id <- get.species.element( community, host, "stage", avail )
  interact <- getOrgInteract( community, host, species, event )[id]
  interact[ is.na( interact ) ] <- 0
  interact
}
###########################################################################################
getOrgInteract <- function( community,
                            org1name = getOrgFeature( community, org2name, "substrate" ),
                            org2name, event = NULL )
{
  OrgInteract <- getOrgInfo( community, "Interact" )
  ## The global org$Interact[[org1name]][[org2name]] contains interaction information.
  tmp <- OrgInteract[[org1name]][[org2name]]
  if( is.null( event ))
    return( tmp )
  event <- as.character( event )
  inter <- tmp[,event]
  if( length( event ) == 1 )
    names( inter ) <- row.names( tmp )
  if(is.character(inter))
    inter <- factor(inter)
  inter
}
###########################################################################################
getOrgMeanValue <- function( community, species )
{
  OrgMeanValue <- getOrgInfo( community, "MeanValue" )
  ## The global org$MeanValue[[species]] contains mean value information.
  OrgMeanValue[[species]]
}
###########################################################################################
copyOrgInfo <- function( fromname, toname )
{
  for( i in c("sim")) {
    from <- paste( fromname, i, sep = "." )
    if( exists( from )) {
      toto <- paste( toname, i, sep = "." )
      assign( toto, get( from ), ".GlobalEnv" )
      cat( "copied", from, "to", toto, "\n" )
    }
  }
  invisible()
}
###########################################################################################
get.alive <- function( community, species, substrate )
{
  alive <- getOrgAlive( community, species )
  alive <- seq( length( alive ))[alive]
  alive[ substrate == get.species.element( community, species, "sub.stage", alive ) ]
}
###########################################################################################
getOrgAlive <- function( community, species, element )
{
  organism <- get.species( community, species )
  ## identify dead organisms (free nodes for leftist tree)
  tmp <- c( FALSE, apply( organism[c("dist","up","left","right"),-1], 2,
                          function( x ) any( x > 1 )))
  if( !any( tmp )) {
    tmpp <- organism["up",1]
    if( tmpp > 1 )
      tmp[tmpp] <- TRUE
  }
  if( !missing( element ))
    tmp <- organism[ element, tmp ]
  tmp
}
###########################################################################################
getOrgAgeClass <- function( community, species, stage = seq( nrow( future )),
                            future = getOrgFuture( community, species ))
{
  ageclass <- future$ageclass[stage]
  tmp <- !is.na( ageclass )
  if( any( tmp ))
    ageclass[ !is.na( ageclass ) ]
  else
    NA
}
###########################################################################################
getOrgSubstrate <- function( community, species, elements = seq( nrow( inter )),
                             substrate = getOrgFeature( community, species, "substrate" ),
                             inter = getOrgInteract( community, substrate, species ))
{
  sites <- inter$substrate[elements]
  tmp <- !is.na( sites )
  if( any( tmp ))
    sites[ !is.na( sites ) ]
  else
    NA
}
###########################################################################################
sampleOrgSubstrate <- function( community, species, elements = seq( nrow( inter )),
                                substrate.name = getOrgFeature( community, species, "substrate" ),
                                inter = getOrgInteract( community, substrate.name, species ))
{
  if( is.na( substrate.name ))
    return( elements )
  newsub <- as.matrix( cbind( elements, inter[ elements, levels( factor(inter$substrate) ) ] ))
  apply( newsub, 1, function( x, is ) {
    ns <- sample( levels( factor(is) ), 1, prob = x[-1] / sum( x[-1] ))
    sub <- seq( nrow( inter ))[ ns == is ]
    if( length( sub ) > 1 ) {
      newsub <- getOrgInteract( community, substrate.name, substrate.name )[x[1],sub]
      sample( sub, 1, prob = newsub / sum( newsub ))
    }
    else
      sub
  }, inter$substrate )
} 
##########################################################################################
### simulation organism administration
##########################################################################################
initOrgInfo <- function( package, messages = TRUE, datafile = "", ... )
{
  community <- list( pop = list( ))
  community$org <- list( )
  community$org$package <- package
  ## Get data
  community$org$Feature <- getOrgData(community, "organism", "features",
                                      messages, datafile)
  
  community$pop <- list()
  community
}
##########################################################################################
setOrgInfo <- function( community, species, hosts, package, messages = TRUE,
                        datafile = "", ... )
{
  Organism <- community$org
  
  if( is.null( Organism$Future ))
    Organism$Future <- list( )
  if( is.null( Organism$Interact ))
    Organism$Interact <- list( )
  
  for( j in hosts )
    if( is.null( Organism$Interact[[j]] ))
      Organism$Interact[[j]] <- list( )
  
  ## Do not reset MeanValue as it may have important spline fits!
  if( is.null( Organism$MeanValue ))
    Organism$MeanValue <- list( )
  
  for( i in species ) {
    future <- getOrgData(community, "future", i,
                         messages, datafile)
    
    # Check that future agrees with organism.feature information
    subclass <- Organism$Feature[i,"subclass"]
    if(!(subclass %in% unique(future$ageclass))) {
      stop(paste("Future table", paste("future", i, sep = "."),
                 "does not include", subclass))
    }
    
    level.ageclass <- unique( future$ageclass )
    level.ageclass <- as.character( level.ageclass[ !is.na( level.ageclass ) ] )
    future$ageclass <- ordered( future$ageclass, level.ageclass )
    Organism$Future[[i]] <- future
    for( j in hosts )
      if( i != j ) {
        Organism$Interact[[j]][[i]] <- getOrgData(community, j, i,
                                                  messages, datafile)
        
        # Check that interaction agrees with host current stage information
        # This is messy!
        if(j %in% species) {
          if(!all(row.names(Organism$Interact[[j]][[i]]) %in%
                  c(as.character(Organism$Future[[j]]$current), i))) {
            stop(paste("Interaction table", paste(j, i, sep = "."),
                       "does not match", j, "current stages"))
          }
        }
      }
    if( is.null( Organism$MeanValue[[i]] ))
      Organism$MeanValue[[i]] <- list( )
    else
      cat( "Keeping Mean Value information for", i, "if any\n" )
  }
  for( i in unique( getOrgFeature( community, species, "substrate" ))) {
    Organism$Interact[[i]][[i]] <- getOrgData(community, i, i,
                                              messages, datafile)
  }
  community$org <- Organism
  community
}
###########################################################################################
getOrgData <- function(community, left, right,
                       messages = TRUE, datafile = "")
{
  # Get Organism Data from
  #     package data
  #     global data supplied by user
  #     external data file supplied by user
  sheet <- paste( left, right, sep = "." )
  if((data_exists <- (datafile != ""))) {
    if(dir.exists(datafile)) {
      extensions <- c(".txt", ".tsv", ".csv", ".xls", ".xlsx")
      datafile <- file.path(datafile, paste0(sheet, extensions))
      data_exists <- file.exists(datafile)
      if(any(data_exists)) {
        datafile <- datafile[data_exists][1]
        data_exists <- TRUE
      } else {
        data_exists <- FALSE
      }
      sheet <- ""
    } else { # datafile is a file, which must be xls or xlsx
      data_exists <- TRUE
    }
  }
  if(!data_exists) {
    # Load package data or get user-provided global data.
    mydata( sheet, getOrgInfo( community, "package" ), messages = messages)
    my.eval( sheet )
  } else {
    # Read data file from user if provided.
    if(sheet == "")
      my.read(datafile)
    else {
      out <- as.data.frame(readxl::read_excel(datafile, sheet = sheet, .name_repair = "none"))
      if(names(out)[1] == "") { # first column is actual row names
        rownames(out) <- out[[1]]
        out[[1]] <- NULL
      }
      out
    }
  }
}
###########################################################################################
getOrgInfo <- function( community, element )
{
  community$org[[element]]
}
###########################################################################################
setOrgMeanValue <- function( community, species, stage, mvalue )
{
  ## The global Organism$MeanValue[[species]] contains mean value information.
  community$org$MeanValue[[species]][[stage]] <- mvalue
  community
}
