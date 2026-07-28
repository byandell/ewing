###########################################################################################
### Organism Features
###########################################################################################
#' @importFrom stringr str_remove
#' @importFrom readxl excel_sheets read_excel
getOrgFeature <- function( community, species, feature = names( OrgFeature ))
{
  OrgFeature <- getOrgInfo( community, "Feature" )
  if (!is.null(OrgFeature)) {
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
    numf <- suppressWarnings(as.numeric( f ))
    if( all( !is.na( numf )))
      f <- numf
    return(f)
  }
  
  # Fallback for webR standalone adapter objects
  if (missing(species)) return(c("host", "parasite"))
  if (missing(feature) || is.null(feature)) return(c(substrate = "substrate", units = "units"))
  if (length(feature) == 1) {
    if (feature == "substrate") return("substrate")
    if (feature == "units") return("units")
    return(NA)
  }
  res <- rep("substrate", length(feature))
  names(res) <- feature
  res
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
  if (!is.null(OrgFuture) && !is.null(OrgFuture[[species]])) {
    future <- OrgFuture[[species]]
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
    return(future)
  }
  
  # Fallback for webR standalone adapter objects
  if (!is.null(community$pop[[species]])) {
    pch_vec <- community$pop[[species]]$pch
    col_vec <- community$pop[[species]]$col
    fut <- data.frame(pch = pch_vec, color = col_vec, stringsAsFactors = FALSE)
    if (missing(feature)) return(fut)
    if (length(feature) == 1) return(fut[[feature]])
    return(fut[, feature, drop = FALSE])
  }
  NULL
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
  if (!is.null(OrgInteract) && !is.null(OrgInteract[[org1name]][[org2name]])) {
    tmp <- OrgInteract[[org1name]][[org2name]]
    if( is.null( event ))
      return( tmp )
    event <- as.character( event )
    inter <- tmp[,event]
    if( length( event ) == 1 )
      names( inter ) <- row.names( tmp )
    if(is.character(inter))
      inter <- factor(inter)
    return(inter)
  }
  
  # Fallback for webR standalone adapter objects
  sub_names <- if (!is.null(community$sub_names)) community$sub_names else c("fr1", "fr2", "fr3", "fr4", "twig", "lftop", "lfbot")
  mat <- matrix(1, nrow = length(sub_names), ncol = 1, dimnames = list(sub_names, "substrate"))
  as.data.frame(mat)
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
  out <- list()
  for( i in c("sim")) {
    from <- paste( fromname, i, sep = "." )
    if( exists( from )) {
      toto <- paste( toname, i, sep = "." )
      out[[toto]] <- get( from )
      cat( "copied", from, "to", toto, "into list\n" )
    }
  }
  return(out)
}
###########################################################################################
get.alive <- function( community, species, substrate )
{
  alive <- getOrgAlive( community, species )
  alive <- seq_along( alive )[alive]
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
getOrgAgeClass <- function( community, species, stage = seq_len( nrow( future )),
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
getOrgSubstrate <- function( community, species, elements = seq_len( nrow( inter )),
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
sampleOrgSubstrate <- function( community, species, elements = seq_len( nrow( inter )),
                                substrate.name = getOrgFeature( community, species, "substrate" ),
                                inter = getOrgInteract( community, substrate.name, species ))
{
  if( is.na( substrate.name ))
    return( elements )
  newsub <- as.matrix( cbind( elements, inter[ elements, levels( factor(inter$substrate) ) ] ))
  apply( newsub, 1, function( x, is ) {
    ns <- sample( levels( factor(is) ), 1, prob = x[-1] / sum( x[-1] ))
    sub <- seq_len( nrow( inter ))[ ns == is ]
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
  data_exists <- FALSE
  if (is.character(datafile) && length(datafile) == 1 && nzchar(datafile)) {
    if (dir.exists(datafile)) {
      extensions <- c(".txt", ".tsv", ".csv", ".xls", ".xlsx")
      datafile_paths <- file.path(datafile, paste0(sheet, extensions))
      exist_idx <- file.exists(datafile_paths)
      if (any(exist_idx)) {
        datafile <- datafile_paths[exist_idx][1]
        data_exists <- TRUE
      } else {
        data_exists <- FALSE
      }
      sheet <- ""
    } else if (file.exists(datafile)) {
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
getOrgNames <- function(datafile = "") {
  if(datafile == "") {
    c("organism.features", "future.host", "future.parasite",
      "substrate.host", "substrate.parasite", "substrate.substrate",
      "temperature.base", "temperature.par")
  } else {
    readxl::excel_sheets(datafile)
  }
}
###########################################################################################
getOrgDataSimple <- function(community, dataname, datafile = ""){
  out <- getOrgData(
    community,
    left = stringr::str_remove(dataname, "\\..*"),
    right = stringr::str_remove(dataname, ".*\\."),
    messages = FALSE, datafile = datafile)
  # Kludge to reinstate rownames as a column
  if(!identical(rownames(out), as.character(seq_len(nrow(out))))) {
    out <- data.frame(rownames = rownames(out), out)
  }
  out
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
