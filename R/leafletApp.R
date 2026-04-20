#' Interactive Leaflet Mapping UI (Input)
#' @param id Module ID
#' @export
#' @importFrom leaflet leafletOutput
#' @rdname leafletApp
leafletInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Interactive Map Explorer"),
    shiny::p("Search for a geographical feature, or click on the map to automatically discover its USGS Watershed Boundary."),
    leaflet::leafletOutput(ns("mapper"), height = "500px"),
    shiny::br(),
    shiny::uiOutput(ns("huc_status"))
  )
}

#' Interactive Leaflet Mapping UI (Output)
#' @param id Module ID
#' @export
#' @rdname leafletApp
leafletOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Extra output if needed, e.g. mapping details
  )
}

#' Interactive Leaflet Mapping Server Logic
#'
#' @param id Module ID
#' @export
#' @importFrom leaflet renderLeaflet leafletProxy addPolygons clearShapes
#' @rdname leafletApp
leafletServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Store dynamic reactive outputs
    status_msg <- shiny::reactiveVal("")
    huc_boundary <- shiny::reactiveVal(NULL)
    
    output$huc_status <- shiny::renderUI({
      shiny::HTML(status_msg())
    })
    
    # Render the initial basemap (Option A: Search included)
    output$mapper <- leaflet::renderLeaflet({
      build_base_map()
    })
    
    # Observer for User Clicks (Option B: Dynamic HUC intersection)
    shiny::observeEvent(input$mapper_click, {
      click <- input$mapper_click
      if (is.null(click)) return()
      
      status_msg(paste0("<div style='color:blue;'><b>Processing:</b> Connecting to USGS at Coordinate [", 
                        round(click$lng, 4), ", ", round(click$lat, 4), "]...</div>"))
      
      # Reverse-geocode the click point to its structural USGS bounds
      shiny::withProgress(message = 'Discovering HUC Boundary...', value = 0.5, {
        huc <- get_huc_from_point(lng = click$lng, lat = click$lat)
        
        if (!is.null(huc) && nrow(huc) > 0) {
          # Success
          huc_id <- huc$huc12[1]
          huc_name <- huc$name[1]
          status_msg(paste0("<div style='color:green;'><b>Found HUC12:</b> ", huc_id, " (", huc_name, ")</div>"))
          
          # Render the polygon bounds dynamically back over the interactive widget
          leaflet::leafletProxy("mapper") |>
            leaflet::clearShapes() |>
            leaflet::addPolygons(
              data = huc,
              color = "#E74C3C",
              weight = 3,
              fillOpacity = 0.2,
              layerId = "huc_poly",
              popup = paste0("<b>HUC12:</b> ", huc_id, "<br/><b>Name:</b> ", huc_name)
            )
            
          huc_boundary(huc)
        } else {
          # Failure bounds (oceans, disconnected mapping grids)
          leaflet::leafletProxy("mapper") |> leaflet::clearShapes()
          huc_boundary(NULL)
          status_msg("<div style='color:orange;'><b>Warning:</b> No USGS Watershed topology found at this location. Ensure click is within US territory.</div>")
        }
      })
    })
  })
}

#' Run the Leaflet Mapping Integration App Tracker
#'
#' @export
#' @rdname leafletApp
leafletApp <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Ewing Spatial Interaction Discovery"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        leafletInput("mapper")
      ),
      shiny::mainPanel(
        leafletOutput("mapper")
      )
    )
  )
  server <- function(input, output, session) {
    leafletServer("mapper")
  }
  shiny::shinyApp(ui, server)
}
