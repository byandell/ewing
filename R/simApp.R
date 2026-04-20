#' Ewing Simulation Engine App
#' 
#' Central evaluation module directing state flow interactions spanning across discrete evaluation limits
#' ($nsim > 1$) and linear continuous event evaluations ($nsim == 1$).
#' 
#' @param title Application title
#' @param id module ID string
#' @param init_par reactive list mapping initial population sizes
#' @param datafile reactive matrix filepath containing physical environment and configuration targets
#' @export
#' @importFrom shiny actionButton fileInput fluidRow h4 HTML moduleServer NS observeEvent radioButtons reactiveVal req sliderInput tagList withProgress
simApp <- function(title = "Population Ethology Simulation") {
  # Stub for testing simApp individually if desired
  ui <- bslib::page_sidebar(
    title = title,
    sidebar = bslib::sidebar(
      initParInput("init_par"),
      simInput("sim")
    ),
    simUI("sim"),
    initParOutput("init_par")
  )
  server <- function(input, output, server) {
    init_par <- initParServer("init_par")
    simServer("sim", init_par)
  }
  shiny::shinyApp(ui = ui, server = server)
}

#' @export
#' @rdname simApp
simServer <- function(id, init_par, datafile = shiny::reactiveVal("")) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    active_sim <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$go_init, {
      nsim <- as.integer(input$nsim)
      if(nsim == 1) {
        siminit <- init.simulation(count = as.numeric(c(shiny::req(init_par$host), shiny::req(init_par$parasite))),
                                   datafile = datafile())
        # automatically advance by step_size to avoid step 0 blank plots
        new_state <- future.events(siminit, nstep = input$step_size, plotit = FALSE)
        active_sim(new_state)
      } else {
        shiny::withProgress(message = paste('Ewing Discrete', nsim, 'Simulations ...'),
                            value = 0,
                            {
                              out <- as.list(seq_len(nsim))
                              inc <- 1 / nsim
                              for(i in seq_len(nsim)) {
                                shiny::incProgress(inc)
                                out[[i]] <- ewing_discrete1(
                                  count = as.numeric(c(shiny::req(init_par$host), shiny::req(init_par$parasite))),
                                  nstep = input$steps) # Does ewing_discrete1 accept datafile? Original didn't pass it.
                              }
                              active_sim(make_ewing_discrete(out))
                            })
      }
    })

    shiny::observeEvent(input$go_step, {
      nsim <- as.integer(input$nsim)
      if(nsim == 1) {
        shiny::req(active_sim())
        new_state <- future.events(active_sim(), nstep = input$step_size, plotit = FALSE)
        active_sim(new_state)
      }
    })

    simres <- shiny::reactive({ active_sim() })
    
    # Return both the active reactive and the input bindings natively
    list(
      simres = simres,
      nsim = shiny::reactive({ as.integer(input$nsim) }),
      input = input
    )
  })
}

#' @export
#' @rdname simApp
simInput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("steps"),
                       label = "Simulation steps:",
                       min = 1000,
                       max = 10000,
                       value = 1000,
                       step = 500),
    shiny::radioButtons(ns("nsim"),
                        "Number of Simulations",
                        c(1,10,20,50,100,200),
                        1, inline = TRUE),
    shiny::sliderInput(ns("step_size"),
                       label = "Steps per click (for nsim=1):",
                       min = 10,
                       max = 500,
                       value = 50,
                       step = 10)
  )
}

#' @export
#' @rdname simApp
simUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::actionButton(ns("go_init"), "Init / Run")),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] > 0 && input['%s'] == '1'", ns("go_init"), ns("nsim")),
        shiny::column(6, shiny::actionButton(ns("go_step"), "Step Forward"))
      )
    )
  )
}
