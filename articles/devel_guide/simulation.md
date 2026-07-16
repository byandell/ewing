# Simulation & Parameters Configuration Modules

This sub-guide explains how parameter configs are read and initialized,
and how stochastic simulations are run, cached, and stepped forward
within the **ewing** Shiny dashboard.

------------------------------------------------------------------------

## 1. Parameters Configuration Panel (`initParApp`)

The parameters panel, defined in
[R/initParApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/initParApp.R),
allows researchers to view default configuration matrices and upload
custom configurations.

### Configuration Source Workflow

The system reads parameters in two ways: 1. **Default Config**: Falls
back to the package-wide Excel workbook
[default.xlsx](file:///Users/brianyandell/Documents/Research/ewing/ewing/data/default.xlsx).
2. **User Overwrite**: Users can upload a custom Excel workbook `.xlsx`
containing the required worksheets (e.g. `organism.features`,
`future.host`, etc.).

### UI Inputs and Outputs

- [`initParInput()`](https://byandell.github.io/ewing/reference/initParApp.md):
  Renders sliders dynamically for species starting sizes based on the
  organism spreadsheet (typically `host` and `parasite` sizes in the
  range 0–500).
- [`initParUI()`](https://byandell.github.io/ewing/reference/initParApp.md):
  Outputs a list of active input selections.
- [`initParOutput()`](https://byandell.github.io/ewing/reference/initParApp.md):
  Displays the selected workbook sheet using a searchable, paginated
  [`DT::dataTableOutput`](https://rdrr.io/pkg/DT/man/dataTableOutput.html).

### Server Functions (`initParServer`)

- Reads the worksheets of the Excel workbook using the `readxl` library.
- Dynamically creates UI sliders for each species retrieved by
  `get.organisms()`.
- Updates the active data table `output$org_table` dynamically when the
  user changes sheet selection in the dropdown.

------------------------------------------------------------------------

## 2. Simulation Execution Panel (`simApp`)

The simulation execution panel, defined in
[R/simApp.R](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/simApp.R),
manages simulation runs and step-wise progression.

### User Interface Elements

- [`simInput()`](https://byandell.github.io/ewing/reference/simApp.md):
  - **Simulation steps**: Total events count (1,000 to 10,000) for
    multi-simulations.
  - **Number of Simulations (`nsim`)**: Mode selector (1 for single
    interactive runs, 10–200 for confidence bounds).
  - **Steps per click**: The chunk size (10 to 500) to advance single
    runs.
- [`simUI()`](https://byandell.github.io/ewing/reference/simApp.md):
  - **Init / Run Button (`go_init`)**: Spawns the initial community.
  - **Step Forward Button (`go_step`)**: Appears using
    [`shiny::conditionalPanel`](https://rdrr.io/pkg/shiny/man/conditionalPanel.html)
    only when `nsim == 1` and the simulation has been initialized.

### Server Implementation (`simServer`)

The module server manages an internal reactive value `active_sim`
containing the state. The execution logic flows as follows:

``` mermaid
graph TD
    ClickGo[User clicks Init / Run] --> CheckNsim{Is nsim == 1?}
    
    CheckNsim -->|Yes: Single Run| InitSim[init.simulation]
    InitSim --> AdvanceFirst[future.events step_size]
    AdvanceFirst --> SetState[active_sim state updated]
    
    CheckNsim -->|No: Multi Run| LoopDiscrete[Loop nsim times]
    LoopDiscrete --> EDiscrete[ewing_discrete1 steps]
    EDiscrete --> MakeDiscrete[make_ewing_discrete]
    MakeDiscrete --> SetState
    
    ClickStep[User clicks Step Forward] --> CheckInit{Is initialized?}
    CheckInit -->|Yes| AdvanceMore[future.events active_sim step_size]
    AdvanceMore --> SetState
```

### Key R Core Integrations

- **[init.simulation()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/init.simulation.R)**:
  Generates the `community` structure.
- **[future.events()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/future.events.R)**:
  Advances the `community` state by performing event queue pops and
  executing competing risks event handlers (death, birth, or movement).
- **[ewing_discrete1()](file:///Users/brianyandell/Documents/Research/ewing/ewing/R/ewing_discrete.R)**:
  Fast C-like wrapper evaluating single runs to build discrete bounds.
