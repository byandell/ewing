# Default Choices for Input Datasets

Discovers or returns default dataset table names from a given ecosystem
subfolder under \`extdata/\` or a custom directory path.

Scans the package \`extdata/\` directory and optional extra directories
for available ecosystem folders.

Shiny module for inspecting simulation input parameter data tables,
organism features, stage transition futures, and substrate interaction
matrices across ecosystems.

Shiny UI module to select from available ecosystem systems.

Server module returning a reactive for the selected system.

Interactive Shiny application to explore datasets across different
systems/ecosystems.

## Usage

``` r
default_choices(ecosystem = "default")

default_choice(ecosystem = "default")

available_ecosystems(extra_dirs = character(0))

discover_dataset_tables(datafile = "", sim = NULL, ecosystem = "default")

inputApp(title = "Input Data Explorer", ecosystem = "default")

inputAppInput(id, choices = NULL, ecosystem = "default")

inputAppOutput(id)

inputAppServer(
  id,
  simres = shiny::reactiveVal(NULL),
  datafile = shiny::reactiveVal(""),
  ecosystem = "default"
)

inputSystemInput(id, choices = NULL, selected = "default")

inputSystemServer(id)

inputSystem(title = "System Data Explorer", selected = "default")
```

## Arguments

- ecosystem:

  Reactive or character target ecosystem name or directory path.
  Defaults to \`"default"\`.

- extra_dirs:

  Optional character vector of extra directory paths to scan.

- datafile:

  Reactive optional datafile path or string

- sim:

  Simulation instance (\`ewing\` object)

- title:

  Application title

- id:

  Module ID string

- choices:

  Optional vector of system choices. Defaults to
  \`available_ecosystems()\`.

- simres:

  Reactive simulation state (\`ewing\` object)

- selected:

  Initial system selection. Defaults to \`"default"\`.

## Value

Character vector of dataset table names with \`"organism.features"\` as
the first element if present.

Character vector of available ecosystem directory names.

Reactive expression returning the selected system name or directory.
