# Simulation Step Size Slider and Parsing Controls

Centralized utilities for discrete simulation step controls offering
logarithmic step choices (1, 2, 5, 10, 20, 50, 100, 200, 500, 1000,
2000), axis units selection modules (Steps vs Time/Days), and Age
Classes display control modules.

## Usage

``` r
step_size_choices

step_size_slider(inputId, label = "Steps per click:", selected = 50)

parse_step_size(val)

axisUnitInput(id, time_label = "Time", selected = "step")

axisUnitServer(id)

ageClassControlInput(id, time_label = "Time")

ageClassControlServer(id)
```

## Arguments

- inputId:

  Shiny input ID string

- label:

  Slider label string (default: \`"Steps per click:"\`)

- selected:

  Default selected step value or choice

- val:

  Input value from slider to parse

- id:

  Module ID string

- time_label:

  Time unit label string for radio button display (default: \`"Time"\`,
  e.g. \`"Days"\`)
