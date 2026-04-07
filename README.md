# Bland Ewing's Systems Ethology

This package was initiated on 28 March 2014 based on Bland Ewing's work dating
back to the early 1970s.
The goal is to make his simulation code available as an
R package and to provide a Shiny interface to the code.
In 2026, I am using AI
([Google Gemini 3 Pro](https://gemini.google.com/)) via
[Antigravity](https://antigravity.google/)
 to improve the code and interface.

Before installing the package, you may want to examine the interactive Shiny app
<https://connect.doit.wisc.edu/SystemsEthology>,
which executes this package.
See also

- [BlandEwing](https://byandell.github.io/BlandEwing): draft life story
- [SystemsEthology](https://byandell.github.io/SystemsEthology): draft book
- [Systems Ethology--The Life and Work of Bland Ewing](https://byandell.github.io/pages/ewing/)
- [Predator-Prey Datasets](inst/doc/datasets.md)
- [Ewing Package Documentation](inst/doc/README.md)

# Install and Run Package

Change one of the simulation values on the Shiny app,
say number or hosts or parasites, or number of simulation steps.
Then click on **Start Simulation** button to start a simulation. You can redo a simulation with the same settings, or change the settings.

To install, first do

```r
install.packages("devtools")
```

If on Windows, you will then need to install Rtools from <http://cran.r-project.org/bin/windows/Rtools>.
This is an executable that will install some applications in c:\Rtools.
You will also need pdflatex, which means you need a TeX distribution such as MikTeX or TeX Live.

```r
library(devtools)

install_github("byandell/ewing")
```

If you have `pandoc`, you can install with vignette:

```r
install_github("byandell/ewing", build_vignettes = TRUE)
```

See [vignettes/ewing.Rmd](https://github.com/byandell/ewing/blob/master/vignettes/ewing.Rmd) for example use of code. A simple example is below:

```r
library(ewing) # attach package
mysim <- init.simulation() # initialize simulation
simres <- future.events(mysim, plotit = FALSE) # simulate future events
plot(simres) # plot populations by stage or substrate over time
ggplot_current(simres, "host") # plot current (last) individuals over space
```

To change the number of initial individuals, do something like:

```r
mysim <- init.simulation(count = c(200, 100))
```

will simulate 200 hosts and 100 parasites. You can also change the name of simulation file results from "sim.out" to something like "sim_200_100.out" to reflect the conditions of the simulation. Note that this file is written into the directory where you are using R.

```r
simres <- future.events(mysim, "mysim_200_100.out", plotit = FALSE)
```

Here are some commands to use `tidyverse` for newer plots:

```r
library(tidyverse)
ggplot_ewing(simres)
ggplot_current(simres, "host")
```

For more information, visit <http://www.stat.wisc.edu/~yandell/ewing>

Reference: B Ewing, BS Yandell, JF Barbieri, and RF Luck (2002) "Event-driven competing risks," _Ecological Modelling 158_: 35--50. <http://doi.org/10.1016/S0304-3800(02)00218-1>
