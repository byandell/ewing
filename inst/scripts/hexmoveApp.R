## Launch Organism Movement on Hex Grid App
library(ewing)

# Initialize simulation and advance 100 steps
mysim <- init.simulation()
mysim <- future.events(mysim, nstep = 100)

# Launch interactive hex movement app
hexmoveApp(mysim)
