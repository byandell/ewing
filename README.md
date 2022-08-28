Ewing's Quantitative Population Ethology

To install, first do

```
> install.packages("devtools")
```

If on Windows, you will then need to install Rtools from <http://cran.r-project.org/bin/windows/Rtools>.
This is an executable that will install some applications in c:\Rtools. 
You will also need pdflatex, which means you need a TeX distribution such as MikTeX or TeX Live.

```
> library(devtools)

> install_github("byandell/ewing")
```

If you have `pandoc`, you can install with vignette:

```
> install_github("byandell/ewing", build_vignettes = TRUE)
```

See [vignettes/ewing.Rmd](https://github.com/byandell/ewing/blob/master/vignettes/ewing.Rmd) for example use of code. A simple example is below:

```
> library(ewing) # attach package
> mysim <- init.simulation() # initialize simulation
> simres <- future.events(mysim, "mysim.out", plotit = FALSE) # simulate future events
> plot(simres) # plot populations by stage or substrate over time
> plot_current(simres, "host") # plot current (last) individuals over space
```

Here are some commands to use `tidyverse` for newer plots:

```
> ggplot_ewing(simres)
> ggplot_current(simres, "host")
``

For more information, visit <http://www.stat.wisc.edu/~yandell/ewing>

Reference: B Ewing, BS Yandell, JF Barbieri, and RF Luck (2002) "Event-driven competing risks," _Ecological Modelling 158_: 35--50. <http://doi.org/10.1016/S0304-3800(02)00218-1>

# Data organization

### Community object

The object `community` has several elements that contain the current state of a simulation. This object is updated at every simulation step, which then changes the future event structure. Past events are removed and cannot be recovered, except in the sense that each individual has their current state and anticipated future state recorded.

- `pop` matrices for each `species` of information (rows) by individual (columns)
  + use `get.species/put.species` and `get.individual/put.individual`
- `org` structure with information about organisms, including interactions
  + use `getOrgInfo`, `getOrgInteract` and other routines
- `temp` structure about degree-day and temperature patterns
- `count` structure of summary counts
- `cpu` CPU time used for various activities

For some reason, I set up `community$pop` entries for each species as matrices by row rather than column, with each column being an individual. It may be useful to flip it. The row names for each individual provide a complete picture of their current and next potential future state. It will take more digging to explain each of these row labels.

- `dist`
- `left`, `right`, `up` leftist tree links
- `dispersion`
- `location`
- `intensity` 
- `truncation`
- `rejection`
- `pos.a`, `pos.b`, `pos.c` position on substrate in triangular coordinates
- `time`
- `stage` current stage
- `future` future stage
- `offspring`
- `sex` sex
- `sub.stage` current substrate
- `sub.future` future substrate

At each time step, the individual at the top of the leftist tree is examined, and actions may be taken. This typically involves a change in the leftist tree affecting just a few individuals. It might involve scheduling a `future` event for that individual, a `death`, one or more `birth` events. Events are either monadic, involving only that individual, or dyadic, involving two individuals, such as a parasite ovipositing (preying) on a host. In the case of a dyadic event, both individuals have altered futures.

At each time step where there is a change in the population structure, totals are written out with `writeCount` to a file, which can be read by `readCount` for instance to create plots over time using `ggplot_ewing` or `plot_ewing`. This file loses the individual information, which is only maintained in the `community` object at the current step. That is, there is temporal information without individual information.

It might be possible to store the updates as the simulation goes along in such a way that one could construct intermediate snapshots of the community. One interesting question is how to capture the spatial migration of individuals over the substrates (see `ggplot_current`).

#### Global datafiles

organism.features

- columns: 	  units	offspring attack   birth  substrate	deplete subclass parasite	move
- rows: host parasite substrate

future.host

- columns:  current future fid time pch color ageclass event init
- rows: 1-17
- current values:
  + crawler first.instar first.molt second.1-3 female male second.molt third.1-3 virgin gravid death starved
- future values:
  + first.instar first.molt second.1-3 female male second.molt  death third.1-3 virgin       gravid gravid death death

future.parasite

- columns:     current   future fid time pch    color ageclass   event init
- rows: 1-11
- current values:
  + egg      larvae   prepupae pupae    adult    adult    feed     ovip     death    male     starved 
- future values:
  + larvae   prepupae pupae    adult    feed     ovip     adult    adult    death    death death

TemperaturePar

- columns: value description
- rows: Unit Days Min LowBeg LowEnd HighBeg HighEnd Length

TemperatureBase

- columns: Day	Time	Base
- rows: 12 rows
- Day: 0, 30
- Time: 0, 8, 12, 15, 18, 20
- Base: various values between 0 and 100

# Plot routines

The primary summary plot is `plot.ewing`, or `ggplot_ewing`, in `plot.R`.
This gives a summary over the whole simulation.
However, other plot routines were created, some being interactive:

- spline.R/five.plot # interactive plot to design curve with spline and backspline
- spline.R/five.show # interactive plot (same thing or different?)
- temp.R/temp.plot # plot of day to degree-day to see effect of temp variation
- temp.R/temp.design # interactive plot to design hi and lo temp ranges
- triangle.R/plot_current # plot spatial positions of hosts and parasites
- triangle.R/text_current # text add (not sure if needed?)

## Code layout

The `init.simulation` and `future.events` are the high level routines to initialize and run a simulation. They draw on default settings pulled from global datafiles.
Plan is to make this more adaptable, of course.

### Code Issues

- gencurve in spline.R internal to five.plot but used elsewise
- dead used in init, event
- organsim.features in community.R using mydata() nonstandard
- TemperatureBase in init.R community.R/initTemp
- to.plot in triangle.R plot.current not defined
- hour, knotrange undefined in temp.R
- lo.hour, hi.hour etc in temp.plot?
- File ‘ewing/R/init.R’: assign(toto, get(from), ".GlobalEnv")
- somehow crawler count ends up being negative, which throws off plot (future.events) [FIXED]
- looks like end of event.R/get.birth has hanging `get.future`

#### init.R

init.simulation # starts simulation with default info

- package = "ewing"
- community = community.R/initOrgInfo(package)
- community = community.R/initTemp(community)
- species = init.R/getOrgFeature( community )[1:2]
- hosts = init.R/getOrgHosts( community, species )
- community = community.R/setOrgInfo( community, species, hosts, package )
- loop on species i
  + count = 200 # default
  + community = init.R/init.population( community, species[i], n = count )

##### init.population

Initialize Organism Population
 
Create data structure with n organisms distributed across life stages.
Includes leftist tree structure. 
Does various initializations based in information in `future.xxx`,
where `xxx` is replaced by the name of the dataset. Normally,
`init.population` is called by `init.simulation`.

init.population # initialize populations by species

- substrate.name = init.R/getOrgFeature( community, species, "substrate" )
- substrate = init.R/getOrgInteract( community, substrate.name, species, "init" )
- organism[,-1] = future.R/get.future( community, species, organism[,-1] )
- community = community.R/put.species( community, species, leftist.create( organism )) # create leftist tree
- community = init.R/initOffspring( community, species ) # mean number of offspring

##### Other routines in init.R

initOffspring( community, species )

- hostname = init.R/getOrgFeature( community, species, "offspring" )
- orgoffspring = init.R/getOffspring( community, species, hostname ) # find if there is offspring load based on host
- norganism = sum( init.R/getOrgAlive( community, species ))
- host = community.R/get.species( community, hostname )
- init.R/getOrgFuture( community, hostname )
- organism = community.R/get.species( community, species )
- community.R/put.species( community, species, organism )

getOffspring( community, species, offspring )

- offspring = init.R/getOrgFeature( community, species, "offspring" )
- init.R/getOrgInteract( community, offspring, species, "offspring" )
  
getOrgFeature

- OrgFeature = community.R/getOrgInfo( community, "Feature" )
- OrgFeature[ species, feature ]

getOrgHosts

- init.R/getOrgFeature()

getOrgInteract

- init.R/getOrgFeature()
- community.R/getOrgInfo()

getOrgFuture( community, species, feature, current, future )

- OrgFuture = community.R/getOrgInfo( community, "Future" )
- future = OrgFuture[[species]]

getOrgAlive( community, species, element )

- community.R/get.species( community, species )

getOrgAgeClass( community, species, stage, future)
  
- future = init.R/getOrgFuture( community, species )

getOrgMeanValue( community, species )

- OrgMeanValue = community.R/getOrgInfo( community, "MeanValue" )

get.offspring( community, species )

- individual = community.R/get.individual( community, species )

#### community.R

initOrgInfo()

- global datafile organism.features

setOrgInfo()

- global datafiles future.host future.parasite
- community.R/getOrgInfo( community, "package" )
- init.R/getOrgFeature( community, species, "substrate" )
- set up for species, hosts (subset of species), substrates

getOrgInfo()

- community$org[[element]]

initCount() # simulation count object administration

- community.R/get.species( community, species[i] )
- init.R/getOrgFeature( community, species[i], "units" )
- community.R/get.individual( community, species[i] )["time"]
- community = temp.R/activeTemp( community, simmin["hr"], , simmin["DD"] )
- init.R/getOrgFeature( community, species, "subclass" )
- community.R/get.individual( community, species[i] )["time"]
- count$mintime[i] = temp.R/getTime( community, species[i], species.time )
- init.R/getOrgFuture( community, species[i] )
- init.R/getOrgFuture( community, species[i], "ageclass" )
- init.R/getOrgInteract( community,, species[i], "substrate" )
- stage = init.R/getOrgAlive( community, species[i], "stage" )
- classes = init.R/getOrgAgeClass( community, species[i], stage )
- substage = init.R/getOrgAlive( community, species[i], "sub.stage" )
- classes = init.R/getOrgInteract( community,, species[i], "substrate" )
- community.R/get.species.element( community, species[i], c("time","stage"), count$base[species[i]] )

initTemp( community, lo.hour, hi.hour ) # creates Temperature object

- global data file TemperaturePar
- global data file TemperatureBase
- temp.R/showTemp( community )
- temp.R/activeTemp( community, lo.hour, hi.hour, community.R/getTemp( community, "Time", 1 )[1] )

getTemp( community, element, sub )

getCount( community, species, "mintime")

setCount( community, species, elements )

get.species( community, species )

put.species( community, species, value )

get.individual( community, species, id )

- id = community.R/get.base( community, species )

put.individual( community, species, individual, id )

- id = community.R/get.base( community, species )

get.base( community, species )

setTemp( community, element, value )

init.timing( community )

- community.R/get.species( community )
- init.R/getOrgFuture( community, species, "event" )
- community.R/set.timing( community, "total" )

set.timing( community, string, flag )
  
#### temp.R

activeTemp( community, lo.hour, hi.hour )	# updates Temperature object

- community.R/getTemp()
- community.R/getDegreeDay( community, lo.hour[1] )
- community = community.R/setTemp( community, "DegreeDay", tmpspline )
  + tmpspline = community.R/temp.spline( community, temp.repeat( community, period ), start = degreeday )
-  community = community.R/setTemp( community, "Hour", brksplne )
  + brkspline = temp.R/break.backSpline( community.R/getTemp( community, "DegreeDay" ))

showTemp( community )

- community.R/getTemp()

getDegreeDay( community, hour )

- community.R/getTemp( community, "DegreeDay" )

temp.spline( community, hour, temp, start = 0, mintemp, cumulative = TRUE, mult )

- mintemp = community.R/getTemp( community, "Min" )
- mult = community.R/getTemp( community, "Unit" )

break.backSpline( tmp )

checkTime( ) # check if activeTemp needs updating to cover time interval

- community.R/getTemp()
- temp.R/activeTemp()

getTime( community, species, x )

- init.R/getOrgFeature( community, species, "units" )
- temp.R/getDegreeDay( community, temp )
- community.R/getTemp( community, "Unit" )

#### future.R

##### future.events

Realize future events in quantitative population ethology simulation
 
This is the main routine for Ewing's Quantitative Population Ethology. It
steps through future events for individuals starting with the next minimum
future event time.
Steps through future events for community with one or more species. Keeps
track of counts by age classes and substrates in external file.

The routine `future.events` builds the name of the event processor
function based on event type, which is established in the `future.host`
or `future.parasite` data structure. The `event.attack` uses
further information from `organism.features` about the type of parasite
(`endo` or `ecto`) in conjunction with the current event
(`feed` or `ovip`) to determine the nature of attack.
 
A plot is created periodically unless `plotit=FALSE`.  An external
`file` is written with simulation counts for re-plotting.

future.events( community ) 

- species = community.R/get.species( community )
- community = community.R/initCount( community, species, file, append )
- community = community.R/init.timing( community )
- mintime = community.R/getCount( community, , "mintime" )
- future = init.R/getOrgFuture( community, species.now )
- mintime = community.R/getCount( community, , "mintime" )
- individual = community.R/get.individual( community, species.now )
- community.R/getCount( community, species.now, "base" )
- future = init.R/getOrgFuture( community, species.now )
- community = sim.R/updateCount( community, species.now, individual, stage == "death", istep )
- community = community.R/put.individual( community, species.now, individual )
- community = future.R/event.death( community, species.now )
- community = event.R/event.parsed( community, species.now )
- community = event.future( community, species.now )
- community = set.timing( community, event.type, 1 )
- init.R/getOrgAlive( community, species.now )
- community = setEvents( community, "final" )
- community = fini.timing( community )

##### event.future

Process immediate, pending and future events in quantitative population
ethology simulation.
 
Process an event for next individual in a species. Events may be monadic
(one individual) or dyadic (involving two individuals).
These are the main event processors for Ewing's Quantitative Population
Ethology. They all take the same arguments, but handle events in quite
different ways.

- `event.future` is the generic routine for monadic
future events, such as progressing through life stages.
- `event.death`
handles all pending events of death of individuals.
- `event.birth` handles immediate events of new births. It assumes at
present one birth at a time, leading ultimately to a starved state.
The `future.host` data has the gravid adult returning to the fertile state
until it depletes its resources. The pending event "starved" then triggers
the pending event "death".
- `event.attack` is an example of a dyadic event processor, in this case
host-parasite interaction. The parasite locates a host based on life stages
and relative position in the simulation space. Ectoparasites kill their
hosts, hence realizing a pending death event (to be processed immediately by
`event.death` in the next step). Endoparasites merely incapacitate
their hosts.
 
At present, the search strategies of parasites and health consequences of
hosts are crude. However, search is over substrates and depends on the stage
of hosts. A crude sex determination strategy is in place, with smaller
(younger) hosts more likely to get male parasite eggs.
The external file `community.out` is written with simulation counts
following each event that changes the number or stage of individuals.

User-supplied `event.blah` functions can be specified in user-supplied
libraries. Note that user code needs to comply with the arguments and
values, and needs to process future events for individuals as they progress
through life. Further, user routines need to properly update counts to the
external file. This is at present an advanced topic, but can be figured out
by examining the above routines.

event.future(community, species) # process immediate, pending and future events

- individual = future.R/get.future( community, species ) # schedule future event based on current stage
- individual = move.R/event.move( community, species, individual ) # move if appropriate
- community = temp.R/checkTime( community, individual["time"], count, feature) # update time translation if needed
  + count = community.R/getCount( community, species, "mintime")
  + feature = init.R/getOrgFeature( community, species, "units" )
- community = community.R/put.individual( community, species, individual ) # put updated individual back in community
- community.R/get.species.element( community, species, "time", individual[c("left","right")] ))
- community = community.R/put.species( community, species, leftup )
  + leftup = leftist.R/leftist.update( community.R/get.species( community, species ))
- community = future.R/update.mintime( community, species )

event.death(community, species, id)

- id = community.R/get.base( community, species )
- community = community.R/put.species( community, species, leftrm) # remove dead individual from leftist tree
  + leftrm = leftist.remove( community.R/get.species( community, species ), id )
- community = put.base( community, species, id ) # free up individual for reuse

##### Other future.R routines

get.future # Get birth and future event

- id = community.R/get.base(community, species)
- individuals = community.R/get.individual(community, species, id)
- init.R/getOrgFuture(community, species, c("current", "fid", "time"))
- getOrgMeanValue(community, species)

set.birth( community, species, neworg )

- community = temp.R/checkTime( community, neworg["time",], count, feature )
  + count = community.R/getCount( community, species, "mintime" )
  + feature init.R/getOrgFeature( community, species, "units" )
- oldbase = community.R/getCount( community, species, "base" )
- leftbirth = leftist.birth( community.R/get.species( community, species ), neworg, fcount)
  + fcount = community.R/getCount( community, species, "free" )
- community = community.R/put.species( community, species, leftbirth$tree )
- community = put.base( community, species, free = leftbirth$free )
- community = sim.R/updateCounts( community, species, newbirths )

update.mintime( object, species, ... )

- base = community.R/get.base( object, species )
- mintime = max( count, time)
  + count = community.R/getCount( object, species, "mintime" )
  + time = temp.R/getTime( object, species, get.species.element( object, species, "time", base ))
- community.R/setCount( object, species, list( base = base, mintime = mintime ))

#### event.R

event.birth(community, species)

- offspring = init.R/get.offspring( community, species)
- newbirths = event.R/get.birth( community, species, offspring ) # get new births
- community = future.R/set.birth( community, species, newbirths ) # merge births into community

event.attack(community, species) # Interaction Events (only attack for now)

- community = event.R/get.deplete( community, species ) # deplete individual based on time spent searching for host
- individual = community.R/get.individual( community, species ) # get individual record of attacker
- host = init.R/getOrgFeature( community, species, "attack" ) # get name of host for attacker
- attack = event.R/get.attack( community, species, individual ) # get attack parasite and event types
- found = move.R/event.find( community, species, host, attack["event"] ) # find host located on the same substrate
- community = event.R/event.parsed( community, species, host, found )
- individual = move.R/event.move( community, species, individual ) # parasite moves along substrate
- individual["stage"] = event.R/set.future( community, species, "starved" ) # parasite dies if it does not feed enough
- community.R/put.individual( community, species, individual ) # put updated individual back in community

##### Other event.R routines

set.future( community, species, stage )

- current = init.R/getOrgFuture( community, species, "current" )

get.birth( community, species, offspring )

- individual = community.R/get.individual( community, species ) # get individual record
- newbirths = move.R/event.move( community, species, newbirths )
- init.R/getOrgFeature( community, species, "units" )
- future.R/get.future( community, species, newbirths)

get.deplete( community, species )

- individual = community.R/get.individual( community, species ) # get individual record of attacker
- init.R/getOrgFeature( community, species, "deplete" )
- community.R/put.individual( community, species, individual )

get.attack( community, species, individual )

- parasite = init.R/getOrgFeature( community, species, "parasite" ) # get parasite type ("ecto" or "endo") and current event ("feed" or "ovip")
- event = init.R/getOrgFuture( community, species, "current", individual["future"] )

event.parsed( community, species, host, found ) # created by get() of function

This function is dynamically created in multiple places

- event.parsed = get( paste( "event", attack["event"], sep="." ))
- event.parsed = get( paste( "host", attack["parasite"], sep="." ))

#### move.R

event.move( community, species, individual )

- move.R/is.move( community, species, individual )
- individual["sub.future",] = init.R/sampleOrgSubstrate( community, species, individual["sub.stage",] )
- individual[position,] = triangle.R/rtri( ncol( individual ), 10, individual[position,] )

is.move( community, species, individual )

- init.R/getOrgFeature( community, species, "move" )
- init.R/getOrgFuture( community, species, "current" )

event.find( community, species, host, event )

- individual = community.R/get.individual( community, species )
- avail = init.R/get.alive( community, host, individual["sub.stage"] ) # pending event: need to find available hosts on substrate
- interact = init.R/get.interact( community, species, host, avail, event ) # preferences based on schedule

#### sim.R

updateCount( community, species, individual, is.death = FALSE, step )

- community = community.R/updateEvents( community, species, individual["future"] )
- community = community.R/setCount( community,, list( step = step ))
- countage = community.R/getCount( community, species, "countage" ) # move individual through age classes or drop if it dies
- ageclass = init.R/getOrgAgeClass( community, species, individual["stage"] )
- ageclass = init.R/getOrgAgeClass( community, species, individual["future"] )
- countsub = community.R/getCount( community, species, "countsub" ) # move individual across substrate elements or drop if it dies
- substrate = init.R/getOrgFeature( community, species, "substrate" )
- elements = init.R/getOrgInteract( community, substrate, species, "substrate" )
- subclass = init.R/getOrgFeature( community, species, "subclass" )
- init.R/getOrgAgeClass( community, species, individual[c("stage","future")] )
- sim.R/writeCount( community, species, individual["time"], individual["future"], countage, countsub )

#### triangle.R

rtri()
