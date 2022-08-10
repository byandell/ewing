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

Alternatively, you can visit <http://www.stat.wisc.edu/~yandell/ewing>

Reference: B Ewing, BS Yandell, JF Barbieri, and RF Luck (2002) "Event-driven competing risks," _Ecological Modelling 158_: 35--50.

### Code Issues

- gencurve in spline.R internal to five.plot but used elsewise
- dead used in init, event
- organsim.features in community.R using mydata() nonstandard
- TemperatureBase in init.R initTemp
- to.plot in triangle.R plot.current not defined
- hour, knotrange undefined in temp.R
- lo.hour, hi.hour etc in temp.plot?
- File ‘ewing/R/init.R’: assign(toto, get(from), ".GlobalEnv")
- somehow crawler count ends up being negative, which throws off plot (future.events?)



