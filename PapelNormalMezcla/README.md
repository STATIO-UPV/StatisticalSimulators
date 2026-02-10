# STATIO: Teaching Simulators for Statistics and Operations Research
This simulator is part of [STATIO](https://statio.webs.upv.es), the Educational Innovation and Improvement Project (PIME/25-26/562) developed by the DEIOAC–[Universitat Politècnica de València](http://www.upv.es).

# Description
This app is used to explore how a normal probability plot behaves when data come from a mixture of several normal populations instead of a single one.

It helps you visually detect heterogeneity, subgroups, curvature patterns, and outliers by dynamically generating and combining up to three normal samples with adjustable parameters (mean, standard deviation, and size).

By modifying these parameters in real time, you can better understand how mixed populations affect the shape of the normal plot and learn to interpret complex data distributions.

# Authors
Vicente Chirivella González

# App
https://statio.webs.upv.es/StatisticalSimulators/PapelNormalMezcla/

# GitHub
https://github.com/STATIO-UPV/StatisticalSimulators/tree/main/PapelNormalMezcla

# Technical Note
This simulator has been developed using [ShinyLive](https://posit-dev.github.io/r-shinylive/). It allows Shiny to run entirely in the browser, without any need for a hosted server. The `appr` folder includes the Shiny R code. The `appsite` folder includes the ShinyLive build, including the `.html` file.
