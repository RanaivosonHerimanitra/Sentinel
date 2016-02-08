# Sentinel
A toolkit for early detection of disease outbreaks (Malaria, Diarrhea, etc.)

## Goals:
* Build a package that could receive any kind of algorithms for epidemiologists to play with.
* Automate data processing and conversion for any kind of format and backend.

## Packages requirements:

* Have a look at `libraries.R`

## Algorithms used to trigger alert:

<img src="app_snap.png" >

`Percentile` algorithm is used to trigger alert in sentinel network. An alert is triggered when during `n` (consecutive or not) week(s) , diseases occurrence exceeds `90th percentile` calculated using the entire historical time series. This calculation of `90th percentile` excludes the current week.


