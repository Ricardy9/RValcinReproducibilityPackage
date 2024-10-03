--- 
 Authors: 
   Ricardy
--- 

# Academic Data Use

 Reproducibility package for "Forced Displacement in Mexico"

## Overview  

The code in this replication packages constructs the analysis files and tables and figures for this analysis using R. 

## Directory Structure

1. 01_raw_data contains only one database 


## Instructions to Replicators

* Clone the repository to your local machine.

* Before executing the Data_Use_Academia_tables_figures.Rmd
file, users should set up the appropriate environment. The
renv package helps maintain consistent package versions and
dependencies, ensuring that users have the required libraries.

* Users should first ensure the renv package is installed. If it’s
not already present, it can be installed using install.packages("renv").

* Once installed, users should set up the environment by running
the following commands:

`renv::activate()`

`renv::restore()`

* With the environment now properly set up, users can proceed, please run 02_programs/Data_Use_Academia_tables_figures.Rmd to generate the data and figures.  This file will run all of the code to generate the data and figures.  The replicator should expect the code to run for around 20-30 minutes.

* There should be no need to change the working directory.  The code should run as is, because the code is using the [here](https://here.r-lib.org/) package in R, which automatically handles file paths on local machines.  Make sure the .here file is included when you clone the repository.

### License
 

### Summary of Availability


### Data Sources


### Data Description



### Software

R version 4.3.1 
