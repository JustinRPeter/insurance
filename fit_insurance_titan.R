# This script tries to fit a multiple linear regresion to the insurance costs
# incurred as a function of various TITAN quantities.

# Justin R. Peter, ICACS, USQ, 3 Nov 2015.

rm(list=ls())

data <- readRDS("titan_insurance_merged.rds")


