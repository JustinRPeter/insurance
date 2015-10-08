# Read the claims insurance data provided by Suncorp
# Justin Peter, ICACS
# 5 August 2015

#rm(list=ls())

read_suncorp_data <- function(data_dir){
    #ins_data_dir <- "/home/jpeter/justinp/rfiles/suncorp/data/insurance/"

    #ins_data <- read.csv(paste0(data_dir,"Claim_home.csv"),header=TRUE)
    ins_data <- read.csv(data_dir,header=TRUE)
    #Some stupidly small values. Get rid of them.
    ins_data <- ins_data[ins_data$Incurred.To.Date >10,]
    return(ins_data)
}


