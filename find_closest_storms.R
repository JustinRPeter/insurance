# Find the nearest TITAN tracks to each insurance claim
# Justin Peter, ICACS, USQ
# Started 13 Aug 2015

rm(list=ls())
library(ggplot2)
library(MASS)
library(gmt) # For distance calculations

# ----- Read in the data sets

    # The insurance data
    #insdir    <- '/home/jpeter/justinp/rfiles/suncorp/data/insurance1/'
    #insfile   <- 'Claim_home.csv'
    #ins_data <- read.csv(paste0(insdir,insfile),header=TRUE)
    insdir    <- '/home/jpeter/justinp/rfiles/suncorp/data/insurance2/clean/'
    #insfile   <- 'HOME_all_claims_complete.rds'
    insfile   <- 'HOME_all_claims_complete.csv'
    ins_data <- read.csv(paste0(insdir,insfile),na.strings=c("NA",""))

    # Remove entries where State is missing as we can't guaranteee accuracy of
    # geocoding, and perform some additional clean up steps.

    ins_data <- ins_data[!is.na(ins_data$Postcode),]
    ins_data <- ins_data[!is.na(ins_data$Address),]
    ins_data <- ins_data[ins_data$Longitude>150. & ins_data$Longitude<155.,]
    ins_data <- ins_data[ins_data$Latitude> -30.0 & ins_data$Latitude< -26.,] 
    ins_data <- ins_data[ins_data$Incurred.To.Date > 10,]

    # Add the date in POSIXlt format
    # ins_data$ins.date <- as.Date(ins_data$Loss.Date,"%d-%b-%y")
    #length(ins_data$Incurred.To.Date) = 4507
    ins_data$ins.date <- as.Date(ins_data$Date.Occurred,"%d/%m/%Y")
    #length(ins_data$Incurred.To.Date) = 3877

    # Remove NAs
    #ins_data <- ins_data[!is.na(ins_data$Latitude) & 
    #                     !is.na(ins_data$Longitude),]
    #length(ins_data$Incurred.To.Date) = 2375

    # Only get insurance losses greater than 10 dollars
    # Some are small numbers
    #ins_data <- ins_data[ins_data$Incurred.To.Date > 10,]

    #Plot some descriptive statistics
    #hist(ins_data$ins.date,breaks="days")
    postscript("insurance_data_frequency_time_series.eps",onefile=FALSE,
               width=10,height=10,horizontal=FALSE,paper="special")
    hist(ins_data$ins.date,breaks="months",freq=T,
         xlim=c(14750.,16000.),
         xlab="Date",ylab="Frequency",main="")


    #length(ins_data$Incurred.To.Date) = 2373
    #length(unique(ins_data$ins.date) = 32)


    #Git a lognormal distribution to the data
    fitlogn <- fitdistr(ins_data$Incurred.To.Date,"lognormal")
    #Calculate the mean and std. dev. of the original data
    #See Wilks pg. 92 2nd edition
    fit.mn <- exp(fitlogn$estimate[1]+(fitlogn$estimate[2]^2)/2)
    fit.sd <- (exp(fitlogn$estimate[2]^2)-1) * 
              exp(2*fitlogn$estimate[1]+fitlogn$estimate[2]^2)
    #fitlogn <- fitdistr(log(ins_data$Incurred_To_Date),"normal") # Same as above

    postscript("insurance_data_histogram.eps",onefile=FALSE,
               width=10,height=10,horizontal=FALSE,paper="special")
    #hist(log(ins_data$Incurred.To.Date))
    #Plotting the log of loss
    #p <- ggplot(ins_data,aes(x=log(Incurred.To.Date)))
    #p <- p + geom_histogram(aes(y=..density..),color="black",fill="white")
    #p <- p + stat_function(geom="line",fun=dnorm,
    #                       args=list(mean=fitlogn$estimate[1],
    #                                 sd=fitlogn$estimate[2]))
    #

    p <- ggplot(ins_data,aes(x=Incurred.To.Date))
    p <- p + geom_histogram(aes(y=..density..),color="black",fill="white")
    p <- p + scale_x_continuous(trans="log",
                                breaks=c(10,100,1000,10000,100000,1000000))
    p <- p + stat_function(geom="line",fun=dnorm,
                           args=list(mean=fitlogn$estimate[1],
                                     sd=fitlogn$estimate[2]),
                                     size=2)
    p <- p + xlab("Incurred Loss (AUD)") + theme_grey(base_size=20)

    plot(p)

    dev.off()



# Read the TITAN data sets
source("/home/jpeter/justinp/rfiles/suncorp/climatology/read_titan_data.R") 

# Now find the unique dates of the insurance data set
uq.ins.dates <- unique(ins_data$ins.date)
titan_data <- data[which(data$date.r %in% uq.ins.dates),]

ins_data_sub <- ins_data[ins_data$ins.date >= min(titan_data$date.r),]
uq.tit.dates <- unique(titan_data$date.r)
ins_data_sub <- ins_data[which(ins_data$ins.date %in% uq.tit.dates),]
uq.ins_data_sub.dates <- unique(ins_data_sub$ins.date)

# Now find all the TITAN tracks within a specified distance
# Will have to try a vector of distances later, but just one for now.

#test <- rdist.earth(matrix(c(ins_data_sub$Longitude,ins_data_sub$Latitude), ncol=2),
#                    matrix(c(titan_data$long,titan_data$lat),
#                    ncol=2),miles=FALSE)

# For each ins_data_sub entry extract the dates which correspond in
# titan_data. Then compute the distances between the ins_data_sub entry and
# each of the titan_data entries. Then extract those which are within the
# specified distance.

spec_dist=2. #kilometres
#spec_dist=10. #kilometres

extdf <- NULL

for (i in seq_along(uq.ins_data_sub.dates)){
#for (i in 1){
    # Extract only those dates from the TITAN data set
    ext_titan <- titan_data[titan_data$date.r == uq.ins_data_sub.dates[i],]
    ext_ins   <- ins_data_sub[ins_data_sub$ins.date == 
                              uq.ins_data_sub.dates[i],]

    #for (j in 1:length(ext_ins$ins.date)){
    for (j in seq_along(ext_ins$ins.date)){
    #for (j in 1){
        dists <- rdist.earth(matrix(c(ext_ins$Longitude[j],ext_ins$Latitude[j]),ncol=2),
                             matrix(c(ext_titan$long,ext_titan$lat),ncol=2),
                             miles=FALSE)
        ext_dists <- which(dists <= spec_dist)
        ext_titan_rows  <- ext_titan[ext_dists,]
        ext_ins_rows  <- ext_ins[j,]
        if (nrow(ext_titan_rows) > 0){
            ext_data <- cbind(ext_titan_rows,ext_ins_rows)
        }
        extdf <- rbind(extdf,ext_data)
    }
}

write.table(extdf,"titan_insurance_merged.csv",sep=",",row.names=FALSE)
saveRDS(extdf,"titan_insurance_merged.rds")



#    # The TITAN data sets
#    source("/home/jpeter/justinp/rfiles/suncorp/climatology/read_titan_data.R")
#
#
#
## ----- Merge the data sets to get only dates which correspond
#    #mdata <- merge(data,ins_data,by.x=data$date.r,by.y=ins_data$ins.date)
#    #mdata <- merge(data,ins_data,by.x="date.r",by.y="ins.date")
#
#    exd1 <- data$date.r %in% ins_data$ins.date
#    exd2 <- ins_data$ins.date %in% data$date.r
#
#    mdata1 <- data[exd1,]
#    mdata2 <- ins_data[exd2,]
#
#    # Now merge them together
#    #mdata <- merge(mdata1,mdata2,by.x="date.r",by.y="ins.date")
#
#    # Find the unique dates in the insurance data
#    uid <- unique(mdata2$ins.date)
#    # Loop along each of these unique dates
#    exd <- vector("list",length=length(uid))
#    for (i in seq_along(uid) ){
#        exd[[i]] <- which(mdata$ins.date == uid[i])
#    }
#    # Convert the list to a vector.
#    exdinds <- unlist(exd)


    







