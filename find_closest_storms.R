# Find the nearest TITAN tracks to each insurance claim

# Justin Peter, ICACS, USQ

# Started 13 Aug 2015

library(ggplot2)



# ----- Read in the data sets

    # The insurance data
    insdir    <- '/home/jpeter/justinp/rfiles/suncorp/data/insurance1/'
    insfile   <- 'Claim_home.csv'
    ins_data <- read.csv(paste0(insdir,insfile),header=TRUE)

    # Add the date in POSIXlt format
    ins_data$ins.date <- as.Date(ins_data$Loss.Date,"%d-%b-%y")
    #length(ins_data$Incurred.To.Date) = 4507

    # Remove NAs
    ins_data <- ins_data[!is.na(ins_data$Latitude) & 
                         !is.na(ins_data$Longitude),]
    #length(ins_data$Incurred.To.Date) = 2375

    # Only get insurance losses greater than 10 dollars
    # Some are small numbers
    ins_data <- ins_data[ins_data$Incurred.To.Date > 10,]

    #Plot some descriptive statistics
    hist(ins_data$ins.date,breaks="days")


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


    # The TITAN data sets
    source("/home/jpeter/justinp/rfiles/suncorp/climatology/read_titan_data.R")



# ----- Merge the data sets to get only dates which correspond
    #mdata <- merge(data,ins_data,by.x=data$date.r,by.y=ins_data$ins.date)
    #mdata <- merge(data,ins_data,by.x="date.r",by.y="ins.date")

    exd1 <- data$date.r %in% ins_data$ins.date
    exd2 <- ins_data$ins.date %in% data$date.r

    mdata1 <- data[exd1,]
    mdata2 <- ins_data[exd2,]

    # Now merge them together
    #mdata <- merge(mdata1,mdata2,by.x="date.r",by.y="ins.date")

    # Find the unique dates in the insurance data
    uid <- unique(mdata2$ins.date)
    # Loop along each of these unique dates
    exd <- vector("list",length=length(uid))
    for (i in seq_along(uid) ){
        exd[[i]] <- which(mdata$ins.date == uid[i])
    }
    # Convert the list to a vector.
    exdinds <- unlist(exd)


    







