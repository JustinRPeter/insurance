# Find the nearest TITAN tracks to each insurance claim

# Justin Peter, ICACS, USQ

# Started 13 Aug 2015



# ----- Read in the data sets

    # The insurance data
    insdir    <- '/home/jpeter/justinp/rfiles/suncorp/data/insurance/'
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
    #length(ins_data$Incurred.To.Date) = 2373

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


    







