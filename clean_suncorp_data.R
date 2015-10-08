# Pre-processs the Suncorp data
# Justin Peter, ICACS, 22 Sep 2015

rm(list=ls())

#load the ggmap library
library(ggmap)

# Set the input directory
indir <- '/home/jpeter/justinp/rfiles/suncorp/data/insurance2'

# Find all the HOME insurance claims
infiles <- list.files(indir,pattern='*HOME*',full.names=TRUE)

# Store each file in a list


#data_sep <- lapply(infiles,read.csv)
#saveRDS(data_sep,"All_homedata_sep.RDS")
#readRDS("All_home_data_sep.RDS")

# Check if the data is already loaded.
# If not check if the rds file exists and read
# If not, then read the original csv files. This takes a while!
# Each file is stored as a member in a list.
if (exists("ins_data") != TRUE){
    rdsfilename <- "All_homedata_sep.rds"
    if (file.exists(rdsfilename)){
           print("Found R data file: reading data from that: ")
           ins_data <- readRDS(rdsfilename)
           } else {
               ins_data <- lapply(infiles,read.csv)
               saveRDS(ins_data,"All_homedata_sep.RDS")
           }
} else {print("Have to read original csv files. This will take a while!")}

# Extract only the entries which contain a claim amount
# and have a missing Latitude or Longitude
# or if the Lat/Lon is zero
# ext contains the subscripts and sub_ins_data is the data.
# We will require the subscripts to place into the data later.
sub_ins_data <- vector("list",length=length(ins_data))
ext <- vector("list",length=length(ins_data))
for (i in seq_along(ins_data)){
    #ext[[i]] <- which(ins_data[[i]]$Incurred.To.Date > 10)
    ext[[i]] <- which( (ins_data[[i]]$Incurred.To.Date > 10) &
                       (is.na(ins_data[[i]]$Latitude)  | 
                        is.na(ins_data[[i]]$Longitude) |
                        ins_data[[i]]$Latitude == 0 |
                        ins_data[[i]]$Longitude == 0) )

    sub_ins_data[[i]] <- ins_data[[i]][ext[[i]],]
}

# Get the address and append "Australia" to the end to increase
# accuracy.
# We are only using the addresses that have claims data.
# See the criteria above.
addresses <- vector("list", length=length(ins_data))
for (i in seq_along(ins_data)){
    addresses[[i]] <- paste0(sub_ins_data[[i]]$Address, ", ",
                             sub_ins_data[[i]]$Postcode, ", ",
                             sub_ins_data[[i]]$State.Of.Risk, ", Australia")
}


# Define a function that will process Google server responses

getGeoDetails <- function(address){   
   #use the gecode function to query google servers
   geo_reply = geocode(address, output='all', messaging=TRUE,
                       override_limit=TRUE)
   #now extract the bits that we need from the returned list
   answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA,
                        address_type=NA, status=NA)
   answer$status <- geo_reply$status
 
   #if we are over the query limit - want to pause for an hour
   while(geo_reply$status == "OVER_QUERY_LIMIT"){
       print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
       time <- Sys.time()
       print(as.character(time))
       #Sys.sleep(60*60)
       #Sys.sleep(1)
       geo_reply = geocode(address, output='all', messaging=TRUE,
                           override_limit=TRUE)
       answer$status <- geo_reply$status
   }
 
   #return Na's if we didn't get a match:
   if (geo_reply$status != "OK"){
       return(answer)
   }   
   #else, extract what we need from the Google server reply into a dataframe:
   answer$lat <- geo_reply$results[[1]]$geometry$location$lat
   answer$long <- geo_reply$results[[1]]$geometry$location$lng   
   if (length(geo_reply$results[[1]]$types) > 0){
       answer$accuracy <- geo_reply$results[[1]]$types[[1]]
   }
   answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
   answer$formatted_address <- geo_reply$results[[1]]$formatted_address
 
   return(answer)
}
 
#initialise a dataframe to hold the results
#geocoded <- data.frame()
geocoded <- vector("list",length=length(ins_data))
# find out where to start in the address list (if the script was interrupted
# before):
startfile  <- 1
startindex <- 1
#if a temp file exists - load it up and count the rows!
#tempfilename <- paste0(infile, '_temp_geocoded.rds')
tempfilename <- vector("character",length=length(ins_data))
#for (i in seq_along(ins_data)){
#    tempfilename[i] <- paste0(infiles[i], '_temp_geocoded.rds')
#        if (file.exists(tempfilename[i])){
#               print("Found temp file - resuming from index:")
#               geocoded[[i]] <- readRDS(tempfilename[i])
#               startindex <- nrow(geocoded[[i]])
#               print(startindex)
#        }
#}

# Construct a temporary file name for each R data file.

# if a temp file exists - load it up and count the rows.
# (1) construct a vector of the temp file names
# (2) Find the one which was modified most recently
# (3) Also need to add conditions in case no files have 
#     been processed.
tempfilename <- paste0(infiles,"_temp_geocoded.rds")
# Find the file which was modified most recently 
tempfileinfo   <- file.info(tempfilename)
lastfilemodind <- which.max(tempfileinfo$mtime)
#tempfileorder  <- sort(tempfileinfo$mtime,decreasing=TRUE)
#lastfilemod    <- tempfileorder[1]
lastfilemod <- tempfilename[lastfilemodind]

        #if (file.exists(tempfilename[i])){
#if ( (length(lastfilemod) > 0) & (file.exists(lastfilemod)) ){
if (length(lastfilemod) > 0){ # check at least one file already processed
    if (file.exists(lastfilemod)){
    print("Found temp file - resuming from index:")
    #geocoded[[i]] <- readRDS(lastfilemod)
    geocoded[[lastfilemodind]] <- readRDS(lastfilemod)
    startindex <- nrow(geocoded[[lastfilemodind]])
    startfile  <- infiles[lastfilemodind]
    print(startindex)
    print(startfile)
    }
}

#}


# Start the geocoding process - address by address. geocode() function takes
# care of query speed limit.
for (filindx in seq_along(ins_data)){

    #Only work on files which have more than one line
    if (length(addresses[[filindx]]) > 1){

        print(paste0("Working on file: ", filindx, basename(infiles[filindx])))

        startindex   <- 1
        tempfilename <- paste0(infiles[filindx], '_temp_geocoded.rds')
        if (file.exists(tempfilename)){
            print("Found temp file - resuming from index:")
            #geocoded[[filindx]] <- readRDS(tempfilename)
            geocoded <- readRDS(tempfilename)
            #startindex <- nrow(geocoded[[filindx]])
            startindex <- nrow(geocoded)
            print(startindex)
        }

        for (ii in seq(startindex,length(addresses[[filindx]]))){
            print(paste("Working on index", ii, "of", length(addresses[[filindx]])))
            result = getGeoDetails(addresses[[filindx]])
            print(result$status)
            result$index <- ii
            #geocoded[[filindx]] <- rbind(geocoded[[filindx]], result)
            geocoded <- rbind(geocoded, result)
            saveRDS(geocoded,tempfilename)
        }
    }
}






    # If no files have been read, start with the first file
    #if (ii == 1){
    # Check to see if a tempdata file exists
    # If it does read it
    #if (file.exists(tempfilename[ii])){
        geocoded[[ii]] <- readRDS(tempfilename[ii])
        # Check the number of rows
        startindex <- nrows(geocoded[[ii]])
        # If it doesn't equal the no. of rows in the file
        # continue from where we left off.
        if (startindex != length(addresses[[ii]])) {
            for (jj in seq(startindex, length(addresses[[ii]]))){
                print(paste("Working on index", jj, "of", length(addresses[[ii]])))
                print(paste("Processing file", basename(infiles[ii])))
                #query the google geocoder - this will pause here if we are over the limit.
                result = getGeoDetails(addresses[[ii]][jj]) 
                print(result$status)     
                result$index <- jj
                #append the answer to the results file.
                geocoded[[ii]] <- rbind(geocoded[[ii]], result)
                #save temporary results as we are going along
                saveRDS(geocoded[[ii]], tempfilename[ii])
            }
        }
    #}
}

            

#for (ii in lastfilemodind:length(ins_data)){
    # Only process a file if there is something in it. 
    if (length(addresses[[ii]]) > 1){
        for (jj in seq(startindex, length(addresses[[ii]]))){
           print(paste("Working on index", jj, "of", length(addresses[[ii]])))
           print(paste("Processing file", basename(infiles[ii])))
           #query the google geocoder - this will pause here if we are over the limit.
           result = getGeoDetails(addresses[[ii]][jj]) 
           print(result$status)     
           result$index <- jj
           #append the answer to the results file.
           geocoded[[ii]] <- rbind(geocoded[[ii]], result)
           #save temporary results as we are going along
           saveRDS(geocoded[[ii]], tempfilename[ii])
        } 
    }
}
 


# We now have the miss
#missing_loc      <- vector("list",length(sub_ins_data))
#missing_loc_inds <- vector("list",length(sub_ins_data))
#for (i in seq_along(sub_ins_data)){
#    missing_loc_inds[[i]] <- which(is.na(sub_ins_data[[i]]$Latitude) | 
#                                   is.na(sub_ins_data[[i]]$Longitude))
#    missing_loc[[i]] <- sub_ins_data[[i]][missing_loc_inds[[i]],]
#}

# We now have the indices of the missing 





# Extract only the entries which contain a claim amount
# and which 
#for (files in infiles){
#    read.csv(infiles

