# Script to obtain lat/lon pairs from the Suncorp
# insurance data
# Justin Peter, ICACS
# 12 August 2015

 
#load the ggmap library
library(ggmap)

# Load the Suncorp data

#indir    <- '/home/jpeter/justinp/rfiles/suncorp/data/insurance/'
#infile   <- 'Claim_home.csv'
indir    <- '/home/jpeter/justinp/rfiles/suncorp/data/insurance2/'
infile   <- 'HOME_200809.csv'
data <- read.csv(paste0(indir,infile),header=TRUE)

ext <- which( (data$Incurred.To.Date > 10) &
              (is.na(data$Latitude)  | 
               is.na(data$Longitude) |
                data$Latitude == 0 |
                data$Longitude == 0) )

    #sub_ins_data[[i]] <- ins_data[[i]][ext[[i]],]
data <- data[ext,]

# addresses <- paste0(data$Address, ", ",data$Postcode, ", ",
#                         data$State.Of.Risk, ", Australia")
# Get the adresses and append "Australia" to the end to increase
# accuracy
addresses <- paste0(data$Address,", ",data$Postcode)
addresses <- paste0(addresses, ', Queensland, Australia')

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
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted
# before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
       print("Found temp file - resuming from index:")
       geocoded <- readRDS(tempfilename)
       startindex <- nrow(geocoded)
       print(startindex)
}
 
# Start the geocoding process - address by address. geocode() function takes
# care of query speed limit.
for (ii in seq(startindex, length(addresses))){
   print(paste("Working on index", ii, "of", length(addresses)))
   #query the google geocoder - this will pause here if we are over the limit.
   result = getGeoDetails(addresses[ii]) 
   print(result$status)     
   result$index <- ii
   #append the answer to the results file.
   geocoded <- rbind(geocoded, result)
   #save temporary results as we are going along
   saveRDS(geocoded, tempfilename)
   # Remove the tempfile if no longer needed
   if (length(geocoded$formatted_address) >= length(addresses)){unlink(tempfilename)}

}
 

#now we add the latitude and longitude to the main data
data$lat <- geocoded$lat
data$long <- geocoded$long
data$accuracy <- geocoded$accuracy
 
#finally write it all to the output files
#saveRDS(data, paste0("../data/", infile ,"_geocoded.rds"))
saveRDS(data, paste0("../data/insurance2/", infile ,"_geocoded.rds"))
write.table(data, file=paste0("../data/insurance2/", infile ,"_geocoded.csv"), sep=",",
row.names=FALSE)
