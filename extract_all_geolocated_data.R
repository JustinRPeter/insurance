# Pre-processs the Suncorp data
# Justin Peter, ICACS, 26 October 2015
# Extract all the claims data which is geolocated and place
# into a separate file.

rm(list=ls())

#load the plyr library
library(plyr)

# Set the input directory
indir <- '/home/jpeter/justinp/rfiles/suncorp/data/insurance2/clean'

# Find all the HOME insurance claims
infiles <- list.files(indir,pattern='*rds*',full.names=TRUE)

# Extract the relevant part to construct the file name
ofname <- c()
for (i in seq_along(infiles)){
    ofname[i] <- basename(infiles[i])
    ofname[i] <- strsplit(ofname[[i]][1],'\\.')
    ofname[i] <- substr(ofname[[i]][1],1,11)
}
ofname <- unlist(ofname)


#data_sep <- lapply(infiles,read.csv)
#saveRDS(data_sep,"All_homedata_sep.RDS")
#readRDS("All_home_data_sep.RDS")

# Read the RDS files for speed
ins_data <- lapply(infiles,readRDS)

ins_data <- lapply(ins_data,na.omit)

for (i in seq_along(ins_data)){
    # Write the data to a new file
    saveRDS(ins_data[[i]],paste0(indir,"/",ofname[i],"_geolocated.rds"))

    write.table(ins_data[[i]],
                paste0(indir,"/",ofname[i],"_geolocated.csv"),
                sep=",",row.names=FALSE)
}

# Now output a file which has only the claims data
ins_data_claims <- lapply(ins_data,function(x) x[which(x$Incurred.To.Date >10),])

for (i in seq_along(ins_data_claims)){
    saveRDS(ins_data_claims[[i]],paste0(indir,"/",ofname[i],"_geolocated_claims.rds"))
    write.table(ins_data_claims[[i]],
                paste0(indir,"/",ofname[i],"_geolocated_claims.csv"),
                sep=",",row.names=FALSE)
}

# Now combine all of the claims into a single data set and write that also
# Note there are several ways we can do this
# (1) df <- d.call("rbind",list) doesn't work because extra column names in some
#       of the lists (Claim.Status)
# (2) df <- ldply(list,data.frame) plyr package
# (3) df <- rbind.fill(list) dplyr package
# (4) df <- rbindlist(list,fill=TRUE) data.frame package. Have to use the
#           fill=TRU argument to insert NAs. I don't get identical dfs using
#           this method though. Not sure why?
# Timing shows that rbind.fill is fastest
comp_ins_data_claims <- rbind.fill(ins_data_claims)

saveRDS(comp_ins_data_claims,
        paste0(indir,"/","HOME_all_claims_complete.rds"))
write.table(comp_ins_data_claims,
            paste0(indir,"/","HOME_all_claims_complete.csv"),
            sep=",",ns.strings="NA",row.names=FALSE)
