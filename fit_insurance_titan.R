# This script tries to fit a multiple linear regresion to the insurance costs
# incurred as a function of various TITAN quantities.

# Justin R. Peter, ICACS, USQ, 3 Nov 2015.

rm(list=ls())

library(lattice)
lattice.options(default.theme = standard.theme(color = FALSE))
library(latticeExtra)

library(grid)
library(gridExtra)
library(ggplot2)
library(scales) #For formatting of axes e.g. percent_format

#library(psych) #For geometric mean function
library(EnvStats) #For geometric mean function


# Read in the data
data <- readRDS("titan_insurance_merged.rds")

# Now try to fit some models

#(1) Plot some distributions of the insurance and TITAN data

#plot_vars <- c("precip_area")

plot_vars <- c("precip_area","precip_rate",
               "vol","mass","top",
               "max_dBZ","mean_dBZ","vil")

#plot_label_strings <- c("Convective storm area [km"^"2","]")
plot_label_strings <- c("Convective storm area [km^2]",
                        "Convective storm rate [mm/hr]",
                        #"Convective storm volume [km"^"3","]",
                        "Convective storm volume [km^3]",
                        "Convective storm mass [ktons]",
                        "Convective storm top [km]",
                        "Convective storm max dBZ",
                        "Convective storm mean dBZ",
                        "Vertically integrated liquid")

# Create a factor based on the reflectivity threshold
    refl_threshold.f <- factor(data$refl_threshold,
                               #labels=c("35 dBZ", "40 dBZ","45 dBZ","50 dBZ"))
                               labels=c("35 dBZ", "40 dBZ","45 dBZ"))
    
# Add it as a column to data set. Need to do this because facet_wrap
# doesn't have a labeller command like facet_grid.
# Essentiall we want to have a label of 35 dBZ not just 35.
data$refl_threshold.f <- refl_threshold.f



#for (pvi in seq_along(plot_vars[1])){
for (pvi in seq_along(plot_vars)){


    #Evaluate some statistics
    # Could use ddply, but can't pass string variable to it.
    # It just averages over the entire data frame.
    #Could use tapply but it doesn't return a data frame.
    #Best option is aggregate. Returns a data frame
    #mnstats <- ddply(data,.(radar_name,refl_threshold.f),summarise,
    #              #precip_area.mean=mean(precip_area))
    #              #mnvar=mean(data[[plot_vars[1]]]))
    #              mnvar=mean(plot_vars[1]))
    #gmnstats <- ddply(data,.(radar_name,refl_threshold.f),summarise,
    #              #precip_area.gmmean=geometric.mean(precip_area))
    #              gmmnvar=geometric.mean(data[[plot_vars[pvi]]]))
    #sdstats <- ddply(data,.(radar_name,refl_threshold.f),summarise,
    #              #precip_area.sd=sd(precip_area))
    #              sdvar=sd(data[[plot_vars[pvi]]]))

    mnstats   <- aggregate(data[[plot_vars[pvi]]],
                        #list(refl_threshold.f,data$radar_name),
                        list(refl_threshold.f),
                        mean)
    sdstats   <- aggregate(data[[plot_vars[pvi]]],
                        list(refl_threshold.f),
                        sd)
    gmnstats  <- aggregate(data[[plot_vars[pvi]]],
                        list(refl_threshold.f),
                        geoMean)
    gsdstats  <- aggregate(data[[plot_vars[pvi]]],
                        list(refl_threshold.f),
                        geoSD)
    #Maybe evaluate the standard error of the mean and geometric mean.


    
    #####Plotting individual facets
    #pdf("precip_area.pdf")
    plot_name <- paste0(plot_vars[pvi],".pdf")
    pdf(plot_name)
    #p1<-ggplot(data,aes(x=precip_area))+
    p1<-ggplot(data,aes_string(x=plot_vars[pvi]))+
        stat_bin(aes(y=..density..),geom="step",binwidth=0.1,size=1.5)+
        scale_x_log10()+
        #labs(x=expression(paste("Convective precipitation area [km"^"2","]",sep=""))) 
        labs(x=expression(paste(plot_label_strings[pvi],sep=""))) 
    plot(p1)
    
    #p2<-ggplot(data,aes(x=precip_area))+ 
    #p2<-ggplot(data,aes(x=precip_area,linetype=radar_name))+ 
    #p2<-ggplot(data,aes_string(x=plot_vars[pvi],linetype="radar_name"))+ 
    p2<-ggplot(data,aes_string(x=plot_vars[pvi]))+ 
        #facet_wrap(~refl_threshold.f)+
        facet_wrap(~refl_threshold.f)+
        ##stat_bin(aes(y=..density..),geom="step",binwidth=0.1,size=1.5)+
        stat_bin(aes(y=..density..),geom="step",
                 binwidth=0.1,size=1.5,position="identity")+

        scale_x_log10()+
        #geom_vline(data=mnstats,aes(xintercept=precip_area.mean),linetype="dashed")+
        #geom_vline(data=gmnstats,aes(xintercept=precip_area.gmmean),linetype="dashed")+
        #geom_vline(data=gmmnstats,aes(xintercept=gmmnstats),linetype="dashed")+
        #labs(x=expression(paste("Convective precipitation area [km"^"2","]",sep="")))
        labs(x=expression(paste(plot_label_strings[pvi],sep="")))
    plot(p2)
    #grid.arrange(p1,p2,nrow=2)
    
    p3 <-ggplot(data,aes_string(x=plot_vars[pvi],
                #colour="refl_threshold.f",linetype="radar_name"))+
                colour="refl_threshold.f"))+
          geom_freqpoly(aes(y=..density..),size=1.5,binwidth=0.1)+scale_x_log10()+
          labs(x=expression(paste(plot_label_strings[pvi],sep="")))
    plot(p3)
    
    #Plotting counts (with percentage labels).
    p4 <- ggplot(data,aes_string(x=plot_vars[pvi],colour="refl_threshold.f"))+
          geom_freqpoly(aes(y=..count../sum(..count..)),size=1.5,binwidth=0.1)+
          scale_x_log10()+
          scale_y_continuous(labels=percent_format())
    plot(p4)
     
    dev.off()
}






