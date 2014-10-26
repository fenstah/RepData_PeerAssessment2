setwd("C:/Repos/RepData_PeerAssessment2/")

##Make sure the raw data file exist.  
##If data file does not yet exists, download the zip file and unzip the raw data file
getDataFiles <- function()
{
    ##check the subdirectories for train and test do not exist, extract from zip file
    if(!file.exists("StormData.csv.bz2")) 
    {        
        ## if raw zip file does not exist, download it
        if(!file.exists("StormData.csv.bz2"))
        {
            ## if doesnt exist download file
            download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile = "StormData.csv.bz2")
        }    
    }               
}    
 
        
readData <- function()
{
        ##if data doesnt exist in cache, read data from zip file into dataset and cache it for future use
        stormData <- read.csv(stormDataFile<-bzfile("StormData.csv.bz2", "r"))
        close(stormDataFile)
        return (stormData)
}

cleanData <- function(stormData)
{
    #there are 898 unique event types in the raw dataset as determined by length(unique(toupper(stormData$EVTYPE)))
    uniqueEventTypes<-sort(unique(toupper(stormData$EVTYPE)))
    
    #however, according to the storm data documentation only 48 specific event types are allowed
    permittedEventsPattern<-c("^Astronomical Low", "^Avalanc", "^Blizzard", "(?=.*Cold|Wind)(?=.*Chill)",
                              "(?=.*Dense)(?=.*Fog)", "(?=.*Dense)(?=.*Smoke)", "(?=.*Drought)", "(?=.*Dust Devil)",
                              "(?=.*Dust Storm)", "(?=.*Extreme)(?=.*Chill)", "(?=.*Flood)","(?=.*Forst|Freeze)",
                              "(?=.*Funnel)","(?=.*Freezing Fog)", "(?=.*Hail)", "(?=.*Heat)", "(?=.*Heavy Rains)", "(?=.*Heavy Snow)",
                              "(?=.*High Surf)","(?=.*High Wind)", "(?=.*Hurricane|Typhoon)", "(?=.*Ice Storm)", "(?=.*Lake)(?=.*Snow)", 
                              "(?=.*Lake)(?=.*Flood)","(?=.*Lightning)", "(?=.*Marine Hail)", "(?=.*Marine High Wind)",  
                              "(?=.*Marine T)","(?=.*Rip Current)", "(?=.*Seiche)", "(?=.*Sleet)", "(?=.*Storm Surge)", "(?=.*Strong Wind)", 
                              "(?=.*Thunderstorm Wind)","(?=.*Tornado)", "(?=.*Tropical Depression)", "(?=.*Tropical Storm)", "(?=.*Tsunami)", 
                              "(?=.*Volcanic Ash)", "(?=.*Waterspout)", "(?=.*Wildfire)", "(?=.*Winter Storm)", "(?=.*Winter Weather)",
                              "(?=.*Coastal)(?=.*Flood)","(?=.*Debris)", "(?=.*Excessive Heat)","(?=.*Flash)(?=.*Flood)","(?=.*Marine Strong Wind)"
                              )
    permittedEvents<-c("Astronomical Low Tide","Avalanche","Blizzard","Cold/Wind Chill",
                        "Dense Fog","Dense Smoke","Drought","Dust Devil",
                        "Dust Storm","Extreme Cold/Wind Chill","Flood","Frost/Freeze", 
                        "Funnel Cloud","Freezing Fog","Hail","Heat","Heavy Rain","Heavy Snow", 
                        "High Surf","High Wind","Hurricane (Typhoon)","Ice Storm", 
                        "Lake-Effect Snow","Lakeshore Flood","Lightning", 
                        "Marine Hail","Marine High Wind", 
                        "Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Surge/Tide","Strong Wind", 
                        "Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami", 
                        "Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather",
                        "Coastal Flood","Debris Flow","Excessive Heat","Flash Flood","Marine Strong Wind")
    goodEvTTable<-data.frame(permittedEvents, permittedEventsPattern)
    
    #    eventPatternMatch<-paste(permittedEventsPattern, collapse = "|")
    #    filteredEVTypes<-uniqueEventTypes[grep(eventPatternMatch, uniqueEventTypes, ignore.case = TRUE, perl=TRUE)]
    
    #remove columns not needed for this evaluation
    cleanedStormData<-stormData[,c(1:8, 23:28)]
    cleanedStormData$GOODEVTYPE<-NA
    
    
    for (i in 1:length(permittedEvents)) {
        cleanedStormData[toupper(cleanedStormData$EVTYPE) %in% uniqueEventTypes[grep(goodEvTTable$permittedEventsPattern[i], uniqueEventTypes, ignore.case = TRUE, perl=TRUE)],]$GOODEVTYPE<-as.character(goodEvTTable$permittedEvents[i])
    }
    cleanedStormData<-na.omit(cleanedStormData)
    cleanedStormData$GOODEVTYPE<-as.factor(cleanedStormData$GOODEVTYPE)
    
    #convert exponential 'keys' to numerics
    expkeys<-c("h","k","m","b")
    expvalue<-c(2,3,6,9)
    for(i in 1:length(expkeys)) {
        cleanedStormData$PROPDMGEXP<-gsub(expkeys[i],expvalue[i],cleanedStormData$PROPDMGEXP, ignore.case = TRUE)
        cleanedStormData$CROPDMGEXP<-gsub(expkeys[i],expvalue[i],cleanedStormData$CROPDMGEXP, ignore.case = TRUE)
    }
    
    #intentionally convert all non-numeric characters that are left (e.g., " +|-|?") to NA and then zero
    suppressWarnings(cleanedStormData$PROPDMGEXP<-as.numeric(cleanedStormData$PROPDMGEXP))
    suppressWarnings(cleanedStormData$CROPDMGEXP<-as.numeric(cleanedStormData$CROPDMGEXP))
    cleanedStormData$PROPDMGEXP[is.na(cleanedStormData$PROPDMGEXP)]<-0
    cleanedStormData$CROPDMGEXP[is.na(cleanedStormData$CROPDMGEXP)]<-0
    
    cleanedStormData$TOTALDMG = (cleanedStormData$PROPDMG * 10^cleanedStormData$PROPDMGEXP) +
                                (cleanedStormData$CROPDMG * 10^cleanedStormData$CROPDMGEXP)
    return (cleanedStormData)
}

createSummationTable<-function(stormData)
{
    aggregatedStormData<-aggregate(stormData$INJURIES, list(EVENTTYPE=stormData$GOODEVTYPE), sum)
    fatalities<-aggregate(stormData$FATALITIES, list(EVENTTYPE=stormData$GOODEVTYPE), sum)
    totaldmg<-aggregate(stormData$TOTALDMG, list(EVENTTYPE=stormData$GOODEVTYPE), sum)
    colnames(aggregatedStormData)[2]<-"injuries"
    colnames(fatalities)[2]<-"fatalities"
    colnames(totaldmg)[2]<-"totaldmg"
    aggregatedStormData<-merge(aggregatedStormData, merge(fatalities, totaldmg))
    
    return (aggregatedStormData)
}

plotStormData<-function(aggregateStormData)
{
    library(ggplot2)
    injuryplot<-ggplot(aggregateStormData, aes(x = EVENTTYPE, y = injuries, fill = EVENTTYPE)) + 
        geom_bar(stat = "identity") + 
        xlab("Weather Event Type") + 
        ylab("Injuries") + 
        guides(fill=FALSE) + 
        ggtitle("US Injuries due to Weather Event Types") +
        coord_flip()  
    
    fatalplot<-ggplot(aggregateStormData, aes(x = EVENTTYPE, y = fatalities, fill = EVENTTYPE)) + 
        geom_bar(stat = "identity") + 
        xlab("Weather Event Type") + 
        ylab("Fatalities") + 
        guides(fill=FALSE) + 
        ggtitle("US Fatalities due to Weather Event Types") +
        coord_flip()  
    
    options(scipen=12)
    economicplot<-ggplot(aggregateStormData, aes(x = EVENTTYPE, y = totaldmg, fill = EVENTTYPE)) + 
        geom_bar(stat = "identity") + 
        xlab("Weather Event Type") + 
        ylab("Economic Damage in USD") + 
        guides(fill=FALSE) + 
        ggtitle("Economic Damage in the US due to Weather Event Types") +
        coord_flip()
    
    print(injuryplot)
    print(fatalplot)
    print(economicplot)
}

getDataFiles()
stormData<-readData()
stormData<-cleanData(stormData)
aggregateTable<-createSummationTable(stormData)
plotStormData(aggregateTable)