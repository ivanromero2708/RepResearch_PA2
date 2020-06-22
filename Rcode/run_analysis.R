## Creating directories inside the project folder
if(!dir.exists("data")) {dir.create("data")}
if(!dir.exists("figures")) {dir.create("figures")}
if(!dir.exists("Rcode")) {dir.create("Rcode")}

## Download file
rdatafilename <- "stormdata.zip"
rdatafileloc <- paste0("./",rdatafilename)
if(!file.exists(rdatafileloc)) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileUrl, rdatafileloc, method = "curl")
}

## Decompressing the zip file and verifying
if(!file.exists("./data/storm.csv")) {
        library(R.utils)
        bunzip2(rdatafileloc, "./data/storm.csv", remove = TRUE)
}

## Reading and cleaning the data
storm_rawdata <- read.csv("./data/storm.csv")
names(storm_rawdata)

library(dplyr)
storm_rawdata <- storm_rawdata %>% subset(PROPDMG > 0, CROPDMG > 0) %>%
                select(STATE, BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
storm_rawdata <- storm_rawdata[storm_rawdata$PROPDMGEXP %in% c("", "K", "M", "B") & storm_rawdata$CROPDMGEXP %in% c("", "K", "M", "B"), ]
str(storm_rawdata)

## Calculating Crop and Property damages in US

storm_rawdata$PROPDMG_inUS <- mapply(function(DMGval, DMGexp) {if (DMGexp == "K" | DMGexp == "k") {DMGval * 1000} 
                           else if (DMGexp == "M" | DMGexp == "m") {DMGval * 1e+06} 
                           else if (DMGexp == "B") {DMGval * 1e+09}
                           else if (DMGexp == "") {DMGval*0}}, 
                           storm_rawdata$PROPDMG, storm_rawdata$PROPDMGEXP)


storm_rawdata$CROPDMG_inUS <- mapply(function(DMGval, DMGexp) {if (DMGexp == "K" | DMGexp == "k") {DMGval * 1000} 
                                else if (DMGexp == "M" | DMGexp == "m") {DMGval * 1e+06} 
                                else if (DMGexp == "B") {DMGval * 1e+09}
                                else if (DMGexp == "") {DMGval*0}}, 
                                storm_rawdata$CROPDMG, storm_rawdata$CROPDMGEXP)

## Calculating FATALITIES AND INJURIES per Event Type
INJURIES_EVTYPE <- storm_rawdata %>% group_by(EVTYPE) %>%
        summarize(TOTAL_INJURIES = sum(INJURIES, na.rm = TRUE)) %>%
        arrange(desc(TOTAL_INJURIES))

FATALITIES_EVTYPE <- storm_rawdata %>% group_by(EVTYPE) %>%
        summarize(TOTAL_FATALITIES = sum(FATALITIES, na.rm = TRUE)) %>%
        arrange(desc(TOTAL_FATALITIES))

## Creating Health impact Data frame
healthimpact_evtype <- merge(INJURIES_EVTYPE, FATALITIES_EVTYPE, by = "EVTYPE")
healthimpact_evtype <- healthimpact_evtype %>% mutate(EVENTS = (TOTAL_FATALITIES+TOTAL_INJURIES), SEVERITY = TOTAL_FATALITIES/(TOTAL_FATALITIES+TOTAL_INJURIES)) %>%
                                                arrange(desc(EVENTS))
# Subsetting top 10 event types per number of health related events
healthimpact_evtype <- healthimpact_evtype[1:10,]

## Calculating Crop and Property Damages economic impact per event type
PROPDMG_EVTYPE <- storm_rawdata %>% group_by(EVTYPE) %>%
        summarize(TOTAL_PROPDMG = sum(PROPDMG_inUS, na.rm = TRUE)) %>%
        arrange(desc(TOTAL_PROPDMG))

CROPDMG_EVTYPE <- storm_rawdata %>% group_by(EVTYPE) %>%
        summarize(TOTAL_CROPDMG = sum(CROPDMG_inUS, na.rm = TRUE)) %>%
        arrange(desc(TOTAL_CROPDMG))

economicimpact_evtype <- merge(CROPDMG_EVTYPE, PROPDMG_EVTYPE, by = "EVTYPE")
economicimpact_evtype <- economicimpact_evtype %>% mutate(TOTAL_DMG = (TOTAL_CROPDMG+TOTAL_PROPDMG)) %>%
        arrange(desc(TOTAL_DMG))

# Subsetting top 10 economic impact per event types 
economicimpact_evtype <- economicimpact_evtype[1:10,]