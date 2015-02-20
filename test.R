library(dplyr)
library(lubridate)

#con <- bzfile("repdata_data_StormData.csv.bz2", open = "rb")
#storm <- read.csv(con)                     # read the data from file 'repdata_data_StormData.csv'
#close(con)

if (!exists("storm")) {
  storm <- read.table(bzfile("repdata_data_StormData.csv.bz2"), header = TRUE, sep = ",", stringsAsFactors=FALSE)
}

# Transform the date strings to dates
storm$BGN_DATE <- as.Date(storm$BGN_DATE, format = "%m/%d/%Y")
storm <- mutate(storm, year = year(storm$BGN_DATE))

# Tidy EVTYPE from 985 categories down to 718
mutate(storm, events = EVTYPE)
storm$EVTYPE <- toupper(storm$EVTYPE)
storm$EVTYPE <- gsub("[\\./-]", " ", storm$EVTYPE)
storm$EVTYPE <- gsub("  ", " ", storm$EVTYPE)
storm$EVTYPE <- gsub(" +", " ", storm$EVTYPE)
storm$EVTYPE <- gsub("^ ", "", storm$EVTYPE)
storm$EVTYPE <- gsub("[0-9]", "", storm$EVTYPE)
storm$EVTYPE <- gsub("[\\(\\)]", "", storm$EVTYPE)
storm$EVTYPE <- gsub(" $", "", storm$EVTYPE)

storm$EVTYPE <- gsub("FLOOD[A-Z]*", "FLOOD", storm$EVTYPE)
storm$EVTYPE <- gsub("FLOOD FLASH FLOOD", "FLASH FLOOD", storm$EVTYPE)
storm$EVTYPE <- gsub("WIND[S]*", "WIND", storm$EVTYPE)
storm$EVTYPE <- gsub("RAINS", "RAIN", storm$EVTYPE)
storm$EVTYPE <- gsub("LANDSLIDES", "LANDSLIDE", storm$EVTYPE)
storm$EVTYPE <- gsub("PRECIPITATION", "PRECIP", storm$EVTYPE)
storm$EVTYPE <- gsub("MUD SLIDE", "MUDSLIDE", storm$EVTYPE)
storm$EVTYPE <- gsub("MUDSLIDES", "MUDSLIDE", storm$EVTYPE)
storm$EVTYPE <- gsub("CURRENTS", "CURRENT", storm$EVTYPE)
storm$EVTYPE <- gsub("THUNDERSTORMS", "THUNDERSTORM", storm$EVTYPE)
storm$EVTYPE <- gsub("SQUALLS", "SQUALL", storm$EVTYPE)
storm$EVTYPE <- gsub("TORNADOS", "TORNADO", storm$EVTYPE)
storm$EVTYPE <- gsub("TORNADO F", "TORNADO", storm$EVTYPE)
storm$EVTYPE <- gsub("WILDFIRES", "WILDFIRE", storm$EVTYPE)
storm$EVTYPE <- gsub("WAVES", "WAVE", storm$EVTYPE)
storm$EVTYPE <- gsub("HEAT WAVE", "HEAT", storm$EVTYPE)
storm$EVTYPE <- gsub("HURRICANE.*", "HURRICANE", storm$EVTYPE)
storm$EVTYPE <- gsub("HURRICANE", "HURRICANE TYPHOON", storm$EVTYPE)
storm$EVTYPE <- gsub("STORM SURGE TIDE", "STORM SURGE", storm$EVTYPE)
storm$EVTYPE <- gsub(".*THUNDERSTORM WIND.*", "THUNDERSTORM WIND", storm$EVTYPE)
storm$EVTYPE <- gsub("TSTM.*", "TSTM", storm$EVTYPE)
# keep TSTM as separate group as TSTM WIND could be Thunder Storm/Wind or Tropical Storm/Wind, which both are separate events
storm$EVTYPE <- gsub("TROPICAL STORM.*", "TROPICAL STORM", storm$EVTYPE)
storm$EVTYPE <- gsub("WILD.*FIRE.*", "WILDFIRE", storm$EVTYPE)

print(c)
#View(sort(unique(storm$EVTYPE)))

g <- group_by(storm, EVTYPE)
events <- summarize(g, eventtypes = n())


# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

#FATALITIES, INJURIES, PROPDMG, CROPDMG, CROPDMGEXP
fatals <- filter(storm, FATALITIES > 0 | INJURIES >0 |PROPDMG > 0 | CROPDMG > 0 | CROPDMGEXP > 0)

g_f <- group_by(fatals, EVTYPE)
fat_evt <- summarize(g_f, eventtypes = n())

g_year <- group_by(storm, EVTYPE, year)
e_by_year <- summarize(g_year, freq = n())
c <- c(1:10)
