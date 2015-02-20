library(dplyr)
library(lubridate)
library(ggplot2)

#con <- bzfile("repdata_data_StormData.csv.bz2", open = "r")         # rb gives push back error
#storm <- read.csv(con)                     # read the data from file 'repdata_data_StormData.csv'
#close(con)


if (!exists("storm")) {
storm <- read.csv(bzfile("repdata_data_StormData.csv.bz2")) # 902297
#storm <- read.csv(bzfile("repdata_data_StormData.csv")) # 902297
#storm <- read.table("repdata_data_StormData.csv.bz2")
#	storm <- read.table(bzfile("repdata_data_StormData.csv.bz2"), sep = ",", stringsAsFactors=FALSE)
#  storm <- read.table(bzfile("repdata_data_StormData.csv.bz2"), header = TRUE, sep = ",", stringsAsFactors=FALSE)
}

f <- 5
g <- group_by(storm, EVTYPE)
e <- summarize(g, freq = n())
e <- filter(e, freq>f)                          # show events with a frequency larger than 5
out<- file("EVTYPE.txt"); writeLines(unique(paste(e$EVTYPE, "\t", e$freq)), out); close(out)

# Transform the date strings to dates
storm$BGN_DATE <- as.Date(storm$BGN_DATE, format = "%m/%d/%Y")
storm <- mutate(storm, year = year(storm$BGN_DATE))

# Tidy EVTYPE from 985 categories down to 718
storm <- mutate(storm, events = EVTYPE)
storm$EVTYPE <- toupper(storm$EVTYPE)
storm$EVTYPE <- gsub("[\\./-]", " ", storm$EVTYPE)
storm$EVTYPE <- gsub(" +", " ", storm$EVTYPE)
storm$EVTYPE <- gsub("^ ", "", storm$EVTYPE)
storm$EVTYPE <- gsub("[0-9]", "", storm$EVTYPE)
storm$EVTYPE <- gsub("[\\(\\)]", "", storm$EVTYPE)
storm$EVTYPE <- gsub(" $", "", storm$EVTYPE)

g0 <- group_by(storm, EVTYPE)
e0 <- summarize(g0, freq = n())
e0 <- filter(e0, freq>f)                          # show events with a frequency larger than 5
out<- file("EVTYPE-step1.txt"); writeLines(unique(paste(e0$EVTYPE, "\t", e0$freq)), out); close(out)

storm$EVTYPE <- gsub("FLOOD[A-Z]*", "FLOOD", storm$EVTYPE)
storm$EVTYPE <- gsub("WIND[S]*", "WIND", storm$EVTYPE)
storm$EVTYPE <- gsub("RAINS", "RAIN", storm$EVTYPE)
storm$EVTYPE <- gsub("LANDSLIDES", "LANDSLIDE", storm$EVTYPE)
storm$EVTYPE <- gsub("MUD SLIDE", "MUDSLIDE", storm$EVTYPE)
storm$EVTYPE <- gsub("MUDSLIDES", "MUDSLIDE", storm$EVTYPE)
storm$EVTYPE <- gsub("CURRENTS", "CURRENT", storm$EVTYPE)
storm$EVTYPE <- gsub("THUNDERSTORMS", "THUNDERSTORM", storm$EVTYPE)
storm$EVTYPE <- gsub("SQUALLS", "SQUALL", storm$EVTYPE)
storm$EVTYPE <- gsub("TORNADOS", "TORNADO", storm$EVTYPE)
storm$EVTYPE <- gsub("WILDFIRES", "WILDFIRE", storm$EVTYPE)
storm$EVTYPE <- gsub("WAVES", "WAVE", storm$EVTYPE)

g1 <- group_by(storm, EVTYPE)
e1 <- summarize(g1, freq = n())
e1 <- filter(e1, freq>f)                          # show events with a frequency larger than 5
out<- file("EVTYPE-step2.txt"); writeLines(unique(paste(e1$EVTYPE, "\t", e1$freq)), out); close(out)

storm$EVTYPE <- gsub("FLOOD FLASH FLOOD", "FLASH FLOOD", storm$EVTYPE)
storm$EVTYPE <- gsub("PRECIPITATION", "PRECIP", storm$EVTYPE)
storm$EVTYPE <- gsub("TORNADO F", "TORNADO", storm$EVTYPE)
storm$EVTYPE <- gsub("HEAT WAVE", "HEAT", storm$EVTYPE)
storm$EVTYPE <- gsub("HURRICANE.*", "HURRICANE", storm$EVTYPE)
storm$EVTYPE <- gsub("HURRICANE", "HURRICANE TYPHOON", storm$EVTYPE)
storm$EVTYPE <- gsub("STORM SURGE TIDE", "STORM SURGE", storm$EVTYPE)
storm$EVTYPE <- gsub(".*THUNDERSTORM WIND.*", "THUNDERSTORM WIND", storm$EVTYPE)
storm$EVTYPE <- gsub("TSTM.*", "TSTM", storm$EVTYPE)
# keep TSTM as separate group as TSTM WIND could be Thunder Storm/Wind or Tropical Storm/Wind, which both are separate events
storm$EVTYPE <- gsub("TROPICAL STORM.*", "TROPICAL STORM", storm$EVTYPE)
storm$EVTYPE <- gsub("WILD.*FIRE.*", "WILDFIRE", storm$EVTYPE)

g2 <- group_by(storm, EVTYPE)
e2 <- summarize(g2, freq = n())
e2 <- filter(e2, freq>f)                          # show events with a frequency larger than 5
out<- file("EVTYPE-step3.txt"); writeLines(unique(paste(e2$EVTYPE, "\t", e2$freq)), out); close(out)

storm$PROMDMEXP <- toupper(storm$PROPDMGEXP)
storm$CROPDMGEXP <- toupper(storm$CROPDMGEXP)


#View(sort(unique(storm$EVTYPE)))

g_s <- group_by(storm, EVTYPE)
s_evt <- summarize(g_s, freq = n())


# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

head(storm[with(storm, order(-damage)),],10)

#FATALITIES, INJURIES, PROPDMG, CROPDMG, CROPDMGEXP
g_i <- group_by(storm, EVTYPE, year)
inj_evt <- summarize(g_i, freq = n())

fatals <- filter(storm, FATALITIES > 0 | INJURIES > 0)


g_f <- group_by(fatals, EVTYPE, year)
fat_evt <- summarize(g_f, freq = n())

damages <- filter(storm, PROPDMG > 0 | CROPDMG > 0 | CROPDMGEXP > 0)
g_d <- group_by(damages, EVTYPE, year)
dam_evt <- summarize(g_d, freq = n())

g_s_year <- group_by(storm, EVTYPE, year)
s_evt_year <- summarize(g_s_year, freq = n())


dim(filter(storm, grepl("[^HKMB]", storm$PROPDMGEXP)))
dim(filter(storm, grepl("[HKMB]", storm$PROPDMGEXP)))
dim(filter(storm, grepl("[^HKMB]", storm$CROPDMGEXP)))
dim(filter(storm, grepl("[HKMB]", storm$CROPDMGEXP)))

Only 9 out of 14681 will be affected. The sum of this 9 is 135.7 compared to 1357802 in total. It can only have a significant impact if the multiplier would be in the billions. As this data goes back to years 1994 and 1995, I assume this is due to input errors and will ignore it.

```{r}
dim(storm)
```


storm <- mutate(storm, PD = PROPDMG * as.numeric(mult_PD_EXP))
storm <- mutate(storm, CD = CROPDMG * as.numeric(mult_CD_EXP))


http://stackoverflow.com/questions/8750871/ggplot2-reverse-order-of-scale-brewer

options(scipen=999)
storm %>% select(REFNUM, PROPDMGEXP, CROPDMGEXP, damage) %>% arrange(desc(damage))
