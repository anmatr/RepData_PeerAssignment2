---
title: "Weather Event Analysis"
author: "André Treffeisen"
date: "Tuesday, February 20, 2015"
output: html_document
---


The U.S. National Oceanic and Athmospheric Administration's (NOAA) storm database tracks characteristics of major storms and weather events in the United States, including estimates of any fatalities, injuries and property damage. This database is used in this analysis to give a basic overview on this two questions:

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

2. Across the United States, which types of events have the greatest economic consequences?

The database can be downloaded from this location: [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).

There is also some documentation of the database available:

* National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

* National Climatic Data Center [Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)


### 1. Read the storm database
After downloading the database the document can be loaded with the following command.

```r
storm <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))
```
The number of rows loaded from the storm database is 902,397 and the first few lines are displayed below.

```r
dim(storm)
```

```
## [1] 902297     37
```

```r
head(storm[, 1:8])
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE
## 1 TORNADO
## 2 TORNADO
## 3 TORNADO
## 4 TORNADO
## 5 TORNADO
## 6 TORNADO
```
Then we transform the date strings to dates and attach a new column for the year which is extracted from the date column.

```r
storm$BGN_DATE <- as.Date(storm$BGN_DATE, format = "%m/%d/%Y")
storm <- mutate(storm, year = year(storm$BGN_DATE))
```

### 2. Tidy data - column EVTYPE
The next column we are interested in is the column EVTYPE. The official permitted storm data events are mentioned in document "pd01016005curr.pdf", section 2.1 and shown on table 2.1.1 "Storm Data Event Table". Many entries in column EVTYPE look very different from this 48 categories, in fact there are 985 distinct categories given. Below are a couple of changes to this  data which brings the entries much closer to the given set of permitted entries and narrows the categories down to 619. There are still some entries which don't fall into the bucket of 48, but looking at the event frequency, as they are very small, we keep them in there without collapsing into other major categories.

```r
length(unique(storm$EVTYPE))                                  # 985 categories to start with
```

```
## [1] 985
```


```r
storm$EVTYPE <- toupper(storm$EVTYPE)                         # change all entries to upper case
storm$EVTYPE <- gsub("[\\./-]", " ", storm$EVTYPE)            # remove . and -
storm$EVTYPE <- gsub(" +", " ", storm$EVTYPE)                 # remove multiple blanks with single blank
storm$EVTYPE <- gsub("^ ", "", storm$EVTYPE)                  # remove any blanks at start
storm$EVTYPE <- gsub("[0-9]", "", storm$EVTYPE)               # remove any numbers
storm$EVTYPE <- gsub("[\\(\\)]", "", storm$EVTYPE)            # remove any open and closing round brackets ()
storm$EVTYPE <- gsub(" $", "", storm$EVTYPE)                  # remove any trailing blanks
```
This first clean up leaves us with 725 categories.

```r
length(unique(storm$EVTYPE))                                  # 725 categories
```

```
## [1] 725
```

A further view on the 725 entries showed that some category names are entered in a different way then the official permitted names and hence we clean up this as well. The entries in the permitted table never use plural (e. g. Flood instead of Floods), so this is changed below:

```r
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
```
This second clean up results to 671 categories.

```r
length(unique(storm$EVTYPE))                                  # 671 categories
```

```
## [1] 671
```

As some of the main categories have been entered in a different way, the EVTYPE on them also get's changed. First we find out the significance of the wronly entered data and sum the frequency of this event types.

```r
g0 <- group_by(storm, EVTYPE)
e0 <- summarize(g0, freq = n())
e0 <- filter(e0, freq > 10)                                   # show events with a frequency larger than 10
print(e0[20:30,])
```

```
## Source: local data frame [11 x 2]
## 
##                            EVTYPE  freq
## 1                    EXTREME HEAT    22
## 2               EXTREME WINDCHILL   204
## 3  EXTREME WINDCHILL TEMPERATURES    19
## 4                     FLASH FLOOD 54993
## 5               FLASH FLOOD FLOOD    32
## 6                           FLOOD 25450
## 7               FLOOD FLASH FLOOD   628
## 8                             FOG   538
## 9                          FREEZE    76
## 10               FREEZING DRIZZLE    24
## 11                   FREEZING FOG    46
```

There are still some entries with higer frequency (frequeny > 10 was only set to emphasise on the example, further analysis will consider all frequencies) which fall into separate categories (e. g. "FLASH FLOOD FLOOD"", "FLOOD FLASH FLOOD" which should be in FLASH FLOOD) and would screw the analysis in a more or less significant way (not even cosidered the actual damage on life or material). We combine them into the official category name and this leads to the following 'fixes':

```r
storm$EVTYPE <- gsub("FLOOD FLASH FLOOD", "FLASH FLOOD", storm$EVTYPE)      # combine the 'FLOOD FLASH FLOOD' category with 'FLASH FLOOD'
storm$EVTYPE <- gsub("PRECIPITATION", "PRECIP", storm$EVTYPE)               # use the short name for precipitation
storm$EVTYPE <- gsub("TORNADO F", "TORNADO", storm$EVTYPE)                  #
storm$EVTYPE <- gsub("HEAT WAVE", "HEAT", storm$EVTYPE)
storm$EVTYPE <- gsub("HURRICANE.*", "HURRICANE", storm$EVTYPE)              # combine any specific hurricane with the overall group
storm$EVTYPE <- gsub("HURRICANE", "HURRICANE TYPHOON", storm$EVTYPE)        # set the official permitted name 'HURRICANE (TYPHOONE) but omit the brackets.
storm$EVTYPE <- gsub("STORM SURGE TIDE", "STORM SURGE", storm$EVTYPE)       # combine storm surge and storm tide in one category
storm$EVTYPE <- gsub(".*THUNDERSTORM WIND.*", "THUNDERSTORM WIND", storm$EVTYPE) # 
storm$EVTYPE <- gsub("TSTM.*", "THUNDERSTORM WIND", storm$EVTYPE)           # combine TSTM with THUNDERSTORM WIND
storm$EVTYPE <- gsub("TROPICAL STORM.*", "TROPICAL STORM", storm$EVTYPE)    # use official permitted name
storm$EVTYPE <- gsub("WILD.*FIRE.*", "WILDFIRE", storm$EVTYPE)              # use official permitted name
```
The category TSTM could be 'Thunder Storm/Wind' or 'Tropical Storm/Wind', which both are separate permitted event categories. It was decided to combine it with the category 'Thunderstrom Wind' as a quick Google check showed that TSTM is usually the abbrevation for Thunderstorm. It was also assumed that they are not marine as it would else fall into categor 'Marine Thunderstorm Wind'.

This third clean up results to 618 categories.

```r
length(unique(storm$EVTYPE))                                  # 618 categories
```

```
## [1] 618
```

### 3. Tidy data - columns PROPDMGEXP and CROPDMGEXP
Columns PROPDMGEXP and CROPDMGEXP contain the exponent for the property and crop damage figures. They contain upper and lower case characters for thousands (M) and millions (K), which will be all changed to upper case.

```r
unique(storm$PROPDMGEXP)                                      # show current entries for property damage exponent
```

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

```r
unique(storm$CROPDMGEXP)                                      # show current entries for cromp damage exponent
```

```
## [1]   M K m B ? 0 k 2
## Levels:  ? 0 2 B k K m M
```

```r
storm$PROPDMGEXP <- toupper(storm$PROPDMGEXP)                 # change to upper case
storm$CROPDMGEXP <- toupper(storm$CROPDMGEXP)                 # change to upper case
```
Further they have numbers and question marks in them. To remove this entries we define a simple function 'multiplier' which sets all entries to the actual multiplier: H = 100 (hundreds), K = 1,000 (thousands), M = 1,000,000 (millions), B = 1,000,000,000 (billions), 1 will be the default multiplier for any other entry.

```r
multiplier <- function(x) { 
  m = 1
  if (x == "H") {
    m = 10
  }
  if (x == "K") {
    m = 1000
  }
  if (x == "M") {
    m = 1000000
  }
  if (x == "B") {
    m = 1000000000
  }
  m
}
storm <- mutate(storm, mult_PD_EXP = PROPDMGEXP)                  # create the new columns
storm <- mutate(storm, mult_CD_EXP = CROPDMGEXP)

storm$mult_PD_EXP <- sapply(storm$PROPDMGEXP, FUN="multiplier")   # calculate numeric multiplier from characters in column PROPDMGEXP
storm$mult_CD_EXP <- sapply(storm$CROPDMGEXP, FUN="multiplier")   # calculate numeric multiplier from characters in column CROPDMGEXP
```
There are now the appropriate multiplier numbers in the fields 'mult_PD_EXP' and 'mult_CD_EXP'.

```r
format(unique(storm$mult_PD_EXP), scientific=F)                   # show current entries for property damage exponent
```

```
## [1] "      1000" "   1000000" "         1" "1000000000" "        10"
```

```r
format(unique(storm$mult_CD_EXP), scientific=F)                   # show current entries for crop damage exponent
```

```
## [1] "         1" "   1000000" "      1000" "1000000000"
```

At this stage we can now calculate the damage for the property and crop columns which we store in a new column called 'damage'.

```r
storm <- mutate(storm, damage = PROPDMG * mult_PD_EXP + CROPDMG * mult_CD_EXP)
```

### 4. Most harmful events to live and health
Now we can tackle the originally raised questions. First the most harmful events, let's repeat the question:
Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
As we are looking at most harmful with respect to population, the two columns FATALITIES and INJURIES are relevant. 
#### 4.1 Injuries
First the total number of injuries per event type is calculated and aggregated below.

```r
g_i <- group_by(storm, EVTYPE)                                    # group by event type
inj_evt <- summarize(g_i, injuries = sum(INJURIES))               # calculate the sum of injuries
inj_evt_high <- filter(inj_evt, injuries > 1000)                  # filter only events with more than 1000 injuries
inj_evt_high$EVTYPE <- reorder(inj_evt_high$EVTYPE, -inj_evt_high$injuries)   # reorder the events by frequency
```
The following is a graphical representation of the total amount of inuries per weather event and shows that the category tornado is by far the ones with highest injuries. After that in close succession the categories 'thunderstorm wind', flood, 'excessive heat' and lightning.

```r
cc <- length(inj_evt_high$EVTYPE)                                 # get the number of total event types
palette <- colorRampPalette(rev(brewer.pal(9, "Greys")))          # match the color palette grey shapes
ggplot(inj_evt_high) + geom_bar(stat="identity", aes(EVTYPE, injuries), fill=palette(cc)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Injuries Per Event Type\n") + xlab("Weather Event Type") + ylab("Injuries")
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png) 

The actual figures are shown below where tornado's as cause for injuries are standing out the most.

```r
head(arrange(inj_evt_high, desc(injuries)), 10)
```

```
## Source: local data frame [10 x 2]
## 
##               EVTYPE injuries
## 1            TORNADO    91364
## 2  THUNDERSTORM WIND     9495
## 3              FLOOD     6791
## 4     EXCESSIVE HEAT     6525
## 5          LIGHTNING     5230
## 6               HEAT     2479
## 7          ICE STORM     1975
## 8        FLASH FLOOD     1800
## 9           WILDFIRE     1606
## 10         HIGH WIND     1440
```
#### 4.2 Fatalities
Secondly the total number of fatalities per event type is calculated and aggregated below.

```r
g_f <- group_by(storm, EVTYPE)                                    # group by event type
fat_evt <- summarize(g_f, fatalities = sum(FATALITIES))           # calculate the sum of injuries
fat_evt_high <- filter(fat_evt, fatalities > 200)                 # filter only events with more than 200 fatalities
fat_evt_high$EVTYPE <- reorder(fat_evt_high$EVTYPE, -fat_evt_high$fatalities)   # reorder the events by frequency
```
The graph below shows the total amount of fatalities per weather event and depicts that again tornado is by far the category with the most fatalities. Then the category 'exessive heat' followed by heat, flash flood, lightning and 'thunderstorm wind' in fairly close succession.

```r
cc <- length(fat_evt_high$EVTYPE)
palette <- colorRampPalette(rev(brewer.pal(9, "Greys")))
ggplot(fat_evt_high) + geom_bar(stat="identity", aes(EVTYPE, fatalities), fill=palette(cc)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Fatalities Per Event Type\n") + xlab("Weather Event Type") + ylab("Fatalities")
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png) 
The actual figures are shown bleow where again tornado's are the most deadliest causes for fatalities on weather events.

```r
head(arrange(fat_evt_high, desc(fatalities)), 10)
```

```
## Source: local data frame [10 x 2]
## 
##               EVTYPE fatalities
## 1            TORNADO       5633
## 2     EXCESSIVE HEAT       1903
## 3               HEAT       1114
## 4        FLASH FLOOD       1016
## 5          LIGHTNING        817
## 6  THUNDERSTORM WIND        719
## 7        RIP CURRENT        572
## 8              FLOOD        476
## 9          HIGH WIND        283
## 10         AVALANCHE        224
```

### 5. Most harmful economic events
The second question which we want to answer is as follows:
2. Across the United States, which types of events have the greatest economic consequences?

The storm database gives two columns 'PROPDMG' and 'CROPDMG' which we already combined in section 3 to a single column called 'damage'. We can now sum the damages and aggregate as follows:


```r
storm[storm$REFNUM==605943,41] <- 147500000
g_d <- group_by(storm, EVTYPE)                                   # group by event type
dam_evt <- summarize(g_d, damages = sum(damage/1000000000))      # sum all damages and convert to billions
dam_evt_high <- filter(dam_evt, damages > 5)                     # filter out events with damages below 5
dam_evt_high$EVTYPE <- reorder(dam_evt_high$EVTYPE, -dam_evt_high$damages)    # reorder the event type
```
It is assumed that the economic figures from this storm database are already adjusted for inflation and normalized.

```r
cc <- length(dam_evt_high$EVTYPE)
palette <- colorRampPalette(rev(brewer.pal(9, "Greys")))
ggplot(dam_evt_high) + geom_bar(stat="identity", aes(EVTYPE, damages), fill=palette(cc)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Damages Per Event Type\n") + xlab("Weather Event Type") + ylab("Damages (Billion US$)")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png) 


```r
head(arrange(dam_evt_high, desc(damages)))
```

```
## Source: local data frame [6 x 2]
## 
##              EVTYPE  damages
## 1 HURRICANE TYPHOON 90.27147
## 2           TORNADO 57.35689
## 3       STORM SURGE 47.96558
## 4             FLOOD 35.55784
## 5              HAIL 18.75886
## 6       FLASH FLOOD 18.16337
```




