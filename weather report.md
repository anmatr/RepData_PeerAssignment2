---
title: "Weather Event Analysis"
author: "André Treffeisen"
date: "Tuesday, February 20, 2015"
output: html_document
---

### 1. Synopsis

The U.S. National Oceanic and Athmospheric Administration's (NOAA) storm database tracks characteristics of major storms and weather events in the United States, including estimates of any fatalities, injuries and property damage. This database is used in this analysis to give a basic overview on this two questions:

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

2. Across the United States, which types of events have the greatest economic consequences?

The database can be downloaded from this location: [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).

There is also some documentation of the database available:

* National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

* National Climatic Data Center [Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)


### 2. Data Processing

Loading necessary libraries:


```r
library(dplyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(RColorBrewer)
```

#### 2.1 Read the storm database

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
head(storm)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```
The columns we are interested in for this analysis are as follows:

|Column      |Content
|------------|-------------
|EVTYPE      |Weather event type.
|FATALITIES  |Number of fatalities from this weather event.
|INJURIES    |Number of injuries from this weather event.
|PROPDMG     |US$ amount of property damage caused by this weather event.
|PROPDMGEXP  |Multiplier for the column PROPDMG.
|CROPDMG     |US$ amount of crop damage caused by this weather event.
|CROPDMGEXP  |Multiplier for the column CROPDMG.

#### 2.2 Tidy data - column EVTYPE

The first column we are interested in is the column EVTYPE. The official permitted storm data events are mentioned in the [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf), section 2.1 and shown on table 2.1.1 "Storm Data Event Table". Many entries in column EVTYPE look very different from this 48 categories, in fact there are 985 distinct categories given. Below are a couple of changes to this  data which brings the entries much closer to the given set of permitted entries and narrows the categories down to 619. There are still some entries which don't fall into the bucket of 48, but looking at the event frequency, as they are very small, we keep them in there without collapsing into other major categories.

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
g0 <- group_by(storm, EVTYPE)                                 # group storm database by event type
e0 <- summarize(g0, freq = n())                               # count the event types
e0 <- filter(e0, freq > 10)                                   # show events with a frequency larger than 10
print(e0[20:30,])                                             # display the entires 20 to 30
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
(Frequeny > 10 was only set to emphasise on the example, further analysis will consider all frequencies.)

There are still some entries with higer occurance which fall into separate categories (e. g. "FLASH FLOOD FLOOD"", "FLOOD FLASH FLOOD" which should be both in FLASH FLOOD) and would screw the analysis in a more or less significant way. We combine them into the official category name and this leads to the following 'fixes':

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

#### 2.3 Tidy data - columns PROPDMGEXP and CROPDMGEXP

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
Further they have numbers and question marks in them. To remove this entries we define a simple function 'multiplier' which sets all entries to the actual multiplier: H = 100 (hundreds), K = 1,000 (thousands), M = 1,000,000 (millions), B = 1,000,000,000 (billions), 1 will be the default multiplier for any other entries.

```r
multiplier <- function(x) { 
  m = 1
  if (x == "H") {
    m = 10
  }
  if (x == "K") {
    m = 10^3
  }
  if (x == "M") {
    m = 10^6
  }
  if (x == "B") {
    m = 10^9
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

At this stage we can now calculate the damage for the property and crop columns which we store in a new column called 'damage'. The total damage is the 

  **damage** = **property damage** * _mulitplier for property damage_** + **crop damage** * _multiplier for crop damage_**


```r
storm <- mutate(storm, damage = PROPDMG * mult_PD_EXP + CROPDMG * mult_CD_EXP)   # calculate total damage and create new column
```

### 3. Results

Now we can tackle the originally raised questions. First the most harmful events, let's repeat the question:

*Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?*

As we are looking at most harmful events with respect to population, the two columns FATALITIES and INJURIES are relevant. We will handle and display them separately.

#### 3.1 Injuries
First the total number of injuries per event type is calculated and aggregated below.

```r
g_i <- group_by(storm, EVTYPE)                                    # group by event type
inj_evt <- summarize(g_i, injuries = sum(INJURIES/1000))          # calculate the sum of injuries in thousands
inj_evt_high <- filter(inj_evt, injuries > 1)                     # filter only events with more than 1000 injuries
inj_evt_high$EVTYPE <- reorder(inj_evt_high$EVTYPE, -inj_evt_high$injuries)   # reorder the events by frequency
```
The following is a graphical representation of the total amount of inuries per weather event and shows that the category tornado is by far the one with highest injuries. After that in close succession follow the categories 'thunderstorm wind', flood, 'excessive heat' and lightning.

```r
cc <- length(inj_evt_high$EVTYPE)                                 # get the number of total event types
palette <- colorRampPalette(rev(brewer.pal(9, "Greys")))          # match the color palette grey shapes
                                                                  # display the data in a plot
ggplot(inj_evt_high) + geom_bar(stat="identity", aes(EVTYPE, injuries), fill=palette(cc)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Injuries Per Event Type\n") + xlab("Weather Event Type") + ylab("Injuries in thousands")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 

The actual figures are shown below where tornado as cause for injuries are standing out the most.

```r
head(arrange(inj_evt_high, desc(injuries)), 10)                   # list the events with highest injuries
```

```
## Source: local data frame [10 x 2]
## 
##               EVTYPE injuries
## 1            TORNADO   91.364
## 2  THUNDERSTORM WIND    9.495
## 3              FLOOD    6.791
## 4     EXCESSIVE HEAT    6.525
## 5          LIGHTNING    5.230
## 6               HEAT    2.479
## 7          ICE STORM    1.975
## 8        FLASH FLOOD    1.800
## 9           WILDFIRE    1.606
## 10         HIGH WIND    1.440
```

#### 3.2 Fatalities

Secondly the total number of fatalities per event type is calculated and aggregated below.

```r
g_f <- group_by(storm, EVTYPE)                                    # group by event type
fat_evt <- summarize(g_f, fatalities = sum(FATALITIES))           # calculate the sum of injuries
fat_evt_high <- filter(fat_evt, fatalities > 200)                 # filter only events with more than 200 fatalities
fat_evt_high$EVTYPE <- reorder(fat_evt_high$EVTYPE, -fat_evt_high$fatalities)   # reorder the events by frequency
```
The graph below shows the total amount of fatalities per weather event and depicts that again tornado is by far the category with the most fatalities. Then the category 'exessive heat' followed by heat, flash flood, lightning and 'thunderstorm wind' in fairly close succession.

```r
cc <- length(fat_evt_high$EVTYPE)                                 # get the number of total event types
palette <- colorRampPalette(rev(brewer.pal(9, "Greys")))          # match the color palette grey shapes
                                                                  # display the data in a plot
ggplot(fat_evt_high) + geom_bar(stat="identity", aes(EVTYPE, fatalities), fill=palette(cc)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Fatalities Per Event Type\n") + xlab("Weather Event Type") + ylab("Fatalities")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png) 

The actual figures are shown below where again tornado is the most deadliest cause for fatalities on weather events.

```r
head(arrange(fat_evt_high, desc(fatalities)), 10)                 # list the events with highest fatilities
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

#### 3.3 Most harmful economic events

The second question which we want to answer is as follows:

*Across the United States, which types of events have the greatest economic consequences?*

The storm database gives two columns 'PROPDMG' and 'CROPDMG' which we already combined in section 2.3 to a single column called 'damage'. 

When viewing this data and ordering by most significant damages it appears that one outlier was found which has a multiplier of 'B' (billion) in column PROPDMGEXP which must have been a mistake.


```r
options(scipen=999)                                              # don't use scientific notation
                                                                 # arrange by damage, decending
head(storm %>% select(REFNUM, PROPDMGEXP, CROPDMGEXP, damage) %>% arrange(desc(damage)), 10)
```

```
##    REFNUM PROPDMGEXP CROPDMGEXP       damage
## 1  605943          B          M 115032500000
## 2  577616          B             31300000000
## 3  577615          B             16930000000
## 4  581535          B             11260000000
## 5  198375          B          B  10000000000
## 6  569288          B             10000000000
## 7  581537          B          B   7390000000
## 8  581533          B              7350000000
## 9  529299          B          M   5705000000
## 10 444407          B              5150000000
```


According to the details in column REMARK the damage is in millions rather than billions. As this is a significant difference and changes the end amount a lot it will be manually fixed.


```r
subset(storm, REFNUM==605943, select=c(8,2,37,40,26,28))         # show the entry with wrong property multiplier
```

```
##        EVTYPE         BGN_DATE REFNUM       damage PROPDMGEXP CROPDMGEXP
## 605953  FLOOD 1/1/2006 0:00:00 605943 115032500000          B          M
```

```r
subset(storm, REFNUM==605943, select=c(37,36))                   # show the comment which mentions damages in millions rather than billions
```

```
##        REFNUM
## 605953 605943
##                                                                                                                                                                                                                                                                                                                                                                                               REMARKS
## 605953 Major flooding continued into the early hours of January 1st, before the Napa River finally fell below flood stage and the water receeded. Flooding was severe in Downtown Napa from the Napa Creek and the City and Parks Department was hit with $6 million in damage alone. The City of Napa had 600 homes with moderate damage, 150 damaged businesses with costs of at least $70 million.
```

```r
storm[storm$REFNUM==605943,40] <- 147500000                      # fix the data
```

We can now sum the damages and aggregate as follows.


```r
g_d <- group_by(storm, EVTYPE)                                   # group by event type
dam_evt <- summarize(g_d, damages = sum(damage/1000000000))      # sum all damages and convert to billions
dam_evt_high <- filter(dam_evt, damages > 5)                     # filter out events with damages below 5
dam_evt_high$EVTYPE <- reorder(dam_evt_high$EVTYPE, -dam_evt_high$damages)    # reorder the event type
```
It is assumed that the economic figures from this storm database are already adjusted for inflation and normalized.
Displaying the data in a graph clearly shows that category 'hurrican typhoon' is the most harmful economic event followed by tornado, 'storm surge' and flood.

```r
cc <- length(dam_evt_high$EVTYPE)                                # get the number of total event types
palette <- colorRampPalette(rev(brewer.pal(9, "Greys")))         # match the color palette grey shapes
                                                                 # display the data in a plot
ggplot(dam_evt_high) + geom_bar(stat="identity", aes(EVTYPE, damages), fill=palette(cc)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + ggtitle("Damages Per Event Type\n") + xlab("Weather Event Type") + ylab("Damages (Billion US$)")
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png) 

The actual data looks like this:

```r
head(arrange(dam_evt_high, desc(damages)), 10)                   # list the events with highest damage
```

```
## Source: local data frame [10 x 2]
## 
##               EVTYPE   damages
## 1  HURRICANE TYPHOON 90.271473
## 2            TORNADO 57.356892
## 3        STORM SURGE 47.965579
## 4              FLOOD 35.557839
## 5               HAIL 18.758863
## 6        FLASH FLOOD 18.163370
## 7            DROUGHT 15.018672
## 8  THUNDERSTORM WIND 11.015195
## 9        RIVER FLOOD 10.292723
## 10         ICE STORM  8.967041
```
It is to note from the detailed documents that major catastrophic events are actually not just counted in one category. For example hurricane 'Katrina' was counted in at least two categories as shown below. This is because events spread out over a couple of days and can cause follow on effects which are registered separately.

```r
subset(storm, REFNUM==577616 | REFNUM==577615, select=c(8,2,37,38,40))   # show the Katrina events is spread over two entries
```

```
##                   EVTYPE          BGN_DATE REFNUM mult_PD_EXP      damage
## 577675 HURRICANE TYPHOON 8/28/2005 0:00:00 577615  1000000000 16930000000
## 577676       STORM SURGE 8/29/2005 0:00:00 577616  1000000000 31300000000
```


