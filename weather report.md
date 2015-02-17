---
title: "Weather Event Analysis"
author: "André Treffeisen"
date: "Tuesday, February 17, 2015"
output: html_document
---


```r
con <- bzfile("repdata_data_StormData.csv.bz2", open = "rb")
storm <- read.csv(con)                     # read the data from file 'repdata_data_StormData.csv'
```

```
## Error in pushBack(c(lines, lines), file, encoding = pbEncoding): can only push back on text-mode connections
```

```r
close(con)
```



```r
dim(storm)
```

```
## [1] 902297     37
```

