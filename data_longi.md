---
title: Poolatun Pitkittäisaineiston rakentaminen EU-silc datasta
author: Markus Kainu
date: 
---


`Last updated`:

```
## [1] "2014-02-18 08:52:37 EET"
```



Idea
===============================

The point of this tutorial is to present a simple reproducible example on how longitudinal analysis can be run in R-environment.

In this document I will focus on three things 

1. merging the raw .csv longitudinal files and pooling across waves
2. creating a descrete time dataset for further longitudinal analysis
3. running simple survival analysis on that data

In this playful demo I'm interested in retirement. But, first let's take a look at the mering the raw data.

Merging the raw data files and pooling across waves
===============================

Eu-silc datasets are delivered on a laser disc, where you have on each disc a four .csv files representing one either *cross-sectional* or *longitudinal* version of data. Raw .csv-files are named as `UDB_L06D_ver 2006-2 from 01-03-2009`, that in this case would mean:

- user database
- longitudinal
- d-file (household register)
- from year 2006 revision 2
- published year 2009

In order to follow this demo you will have to rename the .csv files following the example below:

- `UDB_L06R_ver 2006-2 from 01-03-2009.csv` =>> `r_file.csv`
- `UDB_L06P_ver 2006-2 from 01-03-2009.csv` =>> `p_file.csv`
- `UDB_L06D_ver 2006-2 from 01-03-2009.csv` =>> `d_file.csv`
- `UDB_L06H_ver 2006-2 from 01-03-2009.csv` =>> `h_file.csv`

in addition to renaming you should also consider structuring your raw data archive into something like (this is how my data is oranised for this demo):


```r

data
|---eusilc
|    |-----2008
|    |      |---longi_rev1
|    |      |    |-------- r_file.csv
|    |      |    |-------- p_file.csv
|    |      |    |-------- d_file.csv
|    |      |    |-------- h_file.csv
|    |      |---cross_rev2
|    |      |    |-------- r_file.csv
|    |      |    |-------- p_file.csv
|    |      |    |-------- d_file.csv
|    |      |    |-------- h_file.csv
|    |-----2009
|    |      |---longi_rev0
|    |      |    |-------- r_file.csv
|    |      |    |-------- p_file.csv
|    |      |    |-------- d_file.csv
|    |      |    |-------- h_file.csv
|    |      |---cross_rev1
|    |      |    |-------- r_file.csv
|    |      |    |-------- p_file.csv
|    |      |    |-------- d_file.csv
|    |      |    |-------- h_file.csv
|    |-----demo
|    |      |---2008
|    |      |     |----- per_longi.csv
|    |      |     |----- hh_longi.csv
|    |      |     |----- hh_cross.csv
|    |      |     |----- hh_cross.csv
|    |      |---2009
|    |      |     |----- per_longi.csv
|    |      |     |----- hh_longi.csv
|    |      |     |----- hh_cross.csv
|    |      |     |----- hh_cross.csv
|    |      |---2010
|    |      |     |----- per_longi.csv
|    |      |     |----- hh_longi.csv
|    |      |     |----- hh_cross.csv
|    |      |     |----- hh_cross.csv

```



Tässä esimerkissä raakadata vuoden 2010 pitkittäisaneistolle ovat muotoa: `~/data/eu_silc/2010/longi_rev1`, jolloin ko. hakemistossa olisi neljä .csv tiedostoa pitkittäisaineisosta vuoden 2010 revision 1.

Raakadatojen (paneeli) yhdistäminen
-----------------------------------

Nyt olemme kiinnostuneet vain yksilötason datoista, joten lataamme ensin kaikki 2010 ja aikaisemmin julkaistut yksilötason paneelidatat R:ään sekä tallennamme yhdistellyt data uudeen hakemistoon. Käytämme siinä apuna r.eusilc-pakettia.

**Installing r.eusilc-package**

For installation you need to have **devtools**-package installed with  `install.packages("devtools")`. In Windows you will need [RTools](http://cran.r-project.org/bin/windows/Rtools/index.html) to be installed before installing **devtools**-package.


```r
library(devtools)
install_github("r.eusilc", "muuankarski")
library(r.eusilc)
```



**Merging individual level longitudinal datasets**

We are using here only variables from personal register and personal data files. First we are merging the files from three waves 2008,2009 and 2010. Again, each panel data has rows from that year, and from three previous years (few countries have longer panels). Below, we subset a ~35 variables from each data from 16 countries.

The following lines will 1) read the raw data, 2) merge the personal level files and 3) save the ouput in `.csv`-format in `demo/year`-folder.

<!--
`data.table=TRUE` attribuutti käyttää [data.table](http://cran.r-project.org/web/packages/data.table/index.html)-paketin hieman eksperimentaalia `fread`-funktiota .csv-datojen lukemiseen, joka on huimasti `read.csv`-funktiota nopeampi. `data.table=TRUE` käyttö edellyttää pakettien **data.table** ja **bit64** asennusta. (Suositellaan vain kokeneille käyttäjille)
-->


```r
# valitaan muuttujat. var.list1 datas other than 2008. 
# (2008) does not have variable PL031
var.list1 <- c("PER_ID_Y", # unique year, cntry, personal ID
              "PER_ID", # unique cntry, personal ID
              "RB010",  # year
              "RB020",  # cntry
              "RB030",  # Personal ID
              "RB040",  # Current Household ID
              "RB060",  # Personal base weight
              "RB080",  # Year of birth
              "RB090",  # Sex
              "RB110",  # Membership status
              "RB210",  # Basic activity status
              "RB230",  # Mother ID
              "RX010",  # Age at the date of the interview
              "RX020",  # Age at the end of the income reference period
              # ----------- from personal data ----------------------
              "PB190",  # Marital status
              "PB200",  # Consensual union
              "PY140G", # Education-related allowances(gross)
              "PL030",  # Self-defined current economic status
              "PL031",  # Self-defined current economic status
              "PL040",  # Status in employment
              "PY010N", # Employee cash or near cash income(gross)
              "PY010G", # Employee cash or near cash income(gross)
              "PY140N", # Education-related allowances
              "PY100N", # Old-age benefits(net)
              "PL210A", # Main activity on January
              "PL210B", # Main activity on February
              "PL210C", # Main activity on March
              "PL210D", # Main activity on April
              "PL210E", # Main activity on May
              "PL210F", # Main activity on June
              "PL210G", # Main activity on July
              "PL210H", # Main activity on August
              "PL210I", # Main activity on September
              "PL210J", # Main activity on October
              "PL210K", # Main activity on November
              "PL210L"  # Main activity on December
              )

var.list2 <- c("PER_ID_Y", # unique year, cntry, personal ID
              "PER_ID", # unique cntry, personal ID
              "RB010",  # year
              "RB020",  # cntry
              "RB030",  # Personal ID
              "RB040",  # Current Household ID
              "RB060",  # Personal base weight
              "RB080",  # Year of birth
              "RB090",  # Sex
              "RB110",  # Membership status
              "RB210",  # Basic activity status
              "RB230",  # Mother ID
              "RX010",  # Age at the date of the interview
              "RX020",  # Age at the end of the income reference period
              # ----------- from personal data ----------------------
              "PB190",  # Marital status
              "PB200",  # Consensual union
              "PY140G", # Education-related allowances(gross)
              "PL030",  # Self-defined current economic status
              #"PL031",  # Self-defined current economic status
              "PL040",  # Status in employment
              "PY010N", # Employee cash or near cash income(gross)
              "PY010G", # Employee cash or near cash income(gross)
              "PY140N", # Education-related allowances
              "PY100N", # Old-age benefits(net)
              "PL210A", # Main activity on January
              "PL210B", # Main activity on February
              "PL210C", # Main activity on March
              "PL210D", # Main activity on April
              "PL210E", # Main activity on May
              "PL210F", # Main activity on June
              "PL210G", # Main activity on July
              "PL210H", # Main activity on August
              "PL210I", # Main activity on September
              "PL210J", # Main activity on October
              "PL210K", # Main activity on November
              "PL210L"  # Main activity on December
              )
# valitaan maat
country.list <- c("AT","BE","DK","ES",
                  "FI","FR","HU","IE","IS",
                  "IT","LU","NL","NO","PT",
                  "SE","UK")
library(r.eusilc)
per_longi_2008 <- merge_longi_personal(origin.path="~/data/eu_silc/2008/longi_rev3",
                                       destination.path="~/data/eu_silc/demo/2008",
                                       format="csv",
                                       data.table=TRUE,
                                       subset.vars=var.list2,
                                       subset.countries=country.list)
per_longi_2009 <- merge_longi_personal(origin.path="~/data/eu_silc/2009/longi_rev3",
                                       destination.path="~/data/eu_silc/demo/2009",
                                       format="csv",
                                       data.table=TRUE,
                                       subset.vars=var.list1,
                                       subset.countries=country.list)
per_longi_2010 <- merge_longi_personal(origin.path="~/data/eu_silc/2010/longi_rev2",
                                       destination.path="~/data/eu_silc/demo/2010",
                                       format="RData",
                                       data.table=csv,
                                       subset.vars=var.list1,
                                       subset.countries=country.list)
```



Pooling data from different waves
------------------------------------

The challenge in pooling the waves is that names or numbers of variables is not same across the waves. In the case of retirement we have work a bit on this. But to be on the clear the logic here is a minimal  example of how to this in the case where your variables of interest have **identical names** and **identical coding scheme** across waves.

**Example one: identical variable names across waves**


```r
# Define variables of interest
var.list <- c("PER_ID_Y", # unique year, cntry, personal ID
              "RB010",  # year
              "RB020" # country
              )
# subset only those variables from panel waves and rbind them on top of each other
dat <- rbind(per_longi_2008[,var.list],
             per_longi_2009[,var.list],
             per_longi_2010[,var.list])

# As there are panel continuing across files 
# you have to remove the duplicated country-year-id combinations
dat$dup <- duplicated(dat$PER_ID_Y)
dat.per <- dat[dat$dup == FALSE,]
dat.per$dup <- NULL
```



**A more complicated example with retirement**

Again, I'm interested in **retirement** in this demo and i will use the variable [`PL030/PL031`](http://www.gesis.org/?id=8063#PL030) (*Self-defined current economic status*) as my main source of information on the event (entry to pension from work). As those variables are "complementary"" (individuals have value in either of variable (missing data still substantial problem!!) a synthetic variable `econStatus` has to be created. In the following lines it done following a logic: 

1. a value from PL031 is taken first
2. if it is missing, the value from PL030 is taken
3. a output varible `econStatus` is simultaneuosly recoded to match PL031 coding. The results is a variable with values as in the table below.

Table: values and labels in variable PL031 AND in output var `econStatus`

| Value | Label |
| ----- | ------ |
| 1 | Employee working full-time |
| 2 | Employee working part-time |
| 3 | Self-employed working full-time (including family worker) |
| 4 | Self-employed working part-time (including family worker) |
| 5 | Unemployed |
| 6 | Pupil, student, further training, unpaid work experience |
| 7 | In retirement or in early retirement or has given up business |
| 8 | Permanently disabled or/and unfit to work |
| 9 | In compulsory military community or service |
| 10 | Fulfilling domestic tasks and care responsibilities |
| 11 | Other inactive person |

In other words, entry to pension system is coded as change from `econStatus != 7` to `econStatus == 7`.


```r
dfX$econStatus <- ifelse(is.na(dfX$PL031), dfX$PL030rec, dfX$PL031)
```


1. merging the original register and data files both at the household and personal level from year 2008, 2009 and 2010 (each containing data from that year and three years before)
2. subsetting same variables from each datasets 
    - same set of variables from personal08, personal09 and personal10 datas
3. combining the datasets with identical variables
    - one dataset including all the cases from personal files (~4 million cases)
    - another dataset including all the cases from household files (~2 million cases)
4. subsetting only cases with unique `year-cntry-id` -combination (longitudinal data files are overlapping)
5. merging the needed household level variables with personal data
5. manipulating the dataset so that the year of exit/entry is found plus the required information from years before and after the event



```r

load("~/data/eu_silc/demo/2008/per_merge_longi.RData")
per_longi_2008 <- per_merge_longi
load("~/data/eu_silc/demo/2009/per_merge_longi.RData")
per_longi_2009 <- per_merge_longi
load("~/data/eu_silc/demo/2010/per_merge_longi.RData")
per_longi_2010 <- per_merge_longi
rm(per_merge_longi)
# A joint variable of var (PL030/PL031",  # Self-defined current economic status)
# have to constructed before subsetting the data
#
# let's code var econStatus by taking the PL031 as a bechmark
# before making any corrections let's recode var PL030 using 
# PL031 classes into var PL030rec
# 2008
per_longi_2008$PL030rec[per_longi_2008$PL030 == 1] <- 1
per_longi_2008$PL030rec[per_longi_2008$PL030 == 2] <- 2
per_longi_2008$PL030rec[per_longi_2008$PL030 == 3] <- 5
per_longi_2008$PL030rec[per_longi_2008$PL030 == 4] <- 6
per_longi_2008$PL030rec[per_longi_2008$PL030 == 5] <- 7
per_longi_2008$PL030rec[per_longi_2008$PL030 == 6] <- 8
per_longi_2008$PL030rec[per_longi_2008$PL030 == 7] <- 9
per_longi_2008$PL030rec[per_longi_2008$PL030 == 8] <- 10
per_longi_2008$PL030rec[per_longi_2008$PL030 == 9] <- 11
# 2009
per_longi_2009$PL030rec[per_longi_2009$PL030 == 1] <- 1
per_longi_2009$PL030rec[per_longi_2009$PL030 == 2] <- 2
per_longi_2009$PL030rec[per_longi_2009$PL030 == 3] <- 5
per_longi_2009$PL030rec[per_longi_2009$PL030 == 4] <- 6
per_longi_2009$PL030rec[per_longi_2009$PL030 == 5] <- 7
per_longi_2009$PL030rec[per_longi_2009$PL030 == 6] <- 8
per_longi_2009$PL030rec[per_longi_2009$PL030 == 7] <- 9
per_longi_2009$PL030rec[per_longi_2009$PL030 == 8] <- 10
per_longi_2009$PL030rec[per_longi_2009$PL030 == 9] <- 11
# 2010
per_longi_2010$PL030rec[per_longi_2010$PL030 == 1] <- 1
per_longi_2010$PL030rec[per_longi_2010$PL030 == 2] <- 2
per_longi_2010$PL030rec[per_longi_2010$PL030 == 3] <- 5
per_longi_2010$PL030rec[per_longi_2010$PL030 == 4] <- 6
per_longi_2010$PL030rec[per_longi_2010$PL030 == 5] <- 7
per_longi_2010$PL030rec[per_longi_2010$PL030 == 6] <- 8
per_longi_2010$PL030rec[per_longi_2010$PL030 == 7] <- 9
per_longi_2010$PL030rec[per_longi_2010$PL030 == 8] <- 10
per_longi_2010$PL030rec[per_longi_2010$PL030 == 9] <- 11

# then we have to fill in missing values in PL031 with
# values in PL030rec
# let's create a PL031 with only NA values
per_longi_2008$PL031 <- NA

per_longi_2008$econStatus <- ifelse(is.na(per_longi_2008$PL031),  
                                    per_longi_2008$PL030rec, per_longi_2008$PL031)
per_longi_2009$econStatus <- ifelse(is.na(per_longi_2009$PL031),  
                                    per_longi_2009$PL030rec, per_longi_2009$PL031)
per_longi_2010$econStatus <- ifelse(is.na(per_longi_2010$PL031),  
                                    per_longi_2010$PL030rec, per_longi_2010$PL031)

# list of variables to be analysed
var.list <- c("PER_ID_Y", # unique year, cntry, personal ID
              "PER_ID", # unique cntry, personal ID
              "RB010",  # year
              "RB020",  # cntry
              "RB030",  # Personal ID
              "RB040",  # Current Household ID
              "RB060",  # Personal base weight
              "RB080",  # Year of birth
              "RB090",  # Sex
              "RB110",  # Membership status
              "RB210",  # Basic activity status
              "RB230",  # Mother ID
              "RX010",  # Age at the date of the interview
              "RX020",  # Age at the end of the income reference period
              # ----------- from personal data ----------------------
              "PB190",  # Marital status
              "PB200",  # Consensual union
              "econStatus",  # Self-defined current economic status
              "PY140G", # Education-related allowances(gross)
              "PL040",  # Status in employment
              "PY010N", # Employee cash or near cash income(gross)
              "PY010G", # Employee cash or near cash income(gross)
              "PY140N", # Education-related allowances
              "PY100N", # Old-age benefits(net)
              "PL210A", # Main activity on January
              "PL210B", # Main activity on February
              "PL210C", # Main activity on March
              "PL210D", # Main activity on April
              "PL210E", # Main activity on May
              "PL210F", # Main activity on June
              "PL210G", # Main activity on July
              "PL210H", # Main activity on August
              "PL210I", # Main activity on September
              "PL210J", # Main activity on October
              "PL210K", # Main activity on November
              "PL210L"  # Main activity on December
)

dat <- rbind(per_longi_2008[,var.list],
             per_longi_2009[,var.list],
             per_longi_2010[,var.list])
```

```
## Error: cannot allocate vector of size 8.9 Mb
```

```r
rm(list=setdiff(ls(), "dat"))
# merge the panels datas
# you have to remove the duplicated country-year-id combinations
dat$dup <- duplicated(dat$PER_ID_Y)
```

```
## Error: object 'dat' not found
```

```r
dat.per <- dat[dat$dup == FALSE,]
```

```
## Error: object 'dat' not found
```

```r
dat.per$dup <- NULL
```

```
## Error: object 'dat.per' not found
```

```r

# because of data.table -package
dat.per$RB030 <- as.numeric(dat.per$RB030)
```

```
## Error: object 'dat.per' not found
```

```r

```


### Analyysejä


```r
tbl <- as.data.frame(table(dat.per$RB010, dat.per$RB020, dat.per$econStatus))
```

```
## Error: object 'dat.per' not found
```

```r
library(reshape2)
tbl.w <- dcast(data = tbl, Var1 + Var2 ~ Var3, value.var = "Freq")
```

```
## Error: object 'tbl' not found
```

```r
tbl.w$sum <- rowSums(tbl.w[, 3:13])
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 3] <- tbl.w[, 3]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 4] <- tbl.w[, 4]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 5] <- tbl.w[, 5]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 6] <- tbl.w[, 6]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 7] <- tbl.w[, 7]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 8] <- tbl.w[, 8]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 9] <- tbl.w[, 9]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 10] <- tbl.w[, 10]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 11] <- tbl.w[, 11]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 12] <- tbl.w[, 12]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w[, 13] <- tbl.w[, 13]/tbl.w[, 14] * 100
```

```
## Error: object 'tbl.w' not found
```

```r
tbl.w$sum <- NULL
```

```
## Error: object 'tbl.w' not found
```

```r
tbl <- melt(tbl.w, id.vars = c("Var1", "Var2"))
```

```
## Error: object 'tbl.w' not found
```

```r

library(ggplot2)
ggplot(tbl, aes(x = Var1, y = value, fill = variable)) + geom_bar(stat = "identity") + 
    facet_wrap(~Var2)
```

```
## Error: object 'tbl' not found
```



Tiettyjen tapahtumien eristäminen ja diskreetin datan luominen
===============================

Case: Entry to pension
------------------------------------------


```r
library(reshape2)
dat.per$RB010X <- factor(paste("x",dat.per$RB010,sep=""))
```

```
## Error: object 'dat.per' not found
```

```r
df.wide <- dcast(dat.per, PER_ID + RB020 + RB030 + 
                  #HX100 + HX090 +
                  PY010G ~ RB010X, 
                 value.var = "econStatus")
```

```
## Error: object 'dat.per' not found
```

```r
# Transitions per year
df.wide$pension06[df.wide$x2005 != 7 & df.wide$x2006 == 7] <- 1
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension06[is.na(df.wide$pension06)] <- 0
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension07[df.wide$x2006 != 7 & df.wide$x2007 == 7] <- 1
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension07[is.na(df.wide$pension07)] <- 0
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension08[df.wide$x2007 != 7 & df.wide$x2008 == 7] <- 1
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension08[is.na(df.wide$pension08)] <- 0
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension09[df.wide$x2008 != 7 & df.wide$x2009 == 7] <- 1
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension09[is.na(df.wide$pension09)] <- 0
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension10[df.wide$x2009 != 7 & df.wide$x2010 == 7] <- 1
```

```
## Error: object 'df.wide' not found
```

```r
df.wide$pension10[is.na(df.wide$pension10)] <- 0
```

```
## Error: object 'df.wide' not found
```

```r
# duplicates?
df.wide$dup <- duplicated(df.wide$PER_ID)
```

```
## Error: object 'df.wide' not found
```

```r
dat.uniq <- df.wide[df.wide$dup == FALSE,]
```

```
## Error: object 'df.wide' not found
```

```r
dat.uniq$dup <- NULL
```

```
## Error: object 'dat.uniq' not found
```

```r
df.wide <- dat.uniq
```

```
## Error: object 'dat.uniq' not found
```

```r

df.long <- melt(df.wide, id.vars =c("PER_ID","RB020","RB030",
                                    #"HX100","HX090",
                                    "PY010G"),
                measure.vars=c("pension06","pension07",
                               "pension08","pension09",
                               "pension10"))
```

```
## Error: object 'df.wide' not found
```

```r
names(df.long) <- c("PER_ID","RB020","RB030",#"HX100","HX090",
                    "PY010G_Z",
                    "year","pensionEnter")
```

```
## Error: object 'df.long' not found
```

```r

df.long$year <- as.character(df.long$year)
```

```
## Error: object 'df.long' not found
```

```r

df.long$year[df.long$year == "pension06"] <- "2006"
```

```
## Error: object 'df.long' not found
```

```r
df.long$year[df.long$year == "pension07"] <- "2007"
```

```
## Error: object 'df.long' not found
```

```r
df.long$year[df.long$year == "pension08"] <- "2008"
```

```
## Error: object 'df.long' not found
```

```r
df.long$year[df.long$year == "pension09"] <- "2009"
```

```
## Error: object 'df.long' not found
```

```r
df.long$year[df.long$year == "pension10"] <- "2010"
```

```
## Error: object 'df.long' not found
```

```r


df.long$year <- factor(df.long$year)
```

```
## Error: object 'df.long' not found
```

```r
df.long$year <- as.numeric(levels(df.long$year))[df.long$year]
```

```
## Error: object 'df.long' not found
```

```r

df.longx <- df.long[df.long$pensionEnter == 1, ] # all we need are the new pensioners
```

```
## Error: object 'df.long' not found
```

```r

datEnterPension <- merge(df.longx[,c("RB020","RB030","year","pensionEnter")],
                          dat.per, 
                          by=c("RB020","RB030"), 
                          all.x=TRUE)
```

```
## Error: object 'df.longx' not found
```

```r
datEnterPension$time <- datEnterPension$RB010 - datEnterPension$year
```

```
## Error: object 'datEnterPension' not found
```

```r
save(datEnterPension, file="data/datEnterPension.RData")
```

```
## Error: object 'datEnterPension' not found
```




**Number of events by year & country**



```r
datTp <- datEnterPension[datEnterPension$year == datEnterPension$RB010, ]
```

```
## Error: object 'datEnterPension' not found
```

```r
tblp <- as.data.frame(table(datTp$RB020, datTp$year))
```

```
## Error: object 'datTp' not found
```

```r

library(ggplot2)

ggplot(data = tblp, aes(x = factor(Var2), y = Freq, label = Freq)) + geom_bar(stat = "identity", 
    position = "dodge") + geom_text(vjust = -0.2) + labs(title = "Number of individuals entering to pension") + 
    facet_wrap(~Var1, ncol = 4)
```

```
## Error: object 'tblp' not found
```



**Distribution of age of individuals by year & country**



```r
library(ggplot2)
ggplot(data = datTp, aes(x = factor(RB010), y = RX010)) + geom_boxplot() + labs(title = "Age of retirees") + 
    facet_wrap(~RB020, ncol = 4)
```

```
## Error: object 'datTp' not found
```



**Distribution of age of individuals by country**


```r
library(RCurl)
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/europe/eu_cntry.csv")
regime <- read.csv(text = GHurl)

library(ggplot2)
datTp <- merge(datTp, regime, by.x = "RB020", by.y = "NUTS_ID")
```

```
## Error: object 'datTp' not found
```

```r
ggplot(data = datTp, aes(x = factor(reorder(RB020, RX010, median, na.rm = TRUE)), 
    y = RX010, fill = regime_en)) + geom_boxplot() + labs(title = "Age of retirees") + 
    theme(legend.position = "top")
```

```
## Error: object 'datTp' not found
```




**Distribution of absolute employee cash income (gross) before and after the event**


```r
library(ggplot2)
ggplot(data = datEnterPension, aes(x = factor(time), y = PY010G)) + geom_boxplot() + 
    labs(title = "Distribution of absolute employee cash income (gross) before and after retirement") + 
    coord_cartesian(ylim = c(0, 50000))
```

```
## Error: object 'datEnterPension' not found
```




**Change in absolute employee cash income (gross) due to event (years before and after)**


```r
library(ggplot2)
ggplot(datEnterPension, aes(x = factor(time), y = PY010G, group = PER_ID)) + 
    geom_point(alpha = 0.2) + geom_line(alpha = 0.2) + coord_cartesian(ylim = c(0, 
    50000)) + facet_wrap(~RB020, ncol = 2) + labs(title = "Change in absolute Employee cash or near cash income(gross) when retiring") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + geom_vline(xintercept = 6, 
    color = "orange", type = "dashed")
```

```
## Error: object 'datEnterPension' not found
```



Statistical analysis
===============================






-----------
-----------


**Some mess here**

**household data*


```r


# Load the merged household level panel datasets
load("~/workspace/data/eu_silc/2008/longi_rev3/hh_merge.RData")
hh_merge08 <- hh_merge
load("~/workspace/data/eu_silc/2009/longi_rev3/hh_merge.RData")
hh_merge09 <- hh_merge
load("~/workspace/data/eu_silc/2010/longi_rev2/hh_merge.RData")
hh_merge10 <- hh_merge
load("~/workspace/data/eu_silc/2011/longi_rev0/hh_merge.RData")
hh_merge11 <- hh_merge
rm(hh_merge)


# list of variables to be analysed
var.list <- c("HH_ID_Y", # unique year, cntry, personal ID
              "HH_ID", # unique cntry, personal ID
              "DB010",  # year
              "DB020",  # cntry
              "DB030",  # household ID
              "DB110",  # Household status
              "HY020",  # Total disposable income
              "HX050",  # Equivivalized household size
              "HX090",  # Equivivalized household income
              "HX100",  # Income quintiles
              "HY050N") # Family/Children related allowances (net)

dat <- rbind(hh_merge08[,var.list],
             hh_merge09[,var.list],
             hh_merge10[,var.list])

dat$dup <- duplicated(dat$HH_ID)
dat.uniq <- dat[dat$dup == FALSE,]
dat.uniq$dup <- NULL
dat.hh <- dat.uniq
dat.hh <- dat.hh[!is.na(dat.hh$HX050), ]
## Merge some files from household data
dat.per <- merge(dat.per,dat.hh, 
                 by.x=c("RB010","RB020","RB040"),
                 by.y=c("DB010","DB020","DB030"),
                 all.x=TRUE)
save(dat.per, file="data/dat.per.RData")

```


