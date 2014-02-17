---
title: Poolatun Pitkittäisaineiston rakentaminen EU-silc datasta
author: Markus Kainu
date: Feb 17, 2014
---


Ajatus
===============================

Pitkittäiaineiston rakentamisen sekä pitkittäisanalyysien tekeminen jaetaan tässä kolmeen vaiheeseen:

1. raakadatojen yhdistäminen ja poolaaminen eri vuosilta
2. tiettyjen tapahtumien eristäminen ja diskreetin datan luominen
3. tilastolliset pitkittäisanalyysit

Ensimmäisen vaiheen raakadatojen yhdistäminen on automatisoitu [r.eusilc](https://github.com/muuankarski/r.eusilc)-paketiksi. Datojen poolaaminen vaati jo käsityötä, sillä eri vuosien datat eroavat toisistaan muuttujien osalta.

Toinen vaihe on työläin ja riippuu paljon tapahtumasta, jota tarkastellaan. Mä kirjoittaisin tätä maanantaina valmiiksi jos Henna voit kertoa tarkemmin mistä tapahtumasta on kyse ja millä muuttuj(a/i)lla olet sitä aikonut tutkia. Sekä taustamuuttujat ja vastemuuttujat kiinnostaisi. Kirjoittaisin nämä skriptit + dokumentaation mahdollisimman pitkälle, jotta voitaisiin tiistaina käyttään enemmän aikaa siihen miksi näin tehdään kuin siihen mitä tehdään.

Teen kaiken vaan R:llä koska osaan sen ja koska R-komentoja on mahdollista ajaa suoraan SPSS:stä (tuore ohje 2013 lokakuulta: http://www.ibm.com/developerworks/library/ba-call-r-spss/) tai upottaa Statan ja SAS:in työvirtoihin.

Kolmanteen vaiheeseen mua kiinnostaa se, mitä pitkittäisanalyysejä olet ajatellut tehdä ja millä ohjelmalla. Mulla on valmiina jotain survival analyysejä and sequence analyysejä, ja voin yrittää soveltaa niitä sun tapaukseen ihan esimerkkinä jos ehdin. 


Raakadatojen (paneeli) yhdistäminen ja poolaaminen eri vuosilta 
===============================


Eu-silc datat toimitetaan laser-levyllä, joissa yhdellä levyllä on **yhden vuoden** ja joko *cross-sectional* tai *longitudinal* -versio datasta. .csv-muotoiset data on nimetty tyyliin `UDB_L06D_ver 2006-2 from 01-03-2009`, joka tässä tapauksessa tarkoittaa

- user database
- longitudinal
- d-file (household register)
- vuoden 2006 datan revision 2
- julkaistu vuonna 2009

Jotta näiden ohjeiden noudattaminen onnistuu, tulee raakadatojen nimiä muuttaa siten, että:

- `UDB_L06R_ver 2006-2 from 01-03-2009.csv` =>> `r_file.csv`
- `UDB_L06P_ver 2006-2 from 01-03-2009.csv` =>> `p_file.csv`
- `UDB_L06D_ver 2006-2 from 01-03-2009.csv` =>> `d_file.csv`
- `UDB_L06H_ver 2006-2 from 01-03-2009.csv` =>> `h_file.csv`

tämä nimimuutos siksi, että automatiointi on helpompi tehdä. Suosittelen sijoittamaan raakatiedostot esim. alla olevan tyyppiseen hakemistorakenteeseen:



```r

data
|---eusilc
|    | --- 2008
|    |      |-- longi_rev1
|    |      |    |-------- r_file.csv
|    |      |    |-------- p_file.csv
|    |      |    |-------- d_file.csv
|    |      |    |-------- h_file.csv
|    |      |-- cross_rev2
|    |      |    |-------- r_file.csv
|    |      |    |-------- p_file.csv
|    |      |    |-------- d_file.csv
|    |      |    |-------- h_file.csv
|    | --- 2009
|    |      |-- longi_rev0
|    |      |    |-------- r_file.csv
|    |      |    |-------- p_file.csv
|    |      |    |-------- d_file.csv
|    |      |    |-------- h_file.csv
|    |      |-- cross_rev1
|    |      |    |-------- r_file.csv
|    |      |    |-------- p_file.csv
|    |      |    |-------- d_file.csv
|    |      |    |-------- h_file.csv
|    | --- merged
|    |      |-- 2008
|    |      |     |----- per_longi.csv
|    |      |     |----- hh_longi.csv
|    |      |     |----- hh_cross.csv
|    |      |     |----- hh_cross.csv
|    |      |-- 2009
|    |      |     |----- per_longi.csv
|    |      |     |----- hh_longi.csv
|    |      |     |----- hh_cross.csv
|    |      |     |----- hh_cross.csv
|    |      |-- 2010
|    |      |     |----- per_longi.csv
|    |      |     |----- hh_longi.csv
|    |      |     |----- hh_cross.csv
|    |      |     |----- hh_cross.csv

```


Tässä esimerkissä raakadata vuoden 2010 pitkittäisaneistolle ovat muotoa: `~/data/eu_silc/2010/longi_rev1`, jolloin ko. hakemistossa olisi neljä .csv tiedostoa pitkittäisaineisosta vuoden 2010 revision 1.

Raakadatojen (paneeli) yhdistäminen
-----------------------------------

Nyt olemme kiinnostuneet vain yksilötason datoista, joten lataamme ensin kaikki 2010 ja aikaisemmin julkaistut yksilötason paneelidatat R:ään sekä tallennamme yhdistellyt data uudeen hakemistoon. Käytämme siinä apuna r.eusilc-pakettia.

### Paketin asentaminen


```r
library(devtools)
install_github("r.eusilc","muuankarski")
library(r.eusilc)
```


### Yksilötason raakadatojen yhdistäminen

Seuraava rivit lukevat raakadatat, yhdistävät sen ja kirjoittavat .csv-muotoon demo-kansioon. `data.table=TRUE` attribuutti käyttää [data.table](http://cran.r-project.org/web/packages/data.table/index.html)-paketin hieman eksperimentaalia `fread`-funktiota .csv-datojen lukemiseen, joka on huimasti `read.csv`-funktiota nopeampi. `data.table=TRUE` käyttö edellyttää pakettien **data.table** ja **bit64** asennusta.



```r
# valitaan muuttujat
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

```
## Error: object 'csv' not found
```



Eri vuosien datojen yhdistäminen
------------------------------------

Mikäli 


### Helppo esimerkki


```r
var.list <- c("PER_ID_Y", # unique year, cntry, personal ID
              "RB010",  # year
              "RB020" # country
              )

dat <- rbind(per_longi_2008[,var.list],
             per_longi_2009[,var.list],
             per_longi_2010[,var.list])

# merge the panels datas
dat$dup <- duplicated(dat$PER_ID_Y)
dat.uniq <- dat[dat$dup == FALSE,]
dat.uniq$dup <- NULL
dat.per <- dat.uniq
```





### Monimutkaisempi esimerkki



```r

# load("~/data/eu_silc/demo/2008/per_merge_longi.RData")
# load("~/data/eu_silc/demo/2009/per_merge_longi.RData")
# load("~/data/eu_silc/demo/2010/per_merge_longi.RData")

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

# merge the panels datas
dat$dup <- duplicated(dat$PER_ID_Y)
dat.uniq <- dat[dat$dup == FALSE,]
dat.uniq$dup <- NULL
dat.per <- dat.uniq
dat.per$RB030 <- as.numeric(dat.per$RB030)

```


### Analyysejä


```r
tbl <- as.data.frame(round(prop.table(table(dat.per$RB010,dat.per$RB020,dat.per$econStatus),2),4)*100)
library(ggplot2)
ggplot(tbl, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(stat="identity")
```

![plot of chunk plot1](figure/plot1.png) 




Tiettyjen tapahtumien eristäminen ja diskreetin datan luominen
===============================

Case: Entry to pension
------------------------------------------


```r
library(reshape2)
dat.per$RB010X <- factor(paste("x",dat.per$RB010,sep=""))
df.wide <- dcast(dat.per, PER_ID + RB020 + RB030 + 
                  #HX100 + HX090 +
                  PY010G ~ RB010X, 
                 value.var = "econStatus")
# Transitions per year
df.wide$pension06[df.wide$x2005 != 7 & df.wide$x2006 == 7] <- 1
df.wide$pension06[is.na(df.wide$pension06)] <- 0
df.wide$pension07[df.wide$x2006 != 7 & df.wide$x2007 == 7] <- 1
df.wide$pension07[is.na(df.wide$pension07)] <- 0
df.wide$pension08[df.wide$x2007 != 7 & df.wide$x2008 == 7] <- 1
df.wide$pension08[is.na(df.wide$pension08)] <- 0
df.wide$pension09[df.wide$x2008 != 7 & df.wide$x2009 == 7] <- 1
df.wide$pension09[is.na(df.wide$pension09)] <- 0
df.wide$pension10[df.wide$x2009 != 7 & df.wide$x2010 == 7] <- 1
df.wide$pension10[is.na(df.wide$pension10)] <- 0
# duplicates?
df.wide$dup <- duplicated(df.wide$PER_ID)
dat.uniq <- df.wide[df.wide$dup == FALSE,]
dat.uniq$dup <- NULL
df.wide <- dat.uniq

df.long <- melt(df.wide, id.vars =c("PER_ID","RB020","RB030",
                                    #"HX100","HX090",
                                    "PY010G"),
                measure.vars=c("pension06","pension07",
                               "pension08","pension09",
                               "pension10"))
names(df.long) <- c("PER_ID","RB020","RB030",#"HX100","HX090",
                    "PY010G_Z",
                    "year","pensionEnter")

df.long$year <- as.character(df.long$year)

df.long$year[df.long$year == "pension06"] <- "2006"
df.long$year[df.long$year == "pension07"] <- "2007"
df.long$year[df.long$year == "pension08"] <- "2008"
df.long$year[df.long$year == "pension09"] <- "2009"
df.long$year[df.long$year == "pension10"] <- "2010"


df.long$year <- factor(df.long$year)
df.long$year <- as.numeric(levels(df.long$year))[df.long$year]

df.longx <- df.long[df.long$pensionEnter == 1, ] # all we need are the new pensioners

datEnterPension <- merge(df.longx[,c("RB020","RB030","year","pensionEnter")],
                          dat.per, 
                          by=c("RB020","RB030"), 
                          all.x=TRUE)
datEnterPension$time <- datEnterPension$RB010 - datEnterPension$year
save(datEnterPension, file="data/datEnterPension.RData")
```




**Number of events by year & country**



```r
datTp <- datEnterPension[datEnterPension$year==datEnterPension$RB010,]
tblp <- as.data.frame(table(datTp$RB020,datTp$year))

library(ggplot2)

ggplot(data=tblp, aes(x=factor(Var2),y=Freq,label=Freq)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_text(vjust=-0.2) + 
  labs(title="Number of individuals entering to pension") + 
  facet_wrap(~Var1, ncol=4)
```

![plot of chunk subsetenterpensionhistogram](figure/subsetenterpensionhistogram.png) 



**Distribution of age of individuals by year & country**



```r
library(ggplot2)
ggplot(data=datTp, 
       aes(x=factor(RB010),y=RX010)) + 
  geom_boxplot() +
  labs(title="Age of retirees") +
  facet_wrap(~RB020, ncol=4)
```

![plot of chunk subsetfigpension1](figure/subsetfigpension1.png) 



**Distribution of age of individuals by country**


```r
library(RCurl)
GHurl <- getURL("https://raw.github.com/muuankarski/data/master/europe/eu_cntry.csv")
regime <- read.csv(text = GHurl)

library(ggplot2)
datTp <- merge(datTp,regime,by.x="RB020",by.y="NUTS_ID")
ggplot(data=datTp, 
       aes(x=factor(reorder(RB020, RX010, median, na.rm=TRUE)),
           y=RX010,
           fill=regime_en)) + 
  geom_boxplot() +
  labs(title="Age of retirees") +
  theme(legend.position="top")
```

![plot of chunk subsetfigpension2](figure/subsetfigpension2.png) 




**Distribution of absolute employee cash income (gross) before and after the event**


```r
library(ggplot2)
ggplot(data=datEnterPension, 
       aes(x=factor(time),y=PY010G)) + 
  geom_boxplot() +
  labs(title="Distribution of absolute employee cash income (gross) before and after retirement") +
  coord_cartesian(ylim=c(0,50000))
```

![plot of chunk subsetpensionplotAbsobox](figure/subsetpensionplotAbsobox.png) 




**Change in absolute employee cash income (gross) due to event (years before and after)**


```r
library(ggplot2)
ggplot(datEnterPension, aes(x=factor(time),y=PY010G,group=PER_ID)) +
  geom_point(alpha=.2) + geom_line(alpha=.2) +
  coord_cartesian(ylim=c(0,50000)) +
  facet_wrap(~RB020, ncol=2) +
  labs(title="Change in absolute Employee cash or near cash income(gross) when retiring") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_vline(xintercept = 6, color="orange", type="dashed")
```

![plot of chunk subsetpensionplotAbso](figure/subsetpensionplotAbso.png) 



Tilastolliset pitkittäisanalyysit 
===============================







Sotkua..
================================

household data
----------------------


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


