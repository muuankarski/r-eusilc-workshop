r.eusilc-package
==============
css: /home/aurelius/workspace/template/rpresentation/slides.css
transition: fade
transition-speed: fast
*Merge eu-silc cross-sectional and longitudinal raw .csv datasets*

<a href="http://markuskainu.fi">Markus Kainu</a></br>
*PhD student* </br>


<div class="github-fork-ribbon-wrapper right">
<div class="github-fork-ribbon">
<a href="https://github.com/muuankarski/" style="color:white;">Fork me on GitHub</a>
</div>
</div>

What & why
========================================================

Package that does several labour intensive and error prone tasks

- merges data- and register files both at the
    - personal and
    - household level
- and dies that both for 
    - longitudinal and 
    - cross-sectional files

Requirements
=================================================



merge-commands
=================================================

Simple read of personal files from longitudinal data
------------------------------------------------


```r
per_long <- merge_longi_personal(origin.path="/media/baikal/Asiakirjat/data/eu_silc/2011/longi_rev0",
                                 destination.path="~/demo",
                                 format="RData")
```


Simple read of personal files from cross-sectional data
------------------------------------------------


```r
hh_cross <- merge_cross_household(origin.path="/media/baikal/Asiakirjat/data/eu_silc/2011/longi_rev0",
                                 destination.path="~/demo",
                                 format="RData")
```

