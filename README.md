# WFDItalia

# Overview
A collection of functions for calculating the italian STAR_ICMi index based on macroinvertebrate community.

# Installation

```R
# install the devtools package and then
library(devtools)

install_github("alexology/WFDItalia")
```
# Usage

```R
# Prepare data for the analysis. You will asked to choose between surber and artificial substrate methods. For the dataset oglio we will choose surber
oglio.prep <- data.prep(oglio)

# calculate aspt 
aspt(oglio.prep)

# calculate STAR_ICMi index and its metrics. You will asked to choose the hydroecoregion, 
oglio.open <- macropen(oglio.prep)

# select the results for further analysis
oglio.star <- oglio.open[[2]]

# calculate STAR_ICMi index and export an excel file of the results
oglio.open <- macropen(oglio.prep, writexlsx=T)
```
