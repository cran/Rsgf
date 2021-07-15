Rsgf 1.0.0
==========

R package that imports SGF (Smart Game File).  Used for Go/Baduk and other board games (emphasis on Go/Baduk).

# Installation #

## Production/CRAN install ##

This package is available in [CRAN](https://bcable.net/x/Rsgf/CRAN).

```
install.packages("Rsgf")
```

## Development/GIT Install ##

To install the development or GIT repository version, this requires the "devtools" package available in [CRAN](https://cran.r-project.org/package=devtools).

### Install devtools ###

Assuming you don't already have devtools installed, run the following:

```
install.packages("devtools")
```

### Install Rsgf ###

With devtools installed, it's fairly simple to install the development branch:

```
library(devtools)
install_git("https://gitlab.com/BCable/Rsgf.git")
```

# Examples #

Simple example of Player Card function:

```r
library(Rsgf)

player_data <- Rsgf::playerCard("Honinbo Shusaku", "~/GoGoDSpring2018/1850-99")
```

Simple aggregation for heatmap:

```r
agg_games <- aggregate(Game.Number ~ x + y, data=player_data, FUN=length)
names(agg_games) <- c("x", "y", "Count")
agg_games$Count <- as.factor(agg_games$Count)
```

Graph heatmap:

```r
Rsgf::boardHeatMap(agg_games)
```

Extended Player Card and Heatmap Examples:

https://bcable.net/analysis-Rsgf_period_cards.html

https://bcable.net/analysis-Rsgf_moves_anim.html

https://bcable.net/analysis-Rsgf_player_card.html
