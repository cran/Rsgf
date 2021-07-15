\name{Rsgf}
\alias{Rsgf}
\alias{Rsgf::loadSGF}
\alias{Rsgf::playerCard}
\alias{Rsgf::boardHeatMap}
\alias{Rsgf::getColorPalette}
\alias{Rsgf::SGFGame}
\alias{loadSGF}
\alias{playerCard}
\alias{boardHeatMap}
\alias{getColorPalette}
\alias{SGFGame}
\title{R package that imports SGF (Smart Game File).  Used for Go and other board games}
\description{Imports SGF (Smart Game File)}
\usage{
loadSGF(filename, game.number=1)
playerCard(player_names, sgf_paths)
boardHeatMap(data)
getColorPalette(count)
SGFGame(moves)
}
\arguments{
\item{filename}{Path to SGF filename}
\item{game.number}{Value to return for Game.Number, useful for multiple game loading into the same data.frame}
\item{player_names}{Player name (or player names as vector) to return results for}
\item{sgf_paths}{Path to SGFs or directories that contain SGFs (or paths as vector)}
\item{data}{Aggregate data.frame with X, Y, and Count columns representing the Go/Baduk board}
\item{moves}{moves returned by playerCard()}
\item{count}{count of graded levels for getColorPalette()}
}
\value{
loadSGF() returns a data.frame
playerCard() returns a data.frame
boardHeatMap() returns a ggplot2 graphics object
getColorPalette() returns a list of colors based on the count provided for the default Rsgf Go theme
SGFGame() returns an RC/S4 object
}
\author{Brad Cable}
\examples{
# loading lots of SGFs
library(Rsgf)
test_data_dir <- system.file("sgf", package="Rsgf")
player_data <- Rsgf::playerCard("Honinbo Shusaku", test_data_dir)

# Simple aggregation for heatmap:
agg_games <- aggregate(Game.Number ~ x + y, data=player_data, FUN=length)
names(agg_games) <- c("x", "y", "Count")
agg_games$Count <- as.factor(agg_games$Count)

# Graph heatmap
Rsgf::boardHeatMap(agg_games)

# Extended Player Card and Heatmap Examples
# https://bcable.net/analysis-Rsgf_period_cards.html
# https://bcable.net/analysis-Rsgf_moves_anim.html
# https://bcable.net/analysis-Rsgf_player_card.html
}
\keyword{Rsgf}
\keyword{sgf}
\keyword{sgfs}
\keyword{baduk}
\keyword{go}
\keyword{igo}
\keyword{smart game file}
\keyword{smart game}
\keyword{save game file}
\keyword{save game}
