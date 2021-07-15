# SGF Node abbreviation, names, and descriptions used in .getTRdf()
# were taken from the specification:

# https://www.red-bean.com/sgf/ff1_3/ff3.html
# Accessed: 2019-09-28

# https://www.red-bean.com/sgf/proplist_ff.html
# Accessed: 2019-09-28

PASS_STRING <- "--"
COLOR_EMPTY <- "."
COLOR_BLACK <- "B"
COLOR_WHITE <- "W"
COLOR_TBLACK <- "TB"
COLOR_TWHITE <- "TW"

.getTRdf <- function(){
	data.frame(
		Node.Abbreviation=c(
			"AB", "AE", "AN",
			"AP", "AR", "AS", "AW",
			"B", "BL", "BM", "BR",
			"BS", "BT", "C",
			"CA", "CH", "CP", "CR",
			"DD", "DG", "DM", "DO",
			"DT", "EL", "EV", "EX", "FF",
			"FG", "GB", "GC", "GM",
			"GN", "GW", "HA", "HO",
			"ID", "IP", "IT", "IY",
			"KM", "KO", "LB", "LN", "LT",
			"MA", "MN", "N", "OB",
			"OM", "ON", "OP", "OT",
			"OV", "OW", "PB", "PC",
			"PL", "PM", "PW", "RE",
			"RG", "RO", "RU", "SC",
			"SE", "SI", "SL", "SO",
			"SQ", "ST", "SU", "SZ",
			"TB", "TC", "TE", "TM",
			"TR", "TW", "UC", "US",
			"V", "VW", "W", "WL", "WR",
			"WS", "WT"
		),
		Node.Name=c(
			"Move.Add.Black", "Move.Add.Empty", "Analysis.Credit",
			"Application", "Mark.Arrow", "Adds.Stones", "Move.Add.White",
			"Move.Black", "Black.Time.Left", "Move.Bad", "Black.Rank",
			"Black.Species", "Black.Team", "Mark.Comment.General",
			"Character.Encoding", "Mark.Check", "Copyright", "Mark.Circle",
			"Mark.Dim.Point", "Diagram", "Even.Position", "Move.Doubtful",
			"Date", "Evaluate.Move", "Event", "Move.Expected", "File.Format",
			"Figure", "Mark.Black.Good", "Comment.Game", "Game.Type",
			"Game.Name", "Mark.White.Good", "Handicap", "Mark.Hotspot",
			"Game.ID", "Initial.Position", "Mark.Interesting", "Invert.Y.Axis",
			"Komi", "Move.WinKo", "Label", "Mark.Line", "Lose.Time",
			"Mark.Cross", "Move.Number", "Node.Name", "ByoYomi.Black",
			"Overtime.Moves", "Opening", "Overtime.Period", "Overtime.Length",
			"ByoYomi.Overhead", "ByoYomi.White", "Black.Player.Name", "Place",
			"Player.Turn", "Print.Move.Mode", "White.Player.Name", "Result",
			"Board.Region", "Round", "Rules", "Mark.Secure.Stone",
			"Move.Self.Test", "Mark.Sigma", "Mark.Selected", "Source",
			"Mark.Square", "Style", "Setup type", "Board.Size",
			"Mark.Territory.Black", "Territory.Count", "Mark.Tesuji", "Time",
			"Mark.Triangle", "Mark.Territory.White", "Mark.Unclear", "User",
			"Node.Value", "View", "Move.White", "White.Time.Left", "White.Rank",
			"White.Species", "White.Team"
		),
		Node.Description=c(
			"Edit: Add black stones",
			"Edit: add empty points",
			"Who did the Analysis",
			"Application name",
			"Arrow",
			"Who adds stones",
			"Edit: Add white stones",
			"Black move",
			"Time left for Black, seconds",
			"bad move",
			"Black's Rank",
			"black species",
			"Team of black player",
			"Comment",
			"Character encoding",
			"Check mark",
			"Copyright on game comments",
			"Circle marker",
			"Dim points",
			"Diagram, for printing",
			"even position",
			"doubtful move",
			"Date",
			"Eval. comp move",
			"Event (tournament)",
			"Expected move",
			"File format",
			"figure, for printing",
			"Good for black",
			"Comment about the game",
			"Game type",
			"Game name",
			"Something good for white",
			"Number of handicap stones",
			"Hotspot node mark",
			"The game ID",
			"Initial position",
			"interesting move",
			"Invert Y-axis",
			"komi",
			"WinKo, execute illegal move",
			"label",
			"Line",
			"Lose on Time is enforced",
			"Mark: crosses",
			"set move number in diagrams",
			"Node name",
			"Number of black stones in byo-yomi",
			"Number of moves per overtime period",
			"The opening",
			"Overtime",
			"Length of overtime period",
			"seconds overhead in byo-yomi",
			"Number of white stones in byo-yomi",
			"Black player name",
			"Place",
			"Player whose turn it is",
			"Print move mode",
			"White player name",
			"Result of the game",
			"region on the board",
			"Round in tournament",
			"Rules: Japanese or Chinese",
			"Secure stones",
			"Moves tried in self-test",
			"Position marked with a sigma",
			"Selected points",
			"Source: book, journal,...",
			"Square",
			"Style",
			"Setup type",
			"size of the board",
			"Black territory",
			"Territory count: B-W",
			"Good move, tesuji",
			"Time for each player",
			"Triangle markers",
			"White territory",
			"Unclear position",
			"User: who entered game",
			"Node value",
			"View",
			"White move",
			"Time left for White",
			"White's Rank",
			"white species",
			"Team of white player"
		)
	)
}

getNodeDescription <- function(property){
	df <- .getTRdf()
	ret <- as.character(
		df$Node.Description[df$Node.Abbreviation == property]
	)
	ret
}

getNodeName <- function(property){
	df <- .getTRdf()
	if(property %in% df$Node.Abbreviation){
		as.character(
			df$Node.Name[df$Node.Abbreviation == property]
		)
	} else {
		"Unknown"
	}
}

loadSGF <- function(filename, game.number=1){
	if(is.vector(filename) && length(filename) > 1){
		ret <- NULL
		gameno <- game.number
		for(filename1 in filename){
			ret <- rbind(ret, loadSGF(filename1, game.number=gameno))
			gameno <- gameno + 1
		}
		ret

	} else if(!file.exists(filename)){
		warning("Invalid SGF, file not found")
		return(NA)

	} else if(file.info(filename)$isdir){
		warning("Invalid SGF, is a directory")
		return(NA)

	} else {
		ret <- NULL
		lineno <- 0
		moveno <- 0
		fp <- file(filename)
		lines <- readLines(fp, warn=FALSE)
		header <- data.frame(Game.Number=game.number)
		allmoves <- data.frame()
		last_token <- NULL
		for(line in lines){
			if(line == ")"){
				break
			}

			lineno <- lineno + 1
			index <- 1
			themove <- NULL

			if(lineno == 1){
				line <- stri_trim_left(line)
				if(substr(line, 1, 1) != "("){
					warning("Invalid SGF")
					return(NA)
				}
				index <- index + 1
			}

			subset_to_end <- substr(line, index, nchar(line))
			while(subset_to_end != ""){
				token_idx <- str_locate(subset_to_end, "[;\\]]")[[1]]
				subset_to_token <- substr(subset_to_end, 1, token_idx)

				if(!is.na(token_idx)){
					subset_to_end <- substr(
						subset_to_end, token_idx+1, nchar(subset_to_end)
					)
				} else {
					subset_to_end <- ""
				}

				if(!is.na(subset_to_token) && nchar(subset_to_token) != 1){
					split_loc <- str_locate(subset_to_token, "\\[")[[1]]
					property <- str_trim(
						substr(subset_to_token, 1, split_loc-1)
					)
					value <- str_trim(substr(
						subset_to_token, split_loc+1,
						nchar(subset_to_token)-1
					))

					if(!is.na(property) && !is.na(value)){
						if(property == ""){
							property <- last_token
						}

						property_name <- getNodeName(property)

						if(
							substr(property_name, 1, 5) %in%
								c("Mark.", "Move.")
						){
							themove <- data.frame(Move.Number=NA)

							if(substr(property_name, 1, 9) %in% c("Move.Add.")){
								themove$Move.Number <- 0

							} else if(
								substr(property_name, 1, 5) %in% c("Move.")
							){
								moveno <- moveno + 1
								themove$Move.Number <- moveno
								if(value == ""){
									value <- PASS_STRING
								}

							} else {
								themove$Move.Number <- NA
							}

							mark_move_in_allmoves <- colnames(allmoves)[
								substr(colnames(allmoves), 1, 5) %in%
									c("Mark.", "Move.")
							]

							for(mm_property in mark_move_in_allmoves){
								if(!(mm_property %in% colnames(themove))){
									themove[[mm_property]] <- NA
								}
							}

							themove[[property_name]] <- value

							if(
								!(property_name %in% colnames(allmoves)) &&
								!is.null(allmoves) &&
								nrow(allmoves) > 0
							){
								allmoves[[property_name]] <- rep(
									NA, nrow(allmoves)
								)
							}

							allmoves <- rbind(allmoves, themove)

						} else if(property_name %in% colnames(header)){
							old_value <- header[[property_name]]
							header[[property_name]] <- paste0(c(
								old_value, ",", value
							), collapse="")

						} else {
							header[[property_name]] <- value
						}

						last_token <- property
					}
				}
			}
		}

		close(fp)
		ret <- c(data.frame(header), data.frame(allmoves))
	}

	ret
}

translateXY <- function(move, move.numbers=FALSE, force.df=FALSE){
	if(force.df || length(move) > 1){
		ret <- t(data.frame(lapply(move, FUN=translateXY)))

		# set row and col names
		if(length(move.numbers) > 1){
			rownames(ret) <- move.numbers
		} else {
			rownames(ret) <- seq(1, nrow(ret))
		}
		colnames(ret) <- c("x", "y")

		ret

	} else {
		# handle a pass or non-move
		if(is.na(move) || move == "" || move == "tt" || move == PASS_STRING){
			c(NA, NA)

		# regular moves
		} else {
			ret <- utf8ToInt(move)-97

			if(
				sum(as.numeric(ret < 0)) > 0 ||
				sum(as.numeric(ret > 18)) > 0
			){
				warning("Error: Invalid Move")
				return(NA)
			}

			# Y is going in negative direction
			ret[[2]] <- -ret[[2]]

			ret
		}
	}
}

.gameAddXYConditional <- function(game, field, color){
	gameDF <- data.frame(game)

	if(field %in% colnames(gameDF)){
		gameRet <- data.frame(translateXY(game[[field]], force.df=TRUE))
		gameRet$Move.Number <- as.numeric(as.character(game$Move.Number))
		gameRet$Color[!is.na(gameRet$x)] <- color
		gameRet$Color[gameDF[[field]] == PASS_STRING] <- color
		gameRet
	}
}

gameXY <- function(game){
	gameB <- .gameAddXYConditional(game, "Move.Black", COLOR_BLACK)
	gameW <- .gameAddXYConditional(game, "Move.White", COLOR_WHITE)
	gameAddB <- .gameAddXYConditional(game, "Move.Add.Black", COLOR_BLACK)
	gameAddW <- .gameAddXYConditional(game, "Move.Add.White", COLOR_WHITE)
	gameMTB <- .gameAddXYConditional(game, "Mark.Territory.Black", COLOR_TBLACK)
	gameMTW <- .gameAddXYConditional(game, "Mark.Territory.White", COLOR_TWHITE)

	game_merge <- gameB

	if(is.null(game_merge)){
		game_merge <- gameW
	} else {
		game_merge[!is.na(gameW$Color),] <- gameW[!is.na(gameW$Color),]
	}

	if(is.null(game_merge)){
		game_merge <- gameAddB
	} else {
		game_merge[!is.na(gameAddB$Color),] <- gameAddB[!is.na(gameAddB$Color),]
	}

	if(is.null(game_merge)){
		game_merge <- gameAddW
	} else {
		game_merge[!is.na(gameAddW$Color),] <- gameAddW[!is.na(gameAddW$Color),]
	}

	if(is.null(game_merge)){
		game_merge <- gameMTB
	} else {
		game_merge[!is.na(gameMTB$Color),] <- gameMTB[!is.na(gameMTB$Color),]
	}

	if(is.null(game_merge)){
		game_merge <- gameMTW
	} else {
		game_merge[!is.na(gameMTW$Color),] <- gameMTW[!is.na(gameMTW$Color),]
	}

	game_merge
}

playerCard <- function(player_names, sgf_paths){
	if(is.vector(sgf_paths) && length(sgf_paths) > 1){
		ret <- NULL
		for(sgf_path in sgf_paths){
			new_pc <- playerCard(player_names, sgf_path)
			if(is.null(ret)){
				ret <- new_pc
			} else {
				ret <- merge(ret, new_pc, all=TRUE)
			}
		}

	} else {
		sgf_path <- sgf_paths
		player_df <- NULL

		if(file.info(sgf_path)$isdir){
			sgf_files <- file.path(sgf_path, list.files(sgf_path))
		} else {
			sgf_files <- sgf_path
		}

		i <- 1
		for(sgf_file in sgf_files){
			new_df <- loadSGF(sgf_file)
			if(length(new_df) == 1 && is.na(new_df)){
				next
			}

			new_xy <- gameXY(new_df)
			new_xy <- new_xy[
				!is.na(new_xy$Move.Number) &
				!is.na(new_xy$x) &
				new_xy$Move.Number != 0,
			]

			# filter by player name
			if(player_names != "*"){
				if(
					player_names == "B" ||
					new_df$Black.Player.Name %in% player_names
				){
					new_xy <- new_xy[new_xy$Color == COLOR_BLACK,]
				} else if(
					player_names == "W" ||
					new_df$White.Player.Name %in% player_names
				){
					new_xy <- new_xy[new_xy$Color == COLOR_WHITE,]
				} else {
					next
				}
			}

			if(!is.null(new_xy) && nrow(new_xy) != 0){
				# add header info to new player df
				new_xy <- merge(new_xy, as.data.frame(new_df)[1,
					names(new_df)[!(
						substr(names(new_df), 1, 5) %in% c("Move.", "Mark.")
					)]
				])

				new_xy$Game.Number <- rep(i, nrow(new_xy))
				new_xy$File.Name <- rep(sgf_file, nrow(new_xy))

				if(is.null(player_df)){
					player_df <- new_xy
				} else {
					player_df <- merge(player_df, new_xy, all=TRUE)
				}

				i <- i + 1
			}

		}
		ret <- player_df
	}

	ret
}

getColorPalette <- function(count){
	colorRampPalette(c("#101010", "#dcb35c"))(
		max(as.numeric(as.character(count)))
	)
}

boardHeatMap <- function(data){
	board_png <- readPNG(
		system.file("img", "Blank_Go_board_hollow.png", package="Rsgf")
	)

	go_palette <- colorRampPalette(c("#101010", "#dcb35c"))(
		max(as.numeric(as.character(data$Count)))
	)

	data$Count <- as.factor(as.character(data$Count))

	g <- ggplot(data, aes(x=data$x, y=data$y, fill=data$Count))
	g <- g + geom_tile()

	g <- g + annotation_raster(
		board_png, ymin=0.6, ymax=-18.6, xmin=-0.6, xmax=18.6
	)

	g <- g + scale_fill_manual(
		values=getColorPalette(data$Count), breaks=seq(1,
			max(as.numeric(as.character(data$Count)))
		)
	)

	g <- g + coord_cartesian(
		xlim=c(-0.5, 18.5), ylim=c(0.5, -18.5), expand=FALSE
	)

	g <- g + theme(
		legend.position="none",
		line=element_blank(),
		rect=element_blank(),
		text=element_blank(),
		panel.background=element_blank(),
		panel.border=element_blank(),
		panel.spacing=unit(0, "cm"),
		plot.margin=margin(0, 0, 0, 0, "cm")
	)
	g
}

# SGFGame class (RC/S4)

# underlying RC for variable encapsulation
.SGFGame <- setRefClass(Class="SGFGame",
	fields=list(
		board_state="matrix",
		captured_black="numeric",
		captured_white="numeric",
		move_df="data.frame",
		move_list="list",
		move_number="numeric",
		move_total="numeric",
		tmp_df1="ANY",
		tmp_df2="ANY"
	),

	methods=list(

		board_initialize=function(moves){
			move_total <<- max(moves$Move.Number[!is.na(moves$Move.Number)])
			move_list <<- moves
			move_df <<- gameXY(moves)
		},

		clear_board=function(){
			move_number <<- -1
			captured_black <<- 0
			captured_white <<- 0
			board_state <<- matrix(rep(COLOR_EMPTY, 19*19), ncol=19)
		},

		get_board_state=function(x, y){
			if(!.self$acceptable_xy(x, y)) return(NA)

			# adjust for 0-18 to 1-19 since that's how matrices are stupidly
			# stored; also because the Y-value is returned as a negative for
			# graphing purposes, this becomes absolute values
			x <- abs(x) + 1
			y <- abs(y) + 1
			board_state[y, x]
		},

		set_board_state=function(x, y, value){
			x <- abs(x) + 1
			y <- abs(y) + 1
			board_state[y, x] <<- value
		},

		opposite_color=function(color){
			if(color == COLOR_WHITE){
				return(COLOR_BLACK)

			} else if(color == COLOR_BLACK){
				return(COLOR_WHITE)

			} else {
				return(NA)
			}
		},

		acceptable_xy=function(x, y){
			if(is.na(x) || is.na(y) || x < 0 || x > 18 || y < 0 || y > 18){
				return(FALSE)
			} else {
				return(TRUE)
			}
		},

		move_to=function(move_end){
			if(move_end < move_number+1 || move_end > move_total){
				return(NA)
			}

			for(move_no in seq(move_number+1, move_end)){
				current_moves <- move_df[
					(move_df$Move.Number == move_no) %in% TRUE,
				]

				for(cur in seq(1, nrow(current_moves))){
					current_move <- current_moves[cur,]
					x <- abs(current_move$x)
					y <- abs(current_move$y)

					if(!is.na(x) && !is.na(y)){
						.self$set_board_state(x, y, current_move$Color)

						opposite <- .self$opposite_color(
							.self$get_board_state(x, y)
						)

						# test to see if anything was captured, starting with
						# the other color
						tmp <- .self$get_board_state(x-1, y)
						if(!is.na(tmp) && tmp == opposite){
							.self$cluster_capture(x-1, y)
						}

						tmp <- .self$get_board_state(x+1, y)
						if(!is.na(tmp) && tmp == opposite){
							.self$cluster_capture(x+1, y)
						}

						tmp <- .self$get_board_state(x, y-1)
						if(!is.na(tmp) && tmp == opposite){
							.self$cluster_capture(x, y-1)
						}

						tmp <- .self$get_board_state(x, y+1)
						if(!is.na(tmp) && tmp == opposite){
							.self$cluster_capture(x, y+1)
						}
					}
				}
			}
			move_number <<- move_end
		},

		cluster_capture=function(x, y){
			if(!.self$acceptable_xy(x, y)) return(NA)

			ret <- .self$cluster_meta(x, y)

			if(!is.null(tmp_df1) && ret[[1]] != 0 && ret[[2]] == 0){
				tmp_df_tmp <- data.frame(tmp_df1)

				# count captured stones
				tmp <- .self$get_board_state(x, y)
				if(!is.na(tmp)){
					if(tmp == COLOR_BLACK){
						captured_white <<- captured_white + nrow(tmp_df_tmp)
					} else {
						captured_black <<- captured_black + nrow(tmp_df_tmp)
					}
				}

				# erase on the board
				for(i in seq(1, nrow(tmp_df_tmp))){
					.self$set_board_state(
						tmp_df_tmp$x[[i]],
						tmp_df_tmp$y[[i]],
						COLOR_EMPTY
					)
				}
			}
		},

		cluster_helper=function(tmp_x, tmp_y, x, y, cluster_color){
			col_check <- .self$get_board_state(tmp_x, tmp_y)

			# it's a part of the cluster
			if(col_check == cluster_color){
				.self$cluster_info(
					tmp_x, tmp_y, x, y, cluster_color
				)

			# it's a liberty
			} else if(col_check == COLOR_EMPTY){
				tmp_df2$x <<- c(tmp_df2$x, tmp_x)
				tmp_df2$y <<- c(tmp_df2$y, tmp_y)
			}
		},

		cluster_info=function(x, y, prev_x=NULL, prev_y=NULL, color=NULL){
			x <- abs(x)
			y <- abs(y)

			# deal with cluster color
			if(is.null(color)){
				cluster_color <- get_board_state(x, y)
			} else {
				cluster_color <- color
			}

			# first call, clear temp data
			if(is.null(prev_x) || is.null(prev_y)){
				tmp_df1 <<- NULL # stones
				tmp_df2 <<- NULL # liberties
			}

			# if we're sitting on a non-stone (liberty!)
			if(cluster_color == COLOR_EMPTY){
				tmp_df2$x <<- c(tmp_df2$x, x)
				tmp_df2$y <<- c(tmp_df2$y, y)
				return(NA)
			}

			# prevent loops
			if(
				!is.null(tmp_df1) &&
				nrow(as.data.frame(tmp_df1)[
					tmp_df1$x == x & tmp_df1$y == y,
				]) > 0
			){
				return(NA)
			} else {
				tmp_df1$x <<- c(tmp_df1$x, x)
				tmp_df1$y <<- c(tmp_df1$y, y)
			}

			# handle all four possible positions for liberties/cluster

			# left
			if(x > 0 && (is.null(prev_x) || !(prev_x == x-1 && prev_y == y))){
				.self$cluster_helper(x-1, y, x, y, cluster_color)
			}

			# right
			if(x < 18 && (is.null(prev_x) || !(prev_x == x+1 && prev_y == y))){
				.self$cluster_helper(x+1, y, x, y, cluster_color)
			}

			# up
			if(y > 0 && (is.null(prev_x) || !(prev_x == x && prev_y == y-1))){
				.self$cluster_helper(x, y-1, x, y, cluster_color)
			}

			# down
			if(y < 18 && (is.null(prev_x) || !(prev_x == x && prev_y == y+1))){
				.self$cluster_helper(x, y+1, x, y, cluster_color)
			}
		},

		cluster_meta=function(x, y){
			.self$cluster_info(x, y)
			c(
				nrow(unique(as.data.frame(tmp_df1))),
				nrow(unique(as.data.frame(tmp_df2)))
			)
		}
	)
)

# create as S4 class to wrap constructor, object, etc
setGeneric("SGFGame", function(moves){
	ret <- .SGFGame()
	ret$clear_board()
	ret$board_initialize(moves)
	ret
})
