substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

fetch_shots_by_player_id_and_season = function(player_id, season) {
  req(player_id, season)

  request = GET(
    "http://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID = player_id,
      Season = season,
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      SeasonType = "Regular Season",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    )
  )

  stop_for_status(request)

  data = content(request)

  raw_shots_data = data$resultSets[[1]]$rowSet
  col_names = tolower(as.character(data$resultSets[[1]]$headers))
  print(paste("shots: ",toString(length(raw_shots_data))))

  if (length(raw_shots_data) == 0) {
    shots = data.frame(
      matrix(nrow = 0, ncol = length(col_names))
    )
  } else {
    shots = data.frame(
      matrix(
        unlist(raw_shots_data),
        ncol = length(col_names),
        byrow = TRUE
      )
    )
  }

  shots = tbl_df(shots)
  names(shots) = col_names

  shots = mutate(shots,
    loc_x = as.numeric(as.character(loc_x)) / 10,
    loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
    shot_distance = as.numeric(as.character(shot_distance)),
    shot_made_numeric = as.numeric(as.character(shot_made_flag)),
    shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
    shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
    shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2)
  )

  raw_league_avg_data = data$resultSets[[2]]$rowSet
  league_avg_names = tolower(as.character(data$resultSets[[2]]$headers))
  league_averages = tbl_df(data.frame(
    matrix(unlist(raw_league_avg_data), ncol = length(league_avg_names), byrow = TRUE)
  ))
  names(league_averages) = league_avg_names
  league_averages = mutate(league_averages,
    fga = as.numeric(as.character(fga)),
    fgm = as.numeric(as.character(fgm)),
    fg_pct = as.numeric(as.character(fg_pct)),
    shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2)
  )

  return(list(player = shots, league_averages = league_averages))
}

# default_shots = fetch_shots_by_player_id_and_season(default_player$person_id, default_season)

fetch_shots_in_range_by_player_id = function(player_id,start,end) {
  # req(player_id,start)
  start = as.integer(substr(start,1,4))
  end = as.integer(substr(end,1,4))
  for(i in start:end){
  season = paste(toString(i), "-", substrRight(toString(i+1),2),sep="")
  print(season)
  request = GET(
    "http://stats.nba.com/stats/shotchartdetail",
    add_headers(referer= 'http://stats.nba.com/scores/'),
    query = list(
      PlayerID = player_id,
      Season = season,
      ContextMeasure = "FGA",
      DateFrom = "",
      DateTo = "",
      GameID = "",
      GameSegment = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      Period = 0,
      Position = "",
      RookieYear = "",
      SeasonSegment = "",
      SeasonType = "Regular Season",
      TeamID = 0,
      VsConference = "",
      VsDivision = ""
    )
  )

  stop_for_status(request)

  data = content(request)

  raw_shots_data = data$resultSets[[1]]$rowSet
  col_names = tolower(as.character(data$resultSets[[1]]$headers))

  if(i == start){
    all_shots = raw_shots_data
  }
  else{
    all_shots <- rbind(all_shots,raw_shots_data)
  }
    # str(raw_shots_data)
}



  if (length(all_shots) == 0) {
    shots = data.frame(
      matrix(nrow = 0, ncol = length(col_names))
    )
  } else {
    shots = data.frame(
      matrix(
        unlist(all_shots),
        ncol = length(col_names),
        byrow = TRUE
      )
    )
  }

  shots = tbl_df(shots)
  names(shots) = col_names
  # print(paste("shots: ",toString(length(shots))))

  shots = mutate(shots,
    loc_x = as.numeric(as.character(loc_x)) / 10,
    loc_y = as.numeric(as.character(loc_y)) / 10 + hoop_center_y,
    shot_distance = as.numeric(as.character(shot_distance)),
    shot_made_numeric = as.numeric(as.character(shot_made_flag)),
    shot_made_flag = factor(shot_made_flag, levels = c("1", "0"), labels = c("made", "missed")),
    shot_attempted_flag = as.numeric(as.character(shot_attempted_flag)),
    shot_value = ifelse(tolower(shot_type) == "3pt field goal", 3, 2)
  )

  raw_league_avg_data = data$resultSets[[2]]$rowSet
  league_avg_names = tolower(as.character(data$resultSets[[2]]$headers))
  league_averages = tbl_df(data.frame(
    matrix(unlist(raw_league_avg_data), ncol = length(league_avg_names), byrow = TRUE)
  ))
  names(league_averages) = league_avg_names
  league_averages = mutate(league_averages,
    fga = as.numeric(as.character(fga)),
    fgm = as.numeric(as.character(fgm)),
    fg_pct = as.numeric(as.character(fg_pct)),
    shot_value = ifelse(shot_zone_basic %in% c("Above the Break 3", "Backcourt", "Left Corner 3", "Right Corner 3"), 3, 2)
  )

  return(list(player = shots, league_averages = league_averages))
}