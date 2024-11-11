set.seed(1212)
library(dplyr)
library(ggplot2)

# Define the Monopoly board setup and properties (as before)
board = c(
  "Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue", "Income Tax", "Reading Railroad",
  "Oriental Avenue", "Chance", "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
  "Electric Company", "States Avenue", "Virginia Avenue", "Pennsylvania Railroad", "St. James Place",
  "Community Chest", "Tennessee Avenue", "New York Avenue", "Free Parking", "Kentucky Avenue",
  "Chance", "Indiana Avenue", "Illinois Avenue", "B&O Railroad", "Atlantic Avenue", "Ventnor Avenue",
  "Water Works", "Marvin Gardens", "Go to Jail", "Pacific Avenue", "North Carolina Avenue", "Community Chest",
  "Pennsylvania Avenue", "Short Line Railroad", "Chance", "Park Place", "Luxury Tax", "Boardwalk"
)

properties = data.frame(
  name = c("Mediterranean Avenue", "Baltic Avenue", "Reading Railroad", "Oriental Avenue", "Vermont Avenue", 
           "Connecticut Avenue", "St. Charles Place", "States Avenue", "Virginia Avenue", "Pennsylvania Railroad",
           "St. James Place", "Tennessee Avenue", "New York Avenue", "Kentucky Avenue", "Indiana Avenue",
           "Illinois Avenue", "B&O Railroad", "Atlantic Avenue", "Ventnor Avenue", "Marvin Gardens",
           "Pacific Avenue", "North Carolina Avenue", "Pennsylvania Avenue", "Short Line Railroad",
           "Park Place", "Boardwalk"),
  color = c("Brown", "Brown", "Railroad", "Light Blue", "Light Blue", "Light Blue",
            "Pink", "Pink", "Pink", "Railroad", "Orange", "Orange", "Orange",
            "Red", "Red", "Red", "Railroad", "Yellow", "Yellow", "Yellow",
            "Green", "Green", "Green", "Railroad", "Dark Blue", "Dark Blue"),
  price = c(60, 60, 200, 100, 100, 120, 140, 140, 160, 200, 180, 180, 200, 220, 220, 240, 200, 260, 260, 280,
            300, 300, 320, 200, 350, 400),
  rent = c(2, 4, 25, 6, 6, 8, 10, 10, 12, 25, 14, 14, 16, 18, 18, 20, 25, 22, 22, 24, 26, 26, 28, 25, 35, 50)
)

# Function to run a single game and track property, color, and player win data
run_monopoly_game = function() {
  # Initialize players
  players = data.frame(
    name = c("Player 1", "Player 2", "Player 3"),
    position = c(1, 1, 1),  # Position Start
    money = c(1500, 1500, 1500),
    properties = I(list(character(0), character(0), character(0)))
  )
  
  # Create a tracker for how often properties are landed on
  game_landing_stats = properties
  game_landing_stats$landed_on = rep(0, nrow(game_landing_stats))
  game_landing_stats$rent_collected = rep(0, nrow(game_landing_stats))
  
  turn_count = 0
  
  # Helper functions
  roll_dice = function() { sum(sample(1:6, 2, replace = TRUE)) }
  
  move_player = function(player) {
    roll = roll_dice()
    new_position = (players$position[player] + roll - 1) %% length(board) + 1
    players$position[player] <<- new_position
    
    # Check if the landed position is a property
    current_space = board[new_position]
    if (current_space %in% game_landing_stats$name) {
      property_index = which(game_landing_stats$name == current_space)
      game_landing_stats$landed_on[property_index] <<- game_landing_stats$landed_on[property_index] + 1
    }
  }
  
  handle_property = function(player) {
    current_space = board[players$position[player]]
    if (current_space %in% properties$name) {
      property_index = which(properties$name == current_space)
      
      # Check if the property is already owned
      owner = NULL
      for (i in 1:nrow(players)) {
        if (current_space %in% players$properties[[i]]) {
          owner = i
          break
        }
      }
      
      if (is.null(owner)) {
        price = properties$price[property_index]
        if (players$money[player] >= price) {
          players$money[player] <<- players$money[player] - price
          players$properties[[player]] <<- c(players$properties[[player]], current_space)
        }
      } else if (owner != player) {
        rent = properties$rent[property_index]
        players$money[player] <<- players$money[player] - rent
        players$money[owner] <<- players$money[owner] + rent
        game_landing_stats$rent_collected[property_index] <<- game_landing_stats$rent_collected[property_index] + rent
      }
    }
  }
  
  # Run rounds until one player wins
  while (sum(players$money > 0) > 1) {
    for (i in 1:nrow(players)) {
      if (players$money[i] > 0) {
        move_player(i)
        handle_property(i)
      }
    }
    turn_count = turn_count + 1  # Count each complete round as a turn player 1 goes and then again is 1 turn 
  }
  
  winner_index = which.max(players$money)
  return(list(winner = winner_index, landing_stats = game_landing_stats, turns = turn_count))
}

num_simulations = 1000
player_wins = c(Player1 = 0, Player2 = 0, Player3 = 0)
total_landing_stats = properties
total_landing_stats$landed_on = rep(0, nrow(properties))
total_landing_stats$rent_collected = rep(0, nrow(properties))
total_turns = numeric(num_simulations)

for (i in 1:num_simulations) {
  game_result = run_monopoly_game()
  winner_index = game_result$winner
  player_wins[winner_index] = player_wins[winner_index] + 1
  total_landing_stats$landed_on = total_landing_stats$landed_on + game_result$landing_stats$landed_on
  total_landing_stats$rent_collected = total_landing_stats$rent_collected + game_result$landing_stats$rent_collected
  total_turns[i] = game_result$turns
}

# Calculate average landings and rent collected per game
total_landing_stats$avg_landed_on = total_landing_stats$landed_on / num_simulations
total_landing_stats$avg_rent_collected = total_landing_stats$rent_collected / num_simulations

color_analysis = total_landing_stats %>%
  group_by(color) %>%
  summarize(
    avg_landed_on = sum(avg_landed_on),
    avg_rent_collected = sum(avg_rent_collected)
  ) %>%
  arrange(desc(avg_rent_collected))

avg_turns = mean(total_turns)
min_turns = min(total_turns)
max_turns = max(total_turns)

cat("Number of Wins Over 1000 Games:\n")
print(player_wins)

cat("\nWin Percentages:\n")
total_games = sum(player_wins)
win_percentages = player_wins / total_games * 100
print(win_percentages)

cat("\nTurn Analysis:\n")
cat("Average Turns:", avg_turns, "\n")
cat("Minimum Turns:", min_turns, "\n")
cat("Maximum Turns:", max_turns, "\n")

cat("\nTop 10 Best Properties to Own Based on Average Times Landed On Per Game:\n")
best_properties = total_landing_stats %>%
  arrange(desc(avg_landed_on)) %>%
  head(10)
print(best_properties[, c("name", "avg_landed_on")])

cat("\nBottom 10  Properties to Own Based on Average Times Landed On Per Game:\n")
worst_properties = total_landing_stats %>%
  arrange(desc(avg_landed_on)) %>%
  tail(10)
print(worst_properties[, c("name", "avg_landed_on")])

cat("\nTop Colors Based on Average Rent Collected:\n")
print(color_analysis)

# Visualization of results
# Wins Bar Plot
wins_df = data.frame(Player = names(player_wins), Wins = player_wins)
ggplot(wins_df, aes(x = Player, y = Wins, fill = Player)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Wins per Player Over 1000 Games", x = "Player", y = "Wins") +
  theme_minimal()

# Properties Landed On Bar Plot
best_properties %>%
  ggplot(aes(x = reorder(name, avg_landed_on), y = avg_landed_on)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Properties Based on Average Times Landed On", x = "Property", y = "Average Landings") +
  theme_minimal()
worst_properties %>%
  ggplot(aes(x = reorder(name, avg_landed_on), y = avg_landed_on)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Bottom 10 Properties Based on Average Times Landed On", x = "Property", y = "Average Landings") +
  theme_minimal()


# Color Groups Rent Collected Bar Plot
ggplot(color_analysis, aes(x = reorder(color, avg_rent_collected), y = avg_rent_collected, fill = color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Colors Based on Average Rent Collected", x = "Color", y = "Average Rent Collected") +
  theme_minimal()

# Histogram of Turn Counts
ggplot(data.frame(Turns = total_turns), aes(x = Turns)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Total Turns per Game", x = "Total Turns", y = "Frequency") +
  theme_minimal()



## is the number of rolls (duration of the game) grown exponetilly or is there an "optimal" number of players
## it appears as if there is no "optimal" number of players the duartion of the game continues to grow the more players you add it appeats pretty exponetnially

## is there an advantage on winning depending on what player you are in the game?
## it appears for 2-4 players going first will yield a higher winning percentage, but when played with 5 players it appears as if going second has the highest chance of winning

## is there a property in the game that gets landed on more than others (I feel it should all be even)
## it appears that every property does not have an equal chance to be landed on which I find shocking.(its a very small margin I wouldnt consider this statistrically)

## Since some properties have a very slight advantage whats the best color to own!
## there is only one property with 4 pieces. and as you could imagine that is the best to own! Other than that is it based on price.
