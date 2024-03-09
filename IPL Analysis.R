# Load required libraries
library(tidyverse)
library(dplyr)

# Read the dataset
ipl_data <- read.csv("IPL Dataset.csv")

# Displayed the first few rows of the dataset
head(ipl_data)

# Summary statistics
summary(ipl_data)
colnames(ipl_data)
str(ipl_data)


#Number of Matches Played by Each Team
team_matches <- ipl_data %>%
  select(team1 = team1, team2 = team2) %>%
  # Combine team1 and team2 into a single column using the gather function
  tidyr::gather(key = "team", value = "team_name", team1, team2) %>%
  group_by(team_name) %>%
  summarize(total_matches = n())

# Result
print(team_matches)

# Counted the total wins for each team
total_wins <- ipl_data %>%
  group_by(winner) %>%
  summarize(total_wins = n())

# Checked the data types of team_matches
str(team_matches)

# Converted total_matches to numeric
team_matches$total_matches <- as.numeric(team_matches$total_matches)

# Calculated win percentage
team_summary <- left_join(total_wins, team_matches, by = c("winner" = "team_name")) %>%
  mutate(win_percentage = ifelse(total_matches > 0, total_wins / total_matches * 100, 0))

# Result
print(team_summary)


#Distribution of Results
result_distribution <- ipl_data %>%
  group_by(result) %>%
  summarize(count = n())

# Visualized the distribution
ggplot(result_distribution, aes(x = result, y = count, fill = result)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Match Results")


#Top Players with Most Player of the Match Awards
top_players <- ipl_data %>%
  group_by(player_of_match) %>%
  summarize(total_awards = n()) %>%
  arrange(desc(total_awards)) %>%
  head(10)

# Visualized the top players
ggplot(top_players, aes(x = player_of_match, y = total_awards, fill = player_of_match)) +
  geom_bar(stat = "identity") +
  labs(title = "Top Players with Most Player of the Match Awards")


# Counted the occurrences of each combination of winner and toss decision
win_toss_count <- ipl_data %>%
  group_by(winner, toss_decision) %>%
  summarize(count = n()) %>%
  arrange(winner)

# Created a stacked bar chart
ggplot(win_toss_count, aes(x = winner, y = count, fill = toss_decision)) +
  geom_bar(stat = "identity") +
  geom_text(data = win_toss_count, aes(label = count), vjust = -0.5) +
  labs(title = "Winners and Toss Decision",
       x = "Winner",
       y = "Count",
       fill = "Toss Decision") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# Counted the occurrences of each combination of venue and toss decision
venue_toss_count <- ipl_data %>%
  group_by(venue, toss_decision) %>%
  summarize(count = n()) %>%
  arrange(venue)

# Identifyed the toss decision with the maximum count for each venue
max_toss_decision <- venue_toss_count %>%
  group_by(venue) %>%
  filter(count == max(count))

# Created a stacked bar chart
ggplot(venue_toss_count, aes(x = venue, y = count, fill = toss_decision)) +
  geom_bar(stat = "identity") +
  geom_text(data = venue_toss_count, aes(label = count), vjust = -0.5) +
  labs(title = "Venues Based on Highest Matches and Toss Decision",
       x = "Venue",
       y = "Count",
       fill = "Toss Decision") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# Read the dataset
ipl_season_winners <- read.csv("Winner Data.csv")

# Displayed the first few rows of the dataset
head(ipl_season_winners)

# Summary statistics
summary(ipl_season_winners)

#Frequency of Season Winners
# Counted the number of times each team won the IPL season
winner_counts <- ipl_season_winners %>%
  group_by(Winner) %>%
  summarize(win_count = n())

# Created a bar chart for visualization
ggplot(winner_counts, aes(x = Winner, y = win_count, fill = Winner)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of IPL Season Wins by Team",
       x = "Team",
       y = "Number of Wins") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Counted the number of times each team won the IPL season
winner_counts <- ipl_season_winners %>%
  group_by(Winner) %>%
  summarize(win_count = n())

# Created a heatmap
ggplot(winner_counts, aes(x = 1, y = Winner, fill = win_count)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("lightblue", "steelblue", "darkblue"), na.value = "white") +
  labs(title = "IPL Season Winners Heatmap",
       x = NULL,
       y = "Team",
       fill = "Number of Wins") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Hide x-axis label
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.text.y = element_text(size = 10),  # Adjust y-axis label size
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))


#Bar Chart for Runner Up
# Checked the column names
colnames(ipl_season_winners)

# Replaced spaces in column names with underscores
colnames(ipl_season_winners) <- make.names(colnames(ipl_season_winners))

# Counted the occurrences of each team as Runner Up
runner_up_counts <- ipl_season_winners %>%
  group_by(Runner.Up) %>%
  summarize(count = n())

# Create a bar chart
ggplot(runner_up_counts, aes(x = Runner.Up, y = count, fill = Runner.Up)) +
  geom_bar(stat = "identity") +
  labs(title = "Runner Up Analysis",
       x = "Team",
       y = "Number of Times as Runner Up") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Bar Chart for Player of the Match
# Replaced spaces in column names with underscores
colnames(ipl_season_winners) <- make.names(colnames(ipl_season_winners))

# Counted the occurrences of each player as Player of the Match
player_match_counts <- ipl_season_winners %>%
  group_by(Player.of.the.Match) %>%
  summarize(count = n())

# Created a bar chart
ggplot(player_match_counts, aes(x = Player.of.the.Match, y = count, fill = Player.of.the.Match)) +
  geom_bar(stat = "identity") +
  labs(title = "Player of the Match Analysis",
       x = "Player",
       y = "Number of Awards") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Bar Chart for Player of the Series
# Replaced spaces in column names with underscores
colnames(ipl_season_winners) <- make.names(colnames(ipl_season_winners))

# Counted the occurrences of each player as Player of the Series
player_series_counts <- ipl_season_winners %>%
  group_by(Player.of.the.Series) %>%
  summarize(count = n())

# Created a bar chart
ggplot(player_series_counts, aes(x = Player.of.the.Series, y = count, fill = Player.of.the.Series)) +
  geom_bar(stat = "identity") +
  labs(title = "Player of the Series Analysis",
       x = "Player",
       y = "Number of Awards") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))