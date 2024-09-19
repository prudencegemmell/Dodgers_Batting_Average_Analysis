install.packages("Lahman")

Batting <- read.csv(file = "C:/Users/prude/Desktop/core/Batting.csv", header = TRUE)
BattingPost <- read.csv(file = "C:/Users/prude/Desktop/core/BattingPost.csv", header = TRUE)

library("dplyr")

Dodgers_batting <- Batting %>% filter(teamID == "BRO" | teamID == "LAN")
Dodgers_batting_post <- BattingPost %>% filter(teamID == "BRO" | teamID == "LAN")

tSeason_batting_average <- c(Dodgers_batting$H / Dodgers_batting$AB)
tPost_batting_average <- c(Dodgers_batting_post$H / Dodgers_batting_post$AB)

my_data <- data.frame(
  group = c(rep("Dodgers Season Batting Average", each = nrow(Dodgers_batting)), 
            rep("Dodgers Post Season Batting Average", each = nrow(Dodgers_batting_post))),
  batting_average = c(tSeason_batting_average, tPost_batting_average)
)

group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(batting_average, na.rm = TRUE),
    sd = sd(batting_average, na.rm = TRUE)
  )

t.test(batting_average ~ group, data = my_data, var.equal = TRUE, alternative = "greater")

install.packages("plotly")
library("plotly")

Season_batting_average <- data.frame(
  year = Dodgers_batting$yearID,
  season_batting_average = (Dodgers_batting$H / Dodgers_batting$AB)
)

batting_average_vs_year <- group_by(Season_batting_average, year) %>%
  summarise(
    mean = mean(season_batting_average, na.rm = TRUE)
  )

Post_batting_average <- data.frame(
  year = Dodgers_batting_post$yearID,
  post_batting_average = (Dodgers_batting_post$H / Dodgers_batting_post$AB)
)

post_batting_average_vs_year <- group_by(Post_batting_average, year) %>%
  summarise(
    mean = mean(post_batting_average, na.rm = TRUE)
  )

p1 <- plot_ly() %>%
  add_trace(data = batting_average_vs_year, x = ~year, 
            y = ~mean, type = "scatter", mode = "markers", name = "Season") %>%
  add_trace(data = post_batting_average_vs_year, x = ~year, 
            y = ~mean, type = "scatter", mode = "markers", name = "Post Season") %>%
  add_lines(data = batting_average_vs_year, x = ~year, 
            y = predict(lm(batting_average_vs_year$mean ~ batting_average_vs_year$year)),
            name = "Season Averages") %>%
  add_lines(data = post_batting_average_vs_year, x = ~year, 
            y = predict(lm(post_batting_average_vs_year$mean ~ post_batting_average_vs_year$year)),
            name = "Post Season Averages") %>%
  layout(title = "Dodgers Batting Averages over Time (season vs post-season)", 
         xaxis = list(title = "Year"), yaxis = list(title = "Batting Average"))

print(p1)

Teams <- read.csv(file = "C:/Users/prude/Desktop/core/Teams.csv", header = TRUE)
Dodgers <- Teams %>% filter(teamID == "BRO" | teamID =="LAN")

Season_winning_percentage_vs_homeruns <- data.frame(
  winning_percentage = (Dodgers$W / Dodgers$G),
  total_homeruns = Dodgers$HR
)

p2 <- plot_ly(data = Season_winning_percentage_vs_homeruns, x = ~winning_percentage, 
              y = ~total_homeruns, type = "scatter", mode = "markers", showlegend = FALSE) %>%
  add_lines(data = Season_winning_percentage_vs_homeruns, x = ~winning_percentage, 
            y = predict(lm(Season_winning_percentage_vs_homeruns$total_homeruns ~ Season_winning_percentage_vs_homeruns$winning_percentage))) %>%
  layout(title = "Correlation between Homeruns and Wins?",
         xaxis = list(title = "Winning Percentage"), yaxis = list(title = "Total Homeruns"))

print(p2)