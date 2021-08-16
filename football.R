library(tidyverse)

df <- read.csv("Foot_Ball.csv")
head(df)

# nationalities of players
(natio <- df %>%
  group_by(Nationality) %>%
  summarize(count = n()))

natio <- df %>%
  group_by(Nationality) %>%
  summarize(count = n()) %>%
  filter(count>=100)

ggplot(natio, aes(x = count, fct_reorder(Nationality, count), color = "Red")) + 
  scale_x_continuous(breaks = seq(100, 2000, by = 100))+
  geom_point() +
  labs(
    title = paste("NATIONALITIES OF WORLD FOOTBALL PLAYERS"),
    subtitle = paste("-MORE THAN 100 PLAYRES UPTO 2020-"),
    caption = paste("out of 164 nationalities"),
    x = "Number of playres", y = "Nationalities"
  )

# Preferred Foot of the players
df %>%
  group_by(Preferred.Foot) %>%
  summarize(count = n())

# the working rate according to  Skill Moves of the players
ggplot(df) +
  geom_bar(
    mapping = aes(x = Work.Rate, fill = Preferred.Foot)) +
  facet_grid(Skill.Moves ~  Preferred.Foot) + 
  scale_y_continuous(breaks = seq(0, 5000, by = 250)) +
  labs(
    title = paste("Working rate according to Skill Moves of the playres"),
    subtitle = paste("--separatly preferred foot"),
     y = "Players"
  ) +
  coord_flip() +
  theme_grey()
  
  
# Wages paying of clubs (by EURO *K)
wagess <- read_csv("Foot_Ball.csv", col_types = cols(Wage = col_number()))
(wag_clubs <- wagess %>%
  count(Club, wt = Wage))

# Higher wages paying clubs (more than EURO2500K)
(wag_clubs <- wagess %>%
  count(Club, wt = Wage, sort = TRUE) %>%
  filter(n>2500))

ggplot(wag_clubs, aes(fct_reorder(Club, n), y = n)) +
  geom_bar(stat = "identity", fill = "lime green") +
  geom_text(aes(label = n), stat = "identity", vjust = 3) +
  labs(
    title = paste("Higher wages paying clubs (more than $2500K)"),
    y = "Wages (EURO *K)", x = "Clubs") +
  coord_polar()+
  theme_gray()

# Best ratings of Goal Keeping Performance of players

(df_gk <- wagess %>%
  filter(GKDiving >= 80, 
         GKHandling >= 80, 
         GKKicking >= 80, 
         GKPositioning >= 80, 
         GKReflexes >= 80
         ) %>%
  select(Name, 
         Nationality,
         GKDiving,
         GKHandling,
         GKKicking,
         GKPositioning,
         GKReflexes,
         Wage
         ))

# Visualization
names <- rep(df_gk[[1]], times = 5)
nationalities <- rep(df_gk[[2]], times = 5)
figures <- rep(
  c("GKDiving", "GKHandling", "GKKicking", "GKPositioning", "GKReflexes"),
  each = 7
)
ratings_gk <- c(df_gk$GKDiving, df_gk$GKHandling, df_gk$GKKicking, df_gk$GKPositioning,
             df_gk$GKReflexes)

new_df_gk <- data.frame(names, nationalities, figures, ratings_gk)

ggplot(new_df_gk, aes(x = names, y = ratings_gk, fill = figures)) +
  geom_bar(stat = "identity", position = "dodge", width = 1) +
  scale_fill_manual(values = rainbow(5))+
  geom_text(new_df_gk, mapping = aes(label = ratings_gk), 
            stat = "identity", position_dodge(width = 1), hjust = 1) +
  facet_grid(~nationalities) + 
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(
    title = paste("Best figures of Goal Keeping Performance of players"),
    y = "Performance Ratings", x = "Player")+
  coord_polar()
  
# Best ratings of shots Performance of players 
# In here we can see some difference positions and also clubs

(df_shots <- df %>%
  filter(HeadingAccuracy >= 75, ShortPassing >=80,
         LongPassing >= 80, LongShots >= 80, ShotPower >=80) %>%
  select( Name, Nationality, Position, Club, HeadingAccuracy, ShortPassing,
          LongPassing, LongShots, ShotPower))

# Visualization

names_st <- rep(df_shots[[1]], times = 5)
nationalities_st <- rep(df_shots[[2]], times = 5)
position <- rep(df_shots[[3]], times = 5)
clubs <- rep(df_shots[[4]], times = 5)
rates <- rep(
  c("HeadingAccuracy", "ShortPassing", "LongPassing", "LongShots", "ShotPower"),
  each = 8  )
ratings_st <- c(df_shots$HeadingAccuracy, df_shots$ShortPassing, 
                df_shots$LongPassing, df_shots$LongShots, df_shots$ShotPower)

new_df_shots <- data.frame(names_st, nationalities_st, 
                           position, clubs, rates, ratings_st)

ggplot(new_df_shots, aes(x = names_st, y = ratings_st, fill = rates))+
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("HeadingAccuracy" = "maroon1",
                               "ShortPassing" = "mediumorchid3",
                               "LongPassing" = "steelblue4 ",
                               "LongShots" = "springgreen2",
                               'ShotPower' = "turquoise")) +
  geom_text(new_df_shots, mapping = aes(label = ratings_st), 
            stat = "identity", position_dodge(width = 1), hjust = 7) +
  facet_grid(~nationalities_st) + 
  scale_y_continuous(breaks = seq(0, 110, by = 10)) +
  labs(
    title = paste("Best ratings of shots Performance of players"),
    y = "Performance Ratings", x = "Player")+
  coord_flip()+
  theme_classic(base_size = 11, base_line_size = 0.8)
           
# Top ratings of Finishing, Penalties and FKAccuracy
(df_rat <- df %>%
    filter(Finishing >= 80,Penalties >= 80, FKAccuracy >= 80) %>%
    select( Name, Nationality, Position, Club, Finishing, Penalties, FKAccuracy))

ggplot(df_rat) +
  geom_line(aes(fct_reorder(Name, Finishing), y = Finishing,group = 1, color = "Finishing"), size = 1) +
  geom_point(aes(x = Name, y = Finishing, color = "Finishing"), size = 8, stat = "identity") +
  geom_text(df_rat, mapping = aes(x = Name, y = Finishing, label = Finishing), 
            stat = "identity", hjust = 2) +
  geom_point(aes(x = Name, y = Penalties, color = "Penalties"), size = 5, stat = "identity")+
  geom_point(aes(x = Name, y = FKAccuracy, color = "FKAccuracy"), size = 5, stat = "identity") +
  scale_y_continuous(breaks = seq(80, 100, by = 1)) +
  labs(
    title = paste("Top ratings of Finishing, Penalties and FKAccuracy"),
    y = "Ratings", x = "Player" ) +
  theme_bw()

# Defensive Performance
(df_diff <- df %>%
    filter(Aggression >= 85, Interceptions >= 85, 
           Marking >= 85, StandingTackle >= 85, SlidingTackle >= 85) %>%
    select( Name, Nationality, Position, Club, Aggression, Interceptions, 
            Marking, StandingTackle, SlidingTackle))

ggplot(df_diff, mapping = aes(fct_reorder(Name, Aggression), y = Aggression, group = 1)) +
  geom_point(aes(color = "Aggression"), size = 7) +
  geom_line(color = "red", size = 1) +
  geom_point(aes(x = Name, y = Interceptions, color = "Interceptions"), size = 4)+
  geom_point(aes(x = Name, y = Marking, shape = "Marking"), size = 5) +
  scale_y_continuous(breaks = seq(85, 100, by = 1)) +
  labs(
    title = paste("Top Defensive Performance"),
    subtitle = paste("-Aggression, Interceptions & Marking"),
    y = "Ratings", x = "Player" ) +
  theme_linedraw() +
  coord_polar()
  

# Tackling performance
ggplot(df_diff) +
  geom_line(aes(fct_reorder(Name, StandingTackle), y = StandingTackle,group = 1, color = "StandingTackle"), size = 1) +
  geom_point(aes(fct_reorder(Name, StandingTackle), y = StandingTackle, color = "StandingTackle"), size = 3) +
  geom_line(aes(x = Name, y = SlidingTackle, group = 1,color = "SlidingTackle"), size = 1) +
  geom_point(aes(x = Name, y = SlidingTackle, color = "SlidingTackle"), size = 3) +
  scale_y_continuous(breaks = seq(85, 100, by = 1)) +
  labs(
    title = paste("Top Tuckling performance"),
    subtitle = paste("-Standing & Sliding"),
    y = "Ratings", x = "Player" ) +
  theme_linedraw() +
  coord_polar()
  
# Top common performance ratings of players
(df_com <- df %>%
    filter(Volleys >= 75, Crossing >= 75, Curve >= 75, Dribbling >= 75, 
           BallControl >= 75, Acceleration >= 75, SprintSpeed >= 75, Agility >= 75,
           Reactions >= 75, Balance >= 70, 
           Positioning >= 75, Composure >= 75, Vision >= 75) %>%
    select( Name, Nationality, Club, Volleys, Crossing, Curve, Dribbling, 
            BallControl, Acceleration, SprintSpeed, Agility,
            Reactions, Balance, 
            Positioning, Composure, Vision))

ggplot(df_com) +
  geom_line(aes(fct_reorder(Name, Vision), y = Vision,group = 1, shape = "Vision"), size = 1) +
  geom_point(aes(fct_reorder(Name, Vision), y = Vision, shape = "Vision"), size = 3) +
  geom_point(aes(x = Name, y = Volleys, color = "Volleys"), size = 3) +
  geom_point(aes(x = Name, y = Crossing, color = "Crossing"), size = 3) +
  geom_point(aes(x = Name, y = Curve, color = "Curve"), size = 3) +
  geom_point(aes(x = Name, y = Dribbling, color = "Dribbling"), size = 5) +
  scale_y_continuous(breaks = seq(75, 100, by = 1)) +
  labs(
    title = paste("Top common performance ratings of players"),
    subtitle = paste("-Vision, Volleys, Crossing, Curve, Dribbling"),
    y = "Ratings", x = "Player" ) +
  theme_get() +
  coord_flip()
  

























