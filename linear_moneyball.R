#Time for Money!!

##Building a better offensive metric for baseball"
library(Lahman)
fit <- Teams%>%
  filter(yearID %in% 1961:2001)%>%
  mutate(BB = BB/G, HR = HR/G, R = R/G)%>%
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = T)

##To build a multivariable linear model we have to assume that all
#Variables are jointly normal:
#If we pick any of these variables and hold the other four fixed,
#the relationship with outcome is linear.
fit <- Teams%>%
  filter(yearID %in% 1961:2001)%>%
  mutate(BB = BB/G, HR = HR/G, R = R/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G)%>%
  lm(R ~ BB + singles+ doubles+ triples+ HR, data = .)
coefs <- tidy(fit, conf.int = T)

coefs

#Predicting our model on next year data (2002):
Teams%>%
  filter(yearID %in% 2002)%>%
  mutate(BB = BB/G, HR = HR/G, R = R/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G)%>%
  mutate(R_hat = predict(fit, newdata = .))%>%
  ggplot(aes(R_hat, R))+geom_point()+geom_text(aes(label = teamID))+
  geom_abline(intercept = 0, slope = 1)

#Comparing both models we can see there is a good fit between them!!


pa_per_game <- Batting %>% filter(yearID %in% 2002)%>%
  group_by(teamID)%>%
  summarise(pa_per_game = sum(AB+BB)/max(G))%>%
  .$pa_per_game%>%
  mean

players <-  Batting %>% filter(yearID %in% 1999:2001)%>%
  group_by(playerID)%>%
  mutate(PA = BB+AB)%>%
  summarise(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G, HR = sum(HR)/G, R = sum(R)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA))%>%
  filter(PA >= 300)%>%
  select(-G)%>%
  mutate(R_hat = predict(fit, newdata = .))
players %>%
  ggplot(aes(R_hat))+geom_histogram(binwidth = 0.5, color = "black")
players <- Salaries%>%
  filter(yearID == 2002)%>%
  select(playerID, salary)%>%
  right_join(players, by = "playerID")

players <- Fielding%>%
  filter(yearID == 2002)%>%
  filter(!POS %in% c("OF", "P"))%>%
  group_by(playerID)%>%
  top_n(1, G)%>%
  filter(row_number(G)==1)%>%
  ungroup()%>%
  select(playerID, POS)%>%
  right_join(players, by = "playerID")%>%
  filter(!is.na(POS) & !is.na(salary))
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut)%>%
  right_join(players, by = "playerID")

players %>%
  select(nameFirst, nameLast, POS, salary, R_hat)%>%
  arrange(desc(R_hat))%>%
  top_n(10)

players %>%
  ggplot(aes(x = salary, y = R_hat, col = POS))+geom_point()+
  scale_x_log10()

#Exercicio:
ex <- data.frame(team = c("A","B"), BB = c(2,1), singles = c(4,6),
                 doubles = c(1,2), triples = c(0,1), HR = c(1,0))
predict(fit, newdata = ex)


#Sophomore Slump

playerInfo <- Fielding %>%
  group_by(playerID)%>%
  arrange(desc(G))%>%
  slice(1)%>%
  ungroup()%>%
  left_join(Master, by = "playerID")%>%
  select(playerID, nameFirst, nameLast, POS)

ROY <- AwardsPlayers %>%
    filter(awardID == "Rookie of the Year")%>%
  left_join(playerInfo, by = "playerID")%>%
  rename(rookie_year = yearID)%>%
  right_join(Batting, by = "playerID")%>%
  mutate(AVG = H/AB)%>%
  filter(POS != "P")
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1)%>%
  group_by(playerID)%>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore"))%>%
  filter(n()==2)%>%
  ungroup()%>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

ROY <- ROY %>% spread(rookie, AVG)%>%arrange(desc(rookie))
ROY


