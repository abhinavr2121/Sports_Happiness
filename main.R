library('ggplot2')
library('readr')
library('ggthemes')
library('dplyr')
library('magrittr')

data <- read_csv('data/happiness.csv', col_names = T, na = c("#N/A", "#DIV/0!", ""), trim_ws = T)
men.cats <- colnames(select(data, contains(".M")))
women.cats <- colnames(select(data, contains(".W")))

# HAPPINESS INDEX INCREASES WITH GDP
p1 <- ggplot(data, aes(x = SGDPPC, y = Rank, color = Total)) + 
  geom_point(size = 4) + 
  geom_smooth(method = c('lm'), se = F, color = 'green', size = 3) +
  geom_smooth(method = c('auto'), se = F, color = 'blue', size = 3) +
  scale_color_continuous(low = 'black', high = 'red')
p1
# KENDALL'S TAU CORRELATION (DISCRETE VARIABLES)
p1.cor <- with(data, cor.test(Rank, SGDPPC, method = 'kendall'))$estimate[[1]] #p-value = 2.2e-16

# HAPPINESS INDEX INCREASES WITH NUMBER OF TOTAL SPORTS TEAMS [MAIN ITEM]
p2 <- ggplot(data, aes(x = Total, y = Rank)) + 
  geom_point(size = 2, alpha = 0.5, color = 'black') +
  geom_smooth(method = 'lm', se = F, color = 'darkgreen', size = 1, linetype = 'dashed') +
  xlab('# Sports Teams') + ylab('Position on Happiness Index')
p2
# KENDALL'S TAU CORRELATION (LOTS OF TIES)
p2.cor <- with(data, cor.test(Rank, Total, method = 'kendall'))$estimate[[1]] #p-value = 7.6e-09

# GDP CORRELATES WITH THE TOTAL NUMBER OF TEAMS --> # of sports teams is a representative of GDP
p3 <- ggplot(data, aes(x = SGDPPC, y = Total)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', se = F, color = 'green', size = 3) +
  geom_smooth(method = 'loess', se = F, color = 'blue', size = 3)
p3
p3.cor <- with(data, cor.test(SGDPPC, Total), method = 'kendall')$estimate[[1]]#p-value = 3.2e-06

# HAPPINESS INDEX IS HIGHER WITH AT LEAST (ONE-THIRD) OF TOTAL AS WOMEN'S TEAM
# TODO: run this on a subset of the middle third data
no.women <- data %>%
  filter(W.Rat <= (1/3))
women <- data %>%
  filter(W.Rat > (1/3))

no.women.median <- no.women$Rank %>% # median with women = 71, median without = 120
  median()
women.median <- women$Rank %>%
  median()
no.women$Rank %<>% as.numeric
women$Rank %<>% as.numeric
no.women.avg <- no.women$Rank %>% # average with women = 71.51261, average without = 102.1667
  mean(na.action = na.omit)
women.avg <- women$Rank %>%
  mean(na.action = na.omit)

# FIRST THIRD
first.third.no.women <- data %>%
  filter(W.Rat <= (1/3) & Rank <= 52)

first.third.women <- data %>%
  filter(W.Rat > (1/3) & Rank <= 52)

fir.no.women.median <- first.third.no.women$Rank %>% # median with women = 22.5, median without = 36.5
  median()
fir.women.median <- first.third.women$Rank %>%
  median()
first.third.no.women$Rank %<>% as.numeric
first.third.women$Rank %<>% as.numeric
fir.no.women.avg <- first.third.no.women$Rank %>% # average with women = 24.5, average without = 37.4
  mean(na.action = na.omit)
fir.women.avg <- first.third.women$Rank %>%
  mean(na.action = na.omit)

# MIDDLE THIRD
middle.third.no.women <- data %>%
  filter(W.Rat <= (1/3) & Rank > 52 & Rank < 104)

middle.third.women <- data %>%
  filter(W.Rat > (1/3) & Rank > 52 & Rank < 104)

mid.no.women.median <- middle.third.no.women$Rank %>% # median with women = 78.5, median without = 79.5
  median()
mid.women.median <- middle.third.women$Rank %>%
  median()
middle.third.no.women$Rank %<>% as.numeric
middle.third.women$Rank %<>% as.numeric
mid.no.women.avg <- middle.third.no.women$Rank %>% # average with women = 78.0, average without = 81.2
  mean(na.action = na.omit)
mid.women.avg <- middle.third.women$Rank %>%
  mean(na.action = na.omit)

# FINAL THIRD
final.third.no.women <- data %>%
  filter(W.Rat <= (1/3) & Rank >= 104)

final.third.women <- data %>%
  filter(W.Rat > (1/3) & Rank >= 104)

fin.no.women.median <- final.third.no.women$Rank %>% # median with women = 130, median without = 130
  median()
fin.women.median <- final.third.women$Rank %>%
  median()
final.third.no.women$Rank %<>% as.numeric
final.third.women$Rank %<>% as.numeric
fin.no.women.avg <- final.third.no.women$Rank %>% # average with women = 128.7, average without = 131.4
  mean(na.action = na.omit)
fin.women.avg <- final.third.women$Rank %>%
  mean(na.action = na.omit)

# MALE-DOMINANCE IN SPORTS CORRELATES TO LOWER HAPPINESS RANKINGS --> EQUALITY
p4 <- ggplot(data, aes(x = M.Rat, y = Rank, color = Total)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm', se = F, color = 'green', size = 3) +
  scale_color_continuous(low = 'black', high = 'red')
p4
p4.cor <- with(data, cor.test(M.Rat, Rank), method = 'kendall')$estimate[[1]]#p-value = 3.2e-06

# HAPPINESS INDEX IS HIGHER WITH THE EXISTENCE OF SPORTS TEAMS
no.teams <- data %>%
  filter(Total <= 2)
teams <- data %>%
  filter(Total > 2)

no.teams.median <- no.teams$Rank %>% # median with sports = 75, median without = 127
  median()
teams.median <- teams$Rank %>%
  median()
no.teams$Rank %<>% as.numeric
teams$Rank %<>% as.numeric
no.teams.avg <- no.teams$Rank %>% # average with sports = 75.72414, average without = 115.0909
  mean(na.action = na.omit)
teams.avg <- teams$Rank %>%
  mean(na.action = na.omit)

# INVESTIGATING PERFORMANCE OF SPORTS TEAMS WITH POSITION ON WORLD HAPPINESS INDEX
# UNWEIGHTED AVERAGE (irrespective of total number of teams)
data$U.Avg <- data %>%
  select(c(men.cats, women.cats)) %>%
  rowMeans(na.rm = T)
p5 <- ggplot(data, aes(x = U.Avg, y = Rank, color = Total)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = 'lm', se = F, color = 'darkgreen', size = 2) +
  scale_color_continuous(low = 'black', high = 'red') + 
  xlab('Average position in sports rankings') + ylab('World Happiness Rank')
p5$labels$colour = '# Sports \nTeams'
p5
p5.cor <- with(data, cor.test(Rank, U.Avg, method = 'kendall'))$estimate[[1]] #p-value = 5.1e-15

# BEST MEN'S/WOMEN'S/AGGREGATE NATIONS
data$W.Avg <- data %>%
  select(c(women.cats)) %>%
  rowMeans(na.rm = T)
data$M.Avg <- data %>%
  select(c(men.cats)) %>%
  rowMeans(na.rm = T)
best.agg <- data[order(data$U.Avg)[1 : 10], c("Country", "Rank", "U.Avg", "Total")]
best.wom <- data[order(data$W.Avg)[1 : 10], c("Country", "Rank", "W.Avg", "Total")]
best.men <- data[order(data$M.Avg)[1 : 10], c("Country", "Rank", "M.Avg", "Total")]
