library('ggplot2')
library('readr')
library('ggthemes')
library('dplyr')
library('coefplot')
library('magrittr')
library('extrafont')

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

data <- read_csv('data/happiness.csv', col_names = T, na = c("#N/A", "#DIV/0!", ""), trim_ws = T)
men.cats <- colnames(select(data, contains(".M")))
women.cats <- colnames(select(data, contains(".W")))

# HAPPINESS INDEX INCREASES WITH GDP
# PEARSON'S CORRELATION (DISCRETE VARIABLES)
p1.cor <- with(data, cor.test(Rank, SGDPPC))$estimate[[1]] #p-value = 2.2e-16
p1 <- ggplot(data, aes(x = SGDPPC / 1000, y = Rank)) + 
  geom_point(size = 2.5, color = '#2c3e50', alpha = 0.8) + 
  geom_smooth(method = c('lm'), se = F, color = '#27ae60', size = 1.5) + 
  xlab('GDP per Capita (thousands USD)') +
  ylab('Happiness Rank') +
  scale_y_continuous(trans = 'reverse', limits = c(156, 0), breaks = c(156, 125, 100, 75, 50, 25, 1), labels = c('Least Happy - 156th', '125th', '100th', '75th', '50th', '25th', 'Most Happy - 1st')) +
  ggtitle('Wealthier countries are happier.') +
  annotate('text', x = 50, y = 100, label = 'R = 0.7165') +
  theme_hc()
p1

# HAPPINESS INDEX INCREASES WITH NUMBER OF TOTAL SPORTS TEAMS [MAIN ITEM]
index_averages <- c()
for(i in 1 : max(data$Total)) {
  sub <- data[data$Total == i, 'Rank']
  index_averages <- append(index_averages, mean(sub$Rank))
}
index_averages <- data.frame(teams = 1 : max(data$Total), average = index_averages)
p2.cor <- with(index_averages, cor.test(teams, average))$estimate[[1]] #p-value = 2.8e-06
p2 <- ggplot(index_averages, aes(x = teams, y = average)) + 
  geom_bar(fill = '#2c3e50', stat = 'identity') +
  geom_smooth(method = 'lm', color = '#27ae60', size = 1.5, se = F) +
  xlab('# International Teams') + ylab('Mean Happiness Rank') +
  ggtitle('Countries with more sports teams tend to be happier.') +
  scale_y_continuous(trans = 'reverse', limits = c(160, 0), breaks = c(156, 125, 100, 75, 50, 25, 0), labels = c('Least Happy - 156th', '125th', '100th', '75th', '50th', '25th', 'Most Happy - 1st')) +
  scale_x_continuous(breaks = seq(1, 14, 1)) + 
  annotate('text', x = 11, y = 100, label = 'R = 0.9217') +
  theme_hc()
p2

# GDP CORRELATES WITH THE TOTAL NUMBER OF TEAMS --> # of sports teams is a representative of GDP
gdp_averages <- c()
for(i in 1 : max(data$Total)) {
  sub <- data[data$Total == i, 'SGDPPC']
  gdp_averages <- append(gdp_averages, mean(sub$SGDPPC))
}
gdp_averages <- data.frame(teams = 1 : max(data$Total), average = gdp_averages)
p3.cor <- with(gdp_averages, cor.test(teams, average))$estimate[[1]]#p-value = 3.2e-06
p3 <- ggplot(gdp_averages, aes(x = average / 1000, y = teams)) +
  geom_point(size = 2, alpha = 1, color = '#2c3e50') +
  geom_smooth(method = 'lm', se = F, color = '#27ae60', size = 1.5) +
  xlab('GDP per Capita (thousands USD)') +
  ylab('# International Sports Teams') +
  ggtitle('Wealthier countries have more sports teams.') +
  annotate('text', x = 40, y = 8, label = 'R = 0.8739') +
  theme_hc()
p3

# HAPPINESS INDEX IS HIGHER WITH ABOVE AVERAGE WOMEN'S REPRESENTATION
separator <- mean(data$W.Rat, na.rm = T)
no.women <- data %>%
  filter(W.Rat <= separator)
women <- data %>%
  filter(W.Rat > separator)

no.women.median <- no.women$Rank %>% # median with women = 74, median without = 102
  median()
women.median <- women$Rank %>%
  median()
no.women$Rank %<>% as.numeric
women$Rank %<>% as.numeric
no.women.avg <- no.women$Rank %>% # average with women = 74.6667, average without = 86.96
  mean(na.action = na.omit)
women.avg <- women$Rank %>%
  mean(na.action = na.omit)

# FIRST THIRD
first.third.no.women <- data %>%
  filter(W.Rat <= (separator) & Rank <= 52)

first.third.women <- data %>%
  filter(W.Rat > (separator) & Rank <= 52)

fir.no.women.median <- first.third.no.women$Rank %>% # median with women = 27, median without = 26
  median()
fir.women.median <- first.third.women$Rank %>%
  median()
first.third.no.women$Rank %<>% as.numeric
first.third.women$Rank %<>% as.numeric
fir.no.women.avg <- first.third.no.women$Rank %>% # average with women = 27.08571, average without = 25.29412
  mean(na.action = na.omit)
fir.women.avg <- first.third.women$Rank %>%
  mean(na.action = na.omit)

# MIDDLE THIRD
middle.third.no.women <- data %>%
  filter(W.Rat <= separator & Rank > 52 & Rank < 104)

middle.third.women <- data %>%
  filter(W.Rat > separator & Rank > 52 & Rank < 104)

mid.no.women.median <- middle.third.no.women$Rank %>% # median with women = 77.5, median without = 86
  median()
mid.women.median <- middle.third.women$Rank %>%
  median()
middle.third.no.women$Rank %<>% as.numeric
middle.third.women$Rank %<>% as.numeric
mid.no.women.avg <- middle.third.no.women$Rank %>% # average with women = 77.3333, average without = 84
  mean(na.action = na.omit)
mid.women.avg <- middle.third.women$Rank %>%
  mean(na.action = na.omit)

# FINAL THIRD
final.third.no.women <- data %>%
  filter(W.Rat <= separator & Rank >= 104)

final.third.women <- data %>%
  filter(W.Rat > separator & Rank >= 104)

fin.no.women.median <- final.third.no.women$Rank %>% # median with women = 132.5, median without = 128
  median()
fin.women.median <- final.third.women$Rank %>%
  median()
final.third.no.women$Rank %<>% as.numeric
final.third.women$Rank %<>% as.numeric
fin.no.women.avg <- final.third.no.women$Rank %>% # average with women = 130.1429, average without = 129.84
  mean(na.action = na.omit)
fin.women.avg <- final.third.women$Rank %>%
  mean(na.action = na.omit)
med.gender.df <- data.frame(third = c('Top Third\n(1st - 52nd)', 'Top Third\n(1st - 52nd)', 'Middle Third\n(53rd - 103rd)', 'Middle Third\n(53rd - 103rd)', 'Bottom Third\n(104th - 156th)', 'Bottom Third\n(104th - 156th)'),
                            women = c('Diverse', 'Male-Dominated', 'Diverse', 'Male-Dominated', 'Diverse', 'Male-Dominated'),
                            quantity = c(fir.women.median, fir.no.women.median, mid.women.median, mid.no.women.median, fin.women.median, fin.no.women.median))
p6 <- ggplot(med.gender.df) + geom_bar(aes(x = women, y = quantity, fill = women), position = position_dodge(preserve = 'single'), stat = 'identity') +
  facet_grid(~third) +
  ylab('Median Happiness Rank') +
  theme_hc() +
  scale_y_continuous(trans = 'reverse', limits = c(160, 0), breaks = c(156, 125, 100, 75, 50, 25, 1), labels = c('Worst - 156th', '125th', '100th', '75th', '50th', '25th', 'Best - 1st')) +
  scale_fill_manual(values = c('#27ae60', '#2c3e50')) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank())
p6

# MALE-DOMINANCE IN SPORTS CORRELATES TO LOWER HAPPINESS RANKINGS --> EQUALITY
male.rank.average <- c()
ratios <- unique(data$M.Rat)[order(unique(data$M.Rat))]
ratios <- ratios[!is.na(ratios)]
for(i in ratios) {
  sub <- data$Rank[data$M.Rat == i]
  male.rank.average <- append(male.rank.average, mean(sub, na.rm = T))
}
male.rank.average <- data.frame(ratio = ratios, average = male.rank.average)
p4.cor <- with(male.rank.average, cor.test(ratio, average))$estimate[[1]]#p-value = 3.2e-06
p4 <- ggplot(male.rank.average, aes(x = round(100 * ratio, 2), y = average)) +
  geom_point(size = 2, alpha = 1, color = '#2c3e50') +
  geom_smooth(method = 'lm', se = F, color = '#27ae60', size = 1.5) +
  scale_y_continuous(trans = 'reverse', limits = c(156, 0), breaks = c(156, 100, 50, 1), labels = c('Least Happy - 156th', '100th', '50th', 'Most Happy - 1st')) +
  xlab('Male Representation Ratio (%)') + ylab('Happiness Index Position') + 
  annotate('text', x = 80, y = 130, label = 'R = -0.2825') +
  ggtitle('Happiness correlates with gender representation.') + theme_hc()
p4

# INVESTIGATING PERFORMANCE OF SPORTS TEAMS WITH POSITION ON WORLD HAPPINESS INDEX
# UNWEIGHTED AVERAGE (irrespective of total number of teams)
data$U.Avg <- data %>%
  select(c(men.cats, women.cats)) %>%
  rowMeans(na.rm = T)
p5 <- ggplot(data, aes(x = U.Avg, y = Rank)) + 
  geom_point(size = 2, alpha = 1, color = '#2c3e50') + 
  geom_smooth(method = 'lm', se = F, color = '#27ae60', size = 2) +
  scale_y_continuous(trans = 'reverse', breaks = c(156, 125, 100, 75, 50, 25, 1), labels = c('Least Happy - 156th', '125th', '100th', '75th', '50th', '25th', 'Most Happy - 1st')) +
  scale_x_continuous(trans = 'reverse', limits = c(156, 0), breaks = c(156, 125, 100, 75, 50, 25, 1), labels = c('Worst - 156th', '125th', '100th', '75th', '50th', '25th', 'Best - 1st')) +
  xlab('Mean Team Performance Standings') + ylab('Happiness Index Rank') +
  annotate('text', x = 125, y = 25, label = 'R = 0.3943') +
  ggtitle('Countries with better teams tend to be happier.') +
  theme_hc()
p5
p5.cor <- with(data, cor.test(Rank, U.Avg, method = 'kendall'))$estimate[[1]] #p-value = 5.1e-15

# BEST MEN'S/WOMEN'S/AGGREGATE NATIONS
data$W.Avg <- data %>%
  select(c(women.cats)) %>%
  rowMeans(na.rm = T)
data$M.Avg <- data %>%
  select(c(men.cats)) %>%
  rowMeans(na.rm = T)
best.agg <- data[order(data$U.Avg), c("Country", "Rank", "U.Avg", "Total")]
best.wom <- data[order(data$W.Avg), c("Country", "Rank", "W.Avg", "Total")]
best.men <- data[order(data$M.Avg), c("Country", "Rank", "M.Avg", "Total")]

best.agg[best.agg$Country == 'United States', 'Country'] <- 'USA'
best.wom[best.wom$Country == 'United States', 'Country'] <- 'USA'
best.men[best.men$Country == 'United States', 'Country'] <- 'USA'

best.agg[best.agg$Country == 'England', 'Country'] <- 'United Kingdom'
best.wom[best.wom$Country == 'England', 'Country'] <- 'United Kingdom'
best.men[best.men$Country == 'England', 'Country'] <- 'United Kingdom'

colnames(best.agg) <- c('region', 'Rank', 'Aggregate Average', 'Total Teams')
colnames(best.wom) <- c('region', 'Rank', 'Aggregate Average', 'Total Teams')
colnames(best.men) <- c('region', 'Rank', 'Aggregate Average', 'Total Teams')

world.map <- map_data('world')
world.map <- world.map[!(world.map$region == 'Antarctica'), ]
ranks <- data[, c('Country', 'Rank')]
ranks[ranks$Country == 'United States', 'Country'] <- 'USA'
colnames(ranks) <- c('region', 'rank')
hap.map <- left_join(world.map, ranks, by = 'region')
p7 <- ggplot(hap.map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = rank), color = 'black', size = 0.1) + 
  scale_fill_gradient(high = '#e74c3c', low = '#27ae60', breaks = c(2, 50, 100, 156), labels = c('Most Happy - 1st', '50th', '100th', 'Least Happy - 156th')) +
  labs(fill = '') +
  ggtitle('Global Happiness Rankings for 156 countries.') +
  theme_hc() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.width = unit(2, 'cm'),
        legend.key.size = unit(1, 'cm'),
        legend.title.align = 1)

p7

p8 <- ggplot(best.agg, aes(x = `Total Teams`)) + 
  geom_histogram(size = 1, alpha = 1, fill = '#2c3e50', color = '#27ae60', binwidth = 1) +
  scale_x_continuous(limits = c(1, 14), breaks = 1 : 14, labels = 1 : 14) +
  xlab('# International Teams') + ylab('Frequency') +
  ggtitle('Histogram of sports team counts.') +
  theme_hc()
p8
