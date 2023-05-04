#---------------Library & Read-in Files----------------
library(tidyverse)
library(caret)
library(ggpubr)
library(randomForest)

setwd("C:/Users/minhk/Documents/GitHub/ML - Video games sales")
orig <- read.csv("vgsales.csv")

options(digits = 3)
set.seed(5)

#================= Meta-data ==================
# According to kaggle: https://www.kaggle.com/datasets/gregorut/videogamesales
# Rank - Ranking of overall sales
# Name - The games name
# Platform - Platform of the games release (i.e. PC,PS4, etc.)
# Year - Year of the game's release
# Genre - Genre of the game
# Publisher - Publisher of the game
# NA_Sales - Sales in North America (in millions)
# EU_Sales - Sales in Europe (in millions)
# JP_Sales - Sales in Japan (in millions)
# Other_Sales - Sales in the rest of the world (in millions)
# Global_Sales - Total worldwide sales.

#==================Variable Transformation ==================
range(orig$Year)
length(unique(orig$Year))

# Transform individual Years to Yr-Released 
orig <- orig %>%
  mutate(Year = as.numeric(Year),
         yr.released = ifelse(Year < 1985, "1980s",
                       ifelse(Year >=1985 & Year <1990,"1985s",
                       ifelse(Year >=1990 & Year <1995,"1990s",
                       ifelse(Year >=1995 & Year <2000,"1995s",
                       ifelse(Year >=2000 & Year <2005,"2000s",
                       ifelse(Year >=2005 & Year <2010,"2005s",
                       ifelse(Year >=2010 & Year <2015,"2010s",
                       ifelse(Year >=2015 & Year <=2020,"2015s","N/A")))))))))

orig <- orig %>%
  mutate(yr.released = ifelse(is.na(yr.released) == TRUE,"unknown",yr.released))

orig %>%
  group_by(yr.released) %>%
  count()

#---
# Publishers 
orig %>%
  select(Global_Sales,
         Publisher) %>%
  group_by(Publisher) %>%
  summarise(Total.sales = sum(Global_Sales)) %>%
  arrange(desc(Total.sales)) %>%
  head(30) %>%
  data.frame()

# publisher.rank: by global sales-  the top 5, top 10, top 15, 
#                                   top 20, other, and other
orig <- orig %>%
  mutate(publisher.rank = ifelse(Publisher == "Nintendo","top-5",
                          ifelse(Publisher == "Electronic Arts","top-5",
                          ifelse(Publisher == "Activision","top-5",
                          ifelse(Publisher == "Sony Computer Entertainment","top-5",
                          ifelse(Publisher == "Ubisoft","top-5",
                          ifelse(Publisher == "Take-Two Interactive","top-10",
                          ifelse(Publisher == "THQ","top-10",       
                          ifelse(Publisher == "Konami Digital Entertainment","top-10",
                          ifelse(Publisher == "Sega","top-10",
                          ifelse(Publisher == "Namco Bandai Games","top-10",
                          ifelse(Publisher == "Microsoft Game Studios","top-15",
                          ifelse(Publisher == "Capcom","top-15",
                          ifelse(Publisher == "Atari","top-15",
                          ifelse(Publisher == "Warner Bros. Interactive Entertainment","top-15",
                          ifelse(Publisher == "Square Enix","top-15",
                          ifelse(Publisher == "Disney Interactive Studios","top-20",
                          ifelse(Publisher == "Eidos Interactive","top-20",       
                          ifelse(Publisher == "LucasArts","top-20",
                          ifelse(Publisher == "Bethesda Softworks","top-20",
                          ifelse(Publisher == "Midway Games","top-20", 
                          ifelse(Publisher == "Acclaim Entertainment","top-25",
                          ifelse(Publisher == "Vivendi Games","top-25",
                          ifelse(Publisher == "SquareSoft","top-25",
                          ifelse(Publisher == "505 Games","top-25",
                          ifelse(Publisher == "Tecmo Koei","top-25",
                          ifelse(Publisher == "Codemasters","top-30",
                          ifelse(Publisher == "Virgin Interactive","top-30",
                          ifelse(Publisher == "Enix Corporation","top-30",
                          ifelse(Publisher == "Deep Silver","top-30",
                          ifelse(Publisher == "GT Interactive","top-30","other")
                          ))))))))))))))))))))))))))))))

orig %>%
  group_by(publisher.rank) %>%
  summarise(Total.sales = sum(Global_Sales),
            n = n(),
            mean.sales = mean(Global_Sales)) %>%
  arrange(desc(mean.sales))

#---
# check Platforms
length(unique(orig$Platform))

orig %>%
  group_by(Platform) %>%
  summarise(n = n(),
            mean = mean(Global_Sales),
            sum = sum(Global_Sales))%>%
  arrange(desc(sum)) %>%
  print(n = 32)

# the last platforms are very low => rename them to other
orig <- orig %>%
  mutate(Platform = ifelse(Platform == "SCD","Not-Popular",
                    ifelse(Platform == "NG","Not-Popular",
                    ifelse(Platform == "WS","Not-Popular",
                    ifelse(Platform == "TG16","Not-Popular",
                    ifelse(Platform == "3DO","Not-Popular",
                    ifelse(Platform == "GG","Not-Popular",
                    ifelse(Platform == "PCFX","Not-Popular",Platform))))))))

orig %>%
  group_by(Platform) %>%
  summarise(n = n(),
            mean = mean(Global_Sales),
            sum = sum(Global_Sales))%>%
  arrange(desc(sum)) %>%
  print(n = 32)

#===Genres
unique(orig$Genre)              # 12 genres
orig %>%
  group_by(Genre) %>%
  summarise(n = n(),
            mean = mean(Global_Sales),
            sum = sum(Global_Sales))%>%
  arrange(desc(sum)) %>%
  print(n = 12)                # Summarise by Genre

nrow(orig)           # there is 11620 rows ,
sum(is.na(orig))     # no missing data

#==================Data Visualization==================

#----Global sales
# by Platforms 
glo.plat <- orig %>%
  select(Global_Sales,
         Platform) %>%
  group_by(Platform) %>%
  summarise(sum = sum(Global_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Platform,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Global")

# by Released Year
glo.yr <- orig %>%
  select(Global_Sales,
         yr.released) %>%
  group_by(yr.released) %>%
  summarise(sum = sum(Global_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = yr.released,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(yr.released,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Global")

# by Genre
glo.genres <- orig %>%
  select(Global_Sales,
         Genre) %>%
  group_by(Genre) %>%
  summarise(sum = sum(Global_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Genre,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Genre,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Global")

# by Publishers
glo.pub <- orig %>%
  select(Global_Sales,
         publisher.rank) %>%
  group_by(publisher.rank) %>%
  summarise(sum = sum(Global_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = publisher.rank,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(publisher.rank,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Global")

# by top 10 publishers
glo.pus <- orig %>%
  select(Global_Sales,
         Publisher) %>%
  group_by(Publisher) %>%
  summarise(sum = sum(Global_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Publisher,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Global")

#----North America sales
# NA sales by Platforms 
na.plat <- orig %>%
  select(NA_Sales,
         Platform) %>%
  group_by(Platform) %>%
  summarise(sum = sum(NA_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Platform, sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("North America")

# NA Sales by Released Year
na.yr <- orig %>%
  select(NA_Sales,
         yr.released) %>%
  group_by(yr.released) %>%
  summarise(sum = sum(NA_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = yr.released,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(yr.released,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("North America")

# NA sales by Genre
na.genres <- orig %>%
  select(NA_Sales,
         Genre) %>%
  group_by(Genre) %>%
  summarise(sum = sum(NA_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Genre,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Genre,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("North America")

# by Publishers
na.pub <- orig %>%
  select(NA_Sales,
         publisher.rank) %>%
  group_by(publisher.rank) %>%
  summarise(sum = sum(NA_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = publisher.rank,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(publisher.rank,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("North America")

# by top 10 publishers
na.pus <- orig %>%
  select(NA_Sales,
         Publisher) %>%
  group_by(Publisher) %>%
  summarise(sum = sum(NA_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Publisher,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("North America")

#----European sales
# Europe sales by Platforms 
eu.plat <- orig %>%
  select(EU_Sales,
         Platform) %>%
  group_by(Platform) %>%
  summarise(sum = sum(EU_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Platform, sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Europe")

# EU Sales by Released Year
eu.yr <- orig %>%
  select(EU_Sales,
         yr.released) %>%
  group_by(yr.released) %>%
  summarise(sum = sum(EU_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = yr.released,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(yr.released,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Europe")

# EU sales by Genre
eu.genres <- orig %>%
  select(EU_Sales,
         Genre) %>%
  group_by(Genre) %>%
  summarise(sum = sum(EU_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Genre,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Genre,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Europe") 

# by Publishers
eu.pub <- orig %>%
  select(EU_Sales,
         publisher.rank) %>%
  group_by(publisher.rank) %>%
  summarise(sum = sum(EU_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = publisher.rank,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(publisher.rank,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Europe")

# by top 10 publishers
eu.pus <- orig %>%
  select(EU_Sales,
         Publisher) %>%
  group_by(Publisher) %>%
  summarise(sum = sum(EU_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Publisher,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Europe")

#----Japanese Sales
# Japan sales by Platforms 
jp.plat <- orig %>%
  select(JP_Sales,
         Platform) %>%
  group_by(Platform) %>%
  summarise(sum = sum(JP_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Platform, sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Japan")

# JP Sales by Released Year
jp.yr <- orig %>%
  select(JP_Sales,
         yr.released) %>%
  group_by(yr.released) %>%
  summarise(sum = sum(JP_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = yr.released,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(yr.released,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Japan")

# JP sales by Genre
jp.genres <- orig %>%
  select(JP_Sales,
         Genre) %>%
  group_by(Genre) %>%
  summarise(sum = sum(JP_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Genre,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Genre,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Japan")

# by Publishers
jp.pub <- orig %>%
  select(JP_Sales,
         publisher.rank) %>%
  group_by(publisher.rank) %>%
  summarise(sum = sum(JP_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = publisher.rank,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(publisher.rank,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Japan")

# by top 10 publishers
jp.pus <- orig %>%
  select(JP_Sales,
         Publisher) %>%
  group_by(Publisher) %>%
  summarise(sum = sum(JP_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Publisher,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Japan")

#----Other Country Sales
# Other countries sales by Platforms 
other.plat <- orig %>%
  select(Other_Sales,
         Platform) %>%
  group_by(Platform) %>%
  summarise(sum = sum(Other_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Platform, sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Other Countries") 

#---
# other Sales by Released Year
other.yr <- orig %>%
  select(Other_Sales,
         yr.released) %>%
  group_by(yr.released) %>%
  summarise(sum = sum(Other_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = yr.released,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(yr.released,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Other Countries") 

# other sales by Genre
other.genres <- orig %>%
  select(Other_Sales,
         Genre) %>%
  group_by(Genre) %>%
  summarise(sum = sum(Other_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Genre,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Genre,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Other Countries") 

# by Publishers
other.pub <- orig %>%
  select(Other_Sales,
         publisher.rank) %>%
  group_by(publisher.rank) %>%
  summarise(sum = sum(Other_Sales)) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = publisher.rank,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(publisher.rank,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Other Countries")

# by top 10 publishers
other.pus <- orig %>%
  select(Other_Sales,
         Publisher) %>%
  group_by(Publisher) %>%
  summarise(sum = sum(Other_Sales)) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  data.frame() %>%
  ggplot(aes(x = sum,
             y = Platform,
             fill = -sum)) +
  geom_bar(mapping = aes(x= sum,
                         y= reorder(Publisher,sum)),
           stat = "identity",
           width = 0.5,
           position = "dodge") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  xlab("Other Countries")

# --------------- Platforms -------------
ggarrange(na.plat,
          eu.plat,
          jp.plat,
          other.plat,
          glo.plat,
          legend.grob = NULL,
          legend = NULL) %>%
  annotate_figure(top = text_grob("Total Video Game Sales (in millions) by Platforms",
                                  color = "black",
                                  face = "bold",
                                  size = 14))

# NA: X360, PS2, Wii, DS, PS3
# Europe: PS3, PS2, Wii, X360, PS
# JP: DS, PS2, PS, SNES, NES
# Other: PS2, PS3, Wii, X360, DS
# Global: PS2, Wii, X360, PS3, DS

# platforms definitely have predictive powers. but it also called for developing
#           different algorithm to each country

# -----Released Years -----
ggarrange(na.yr,
          eu.yr,
          jp.yr,
          other.yr,
          glo.yr,
          legend = NULL) %>%
  annotate_figure(top = text_grob("Total Video Game Sales (in millions) by Released Years",
                                  color = "black",
                                  face = "bold",
                                  size = 14))

# the years are roughly similar across countries, going by: 2005, 2010, 2000, 1995
# --------------Genres ------------------
ggarrange(na.genres,
          eu.genres,
          jp.genres,
          other.genres,
          glo.genres,
          legend = NULL,
          ncol = 2,
          nrow = 3) %>%
  annotate_figure(top = text_grob("Total Video Game Sales (in millions) by Genres",
                                  color = "black",
                                  face = "bold",
                                  size = 14))

# Action, Sport, Shooter: top 3 for all countries but JP 
# JP: loves Role-playing then action & platform
# Racing is also popular 

# -------------- Publishers Rank -----------------
ggarrange(na.pub,
          eu.pub,
          jp.pub,
          other.pub,
          glo.pub,
          legend = NULL) %>%
  annotate_figure(top = text_grob("Total Video Game Sales (in millions) by Publisher Rank",
                                  color = "black",
                                  face = "bold",
                                  size = 14))
# this information is consistent across countries. This called for publishers in
#      in the top-10 tend to have more power controlling sales 

# ----------------Top 10 Publishers -------------
ggarrange(na.pus,
          eu.pus,
          jp.pus,
          other.pus,
          glo.pus,
          legend = NULL,
          nrow = 3,
          ncol = 2) %>%
  annotate_figure(top = text_grob("Total Video Game Sales (in millions) by Top-10 Publishers",
                                  color = "black",
                                  face = "bold",
                                  size = 14))
# Nintendo & Electronic Arts remain the top 2 publishers. Different countries have 
#          different preferences in regard to their favorite publishers
#          For example, JP likes Konami and Sony, but other country dont care for 
#          Konami much 

#================== Train vs.test set=========================
# train index
train.index <- createDataPartition(orig$Global_Sales,
                                   times = 1,
                                   p = .7,
                                   list = FALSE)
# train set
train.set <- orig %>%
  slice(train.index) %>%
  select(!c(Rank,
            Name,
            Year,
            Publisher)) %>%
  mutate(Platform = factor(Platform),
         Genre = factor(Genre),
         yr.released = factor(yr.released),
         publisher.rank = factor(publisher.rank))

# test set
test.set <- orig %>%
  slice(-train.index) %>%
  select(!c(Rank,
            Name,
            Year,
            Publisher)) %>%
  mutate(Platform = factor(Platform),
         Genre = factor(Genre),
         yr.released = factor(yr.released),
         publisher.rank = factor(publisher.rank))

#--- check levels 
levels(factor(train.set$Platform))
levels(factor(test.set$Platform))         # have different levels

levels(factor(train.set$Genre))
levels(factor(test.set$Genre))            # similar levels

levels(factor(train.set$yr.released))
levels(factor(test.set$yr.released))      # similar levels

levels(factor(train.set$publisher.rank))
levels(factor(test.set$publisher.rank))   # similar levels

#------------- NA Algorithms ------------------ 
# GLM model 
NA.fit_glm <- train(data = train.set,
                    NA_Sales~Platform + Genre + yr.released + publisher.rank,
                    method = "glm")

NA.y_hat_glm <- predict(NA.fit_glm,test.set)

# Random Forest Model 
NA.train_forest <- train(data = train.set,
                         NA_Sales~Platform + Genre + 
                      yr.released + publisher.rank,
                    method = "rf",
                    tuneGrid = data.frame(mtry = seq(1:7)),
                    ntree = 50)

NA.fit_forest <- randomForest(NA_Sales~Platform + Genre + 
                                yr.released + publisher.rank,
                       data = train.set,
                       minNode = NA.train_forest$bestTune$mtry)

NA.y_hat_forest <- predict(NA.fit_forest,test.set)

data.frame(GLM = mean((NA.y_hat_glm - test.set$NA_Sales)^2),
           Random.Forest = mean((NA.y_hat_forest - test.set$NA_Sales)^2))
# we will choose Random.forest method 

#------------- Europe Algorithms ------------------ 
# GLM model 
EU.fit_glm <- train(data = train.set,
                    EU_Sales~Platform + Genre + yr.released + publisher.rank,
                    method = "glm")

EU.y_hat_glm <- predict(EU.fit_glm,test.set)

# Random Forest Model 
EU.train_forest <- train(data = train.set,
                         EU_Sales~Platform + Genre + 
                           yr.released + publisher.rank,
                         method = "rf",
                         tuneGrid = data.frame(mtry = seq(1:7)),
                         ntree = 50)

EU.fit_forest <- randomForest(EU_Sales~Platform + Genre + 
                                yr.released + publisher.rank,
                              data = train.set,
                              minNode = EU.train_forest$bestTune$mtry)

EU.y_hat_forest <- predict(EU.fit_forest,test.set)

# RMSE
data.frame(GLM = mean((EU.y_hat_glm - test.set$EU_Sales)^2),
           Random.Forest = mean((EU.y_hat_forest - test.set$EU_Sales)^2))
# we will choose Random.forest method 

#------------- Japan Algorithms ------------------ 
# GLM model 
JP.fit_glm <- train(data = train.set,
                    JP_Sales~Platform + Genre + yr.released + publisher.rank,
                    method = "glm")

JP.y_hat_glm <- predict(JP.fit_glm,test.set)

# Random Forest Model 
JP.train_forest <- train(data = train.set,
                         JP_Sales~Platform + Genre + 
                           yr.released + publisher.rank,
                         method = "rf",
                         tuneGrid = data.frame(mtry = seq(1:7)),
                         ntree = 50)

JP.fit_forest <- randomForest(JP_Sales~Platform + Genre + 
                                yr.released + publisher.rank,
                              data = train.set,
                              minNode = JP.train_forest$bestTune$mtry)

JP.y_hat_forest <- predict(JP.fit_forest,test.set)

# RMSE
data.frame(GLM = mean((JP.y_hat_glm - test.set$JP_Sales)^2),
           Random.Forest = mean((JP.y_hat_forest - test.set$JP_Sales)^2))
# we will choose Random.forest method 

#------------- Other Countries Algorithms ------------------ 
# GLM model 
Other.fit_glm <- train(data = train.set,
                    Other_Sales~Platform + Genre + yr.released + publisher.rank,
                    method = "glm")

Other.y_hat_glm <- predict(Other.fit_glm,test.set)

# Random Forest Model 
Other.train_forest <- train(data = train.set,
                         Other_Sales ~ Platform + Genre + 
                           yr.released + publisher.rank,
                         method = "rf",
                         tuneGrid = data.frame(mtry = seq(1:7)),
                         ntree = 50)

Other.fit_forest <- randomForest(Other_Sales~Platform + Genre + 
                                yr.released + publisher.rank,
                                data = train.set,
                                minNode = Other.train_forest$bestTune$mtry)

Other.y_hat_forest <- predict(Other.fit_forest,
                              test.set)

# RMSE
data.frame(GLM = mean((Other.y_hat_glm - test.set$Other_Sales)^2),
           Random.Forest = mean((Other.y_hat_forest - test.set$Other_Sales)^2))
# we will choose Random.forest method 