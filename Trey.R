library(tidyverse)
library(knitr)
library(ggpubr)
options(stringsAsFactors = F)
df = read.csv("Airbnb_NYC_2019.csv")[,-1] %>% as_tibble()
df %>% group_by(room_type) %>% summarize(count = n())

## Price Analysis: Room Type ##

RT_df = NULL
for (i in 1:1000){
  c = unique(df$room_type)
  ti = NULL
  for (j in 1:length(c)){
    df_ = df %>% filter(room_type == c[j])
    n = df_[sample(x = 1:nrow(df_), size = 50, replace = T),]
    ti = rbind.data.frame(ti, data.frame(room_type = c[j], mean_price = mean(n$price)))
  }
  RT_df = rbind.data.frame(RT_df, ti)
}
RT_df = RT_df %>% filter(mean_price < 350)
RT_df %>% ggplot(aes(x = mean_price, fill = room_type)) + theme_bw() + geom_histogram(position = "identity", binwidth = 5, alpha = I(3/4)) + theme_bw() + scale_fill_manual("Room Type", values = c("#d6cbd3", "#eca1a6", "#bdcebe")) + scale_x_continuous("Mean Price") + scale_y_continuous("Count (per 1,000 resamples)")
rt_tab = RT_df %>% group_by(room_type) %>% summarise(.groups = "drop", mean_x_bar = mean(mean_price), stdev_x_bar = sd(mean_price))
kable(rt_tab, caption = "Simulation of 1,000 samples with size 50 (each group)")

## Price Analysis: Neighbourhood Group ##

NG_df = NULL
for (i in 1:1000){
  c = unique(df$neighbourhood_group)
  ti = NULL
  for (j in 1:length(c)){
    df_ = df %>% filter(neighbourhood_group == c[j])
    n = df_[sample(x = 1:nrow(df_), size = 50, replace = T),]
    ti = rbind.data.frame(ti, data.frame(neighbourhood_group = c[j], mean_price = mean(n$price)))
  }
  NG_df = rbind.data.frame(NG_df, ti)
}
NG_df = NG_df %>% filter(mean_price < 350)
NG_df %>% ggplot(aes(x = mean_price, fill = neighbourhood_group)) + theme_bw() + geom_histogram(alpha = I(2/3), position = "identity", binwidth = 5) + theme_bw() + scale_x_continuous("Mean Price") + scale_y_continuous("Count (per 1,000 resamples)") + labs(fill = "Neighbourhood Group")
ng_tab = NG_df %>% group_by(neighbourhood_group) %>% summarise(.groups = "drop", mean_x_bar = mean(mean_price), stdev_x_bar = sd(mean_price))
kable(ng_tab, caption = "Simulation of 1,000 samples with size 50 (each group)")

## Price Analysis: Number of Reviews

df = df %>% mutate(num_rev = ifelse(number_of_reviews == 0, "0", ifelse(number_of_reviews == 1, "1", ifelse(number_of_reviews < 6, "2-5", ifelse(number_of_reviews < 25, "6-24", "25+")))))
df$num_rev = factor(df$num_rev, levels = c("0", "1", "2-5", "6-24", "25+"))
pl1 = df %>% group_by(num_rev) %>% summarize(.groups = "drop", mean_price = mean(price), count = n()) %>% ggplot(aes(x = num_rev, y = mean_price, fill = num_rev)) + theme_bw() + geom_bar(color = "black", width = I(1/2), stat = "identity") + scale_x_discrete("Number of Reviews") + theme(legend.position = "none") + scale_fill_manual(values = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")) + scale_y_continuous("Mean Price", limits = c(0,200))

df = df %>% mutate(num_revp = ifelse(reviews_per_month <= .19, ".01-.19", ifelse(reviews_per_month <= .72, ".20-.72", ifelse(reviews_per_month <= 2.02, ".73-2.02", "2.03+"))))
pl2 = df %>% group_by(num_revp) %>% summarize(.groups = "drop", mean_price = mean(price), count = n()) %>% drop_na() %>% ggplot(aes(x = num_revp, y = mean_price, fill = num_revp)) + geom_bar(color = "black", width = I(1/2), stat = "identity") + scale_x_discrete("Number of Reviews per Month") + theme_bw() + scale_fill_manual(values = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C")) + theme(legend.position = "none") + scale_y_continuous("Mean Price", limits = c(0,200))

ggarrange(pl1, pl2, nrow = 1, ncol = 2)

'distance_to_closest = NULL
for (i in 1:nrow(df)){
  la = df$latitude[i];lo = df$longitude[i]
  dist = df[-i,] %>% mutate(dis = sqrt(((latitude-la)^2)+((longitude-lo)^2))) %>% arrange(dis) %>% head(1) %>% select(dis, id_closest = id)
  distance_to_closest = rbind.data.frame(distance_to_closest, dist)
  print(round(100*(i/nrow(df)),3))
}
data.frame(df$id, distance_to_closest) %>% write.csv("distance.csv")
distance = read.csv("distance.csv")[,-1]
df_dis = df %>% left_join(distance, by = "id")
prox_df = df_dis %>% group_by(id_closest) %>% summarize(.groups = "drop", count_closest = n()) %>% arrange(desc(count_closest)) %>% transmute(id = id_closest, count_closest) # gives each ID the number of properties for which it is the closest property.
df_dis = df_dis %>% left_join(prox_df, by = "id") %>% select(id, count_closest, dis_closest, id_closest, everything())
df_dis$count_closest[which(is.na(df_dis$count_closest))] = 0
'

## Proximity Analysis
df = read.csv("Airbnb_NYC_2019.csv")[,-1]
table(df$count_closest)

dis_tb = df %>% group_by(count_closest = as.factor(count_closest)) %>% summarize(.groups = "drop", n = n(),mean_dis_closest = mean(dis_closest),median_dis_closest = median(dis_closest),sd_dis_closest = sd(dis_closest))
kable(dis_tb)
df %>% ggplot(aes(x = as.factor(count_closest), y = sqrt(dis_closest), fill = as.factor(count_closest))) + geom_boxplot(alpha = I(2/3)) + theme_bw() +
  theme(legend.position = "none") + scale_x_discrete("Closest to (Count)") + scale_y_continuous("Distance to Closest Property (Square Root)", breaks = seq(0,.15,.025)) + scale_fill_manual(values = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")) +
  ggtitle("Proximity Analysis", subtitle = "Number of Nearby Properties versus Distance to Closest Property")




