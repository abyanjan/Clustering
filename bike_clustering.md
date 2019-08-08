Bike Clustering
================
Ajay Byanjankar
2019-05-02

``` r
library(tidyverse)
library(readxl)
library(knitr)
```

### Read Data

``` r
customers <- read_excel('Bike data/bikeshops.xlsx')
products  <- read_excel('Bike data/bikes.xlsx')
orders    <- read_excel('Bike data/orders.xlsx') %>% select(-1)
```

### Combining the data frames into single data frame

``` r
bike_df <- orders %>% left_join(customers, by= c('customer.id' = 'bikeshop.id')) %>% 
  left_join(products, by = c('product.id' = 'bike.id'))

# selecting only required features

bike_df <- bike_df %>%  select(order.date, order.id, order.line, bikeshop.name, model,
         quantity, price, category1, category2, frame) %>% 
  arrange(order.id,order.line)


knitr::kable(bike_df %>% head())
```

| order.date | order.id | order.line | bikeshop.name             | model                    | quantity | price | category1 | category2     | frame    |
| :--------- | -------: | ---------: | :------------------------ | :----------------------- | -------: | ----: | :-------- | :------------ | :------- |
| 2011-01-07 |        1 |          1 | Ithaca Mountain Climbers  | Jekyll Carbon 2          |        1 |  6070 | Mountain  | Over Mountain | Carbon   |
| 2011-01-07 |        1 |          2 | Ithaca Mountain Climbers  | Trigger Carbon 2         |        1 |  5970 | Mountain  | Over Mountain | Carbon   |
| 2011-01-10 |        2 |          1 | Kansas City 29ers         | Beast of the East 1      |        1 |  2770 | Mountain  | Trail         | Aluminum |
| 2011-01-10 |        2 |          2 | Kansas City 29ers         | Trigger Carbon 2         |        1 |  5970 | Mountain  | Over Mountain | Carbon   |
| 2011-01-10 |        3 |          1 | Louisville Race Equipment | Supersix Evo Hi-Mod Team |        1 | 10660 | Road      | Elite Road    | Carbon   |
| 2011-01-10 |        3 |          2 | Louisville Race Equipment | Jekyll Carbon 4          |        1 |  3200 | Mountain  | Over Mountain | Carbon   |

``` r
# obtaining the quantity of bike purchased by bike shops for each model and model features

order_trend <- bike_df %>% 
  group_by(bikeshop.name, model,category1, category2, frame, price) %>% 
  summarise(total_qty = sum(quantity))
```

``` r
# converting total quanity to the proportion of bike purchased by a bike shop

order_trend <- order_trend %>% 
  group_by(bikeshop.name) %>% 
  mutate(total_qty = total_qty/sum(total_qty)) %>% 
  ungroup() %>% 
  spread(bikeshop.name,total_qty, fill = 0)

# converting price into category - high and low price

order_trend$price <- Hmisc::cut2(order_trend$price,g=2)

order_trend %>% 
  head() %>% 
  knitr::kable()
```

| model               | category1 | category2  | frame    | price         | Albuquerque Cycles | Ann Arbor Speed | Austin Cruisers | Cincinnati Speed | Columbus Race Equipment | Dallas Cycles | Denver Bike Shop | Detroit Cycles | Indianapolis Velocipedes | Ithaca Mountain Climbers | Kansas City 29ers | Las Vegas Cycles | Los Angeles Cycles | Louisville Race Equipment | Miami Race Equipment | Minneapolis Bike Shop | Nashville Cruisers | New Orleans Velocipedes | New York Cycles | Oklahoma City Race Equipment | Philadelphia Bike Shop | Phoenix Bi-peds | Pittsburgh Mountain Machines | Portland Bi-peds | Providence Bi-peds | San Antonio Bike Shop | San Francisco Cruisers | Seattle Race Equipment | Tampa 29ers | Wichita Speed |
| :------------------ | :-------- | :--------- | :------- | :------------ | -----------------: | --------------: | --------------: | ---------------: | ----------------------: | ------------: | ---------------: | -------------: | -----------------------: | -----------------------: | ----------------: | ---------------: | -----------------: | ------------------------: | -------------------: | --------------------: | -----------------: | ----------------------: | --------------: | ---------------------------: | ---------------------: | --------------: | ---------------------------: | ---------------: | -----------------: | --------------------: | ---------------------: | ---------------------: | ----------: | ------------: |
| Bad Habit 1         | Mountain  | Trail      | Aluminum | \[ 415, 3500) |          0.0174825 |       0.0066445 |       0.0081301 |        0.0051151 |               0.0101523 |     0.0128205 |        0.0117340 |      0.0099206 |                0.0062696 |                0.0181962 |         0.0181504 |        0.0016026 |          0.0062893 |                 0.0075949 |            0.0042135 |             0.0182648 |          0.0086705 |               0.0184783 |       0.0074074 |                    0.0129870 |              0.0244898 |       0.0112755 |                    0.0159151 |        0.0108696 |          0.0092251 |             0.0215054 |              0.0026738 |              0.0156250 |   0.0194175 |     0.0059172 |
| Bad Habit 2         | Mountain  | Trail      | Aluminum | \[ 415, 3500) |          0.0069930 |       0.0099668 |       0.0040650 |        0.0000000 |               0.0000000 |     0.0170940 |        0.0139070 |      0.0158730 |                0.0031348 |                0.0110759 |         0.0158456 |        0.0000000 |          0.0094340 |                 0.0000000 |            0.0112360 |             0.0167428 |          0.0173410 |               0.0021739 |       0.0074074 |                    0.0095238 |              0.0040816 |       0.0190275 |                    0.0026525 |        0.0108696 |          0.0239852 |             0.0000000 |              0.0026738 |              0.0078125 |   0.0000000 |     0.0000000 |
| Beast of the East 1 | Mountain  | Trail      | Aluminum | \[ 415, 3500) |          0.0104895 |       0.0149502 |       0.0081301 |        0.0000000 |               0.0000000 |     0.0042735 |        0.0182529 |      0.0119048 |                0.0094044 |                0.0213608 |         0.0181504 |        0.0016026 |          0.0251572 |                 0.0000000 |            0.0140449 |             0.0167428 |          0.0086705 |               0.0086957 |       0.0172840 |                    0.0242424 |              0.0000000 |       0.0126850 |                    0.0053050 |        0.0108696 |          0.0092251 |             0.0053763 |              0.0000000 |              0.0156250 |   0.0097087 |     0.0000000 |
| Beast of the East 2 | Mountain  | Trail      | Aluminum | \[ 415, 3500) |          0.0104895 |       0.0099668 |       0.0081301 |        0.0000000 |               0.0050761 |     0.0042735 |        0.0152108 |      0.0059524 |                0.0094044 |                0.0181962 |         0.0138289 |        0.0000000 |          0.0220126 |                 0.0050633 |            0.0084270 |             0.0076104 |          0.0086705 |               0.0097826 |       0.0172840 |                    0.0086580 |              0.0000000 |       0.0232558 |                    0.0106101 |        0.0155280 |          0.0147601 |             0.0107527 |              0.0026738 |              0.0234375 |   0.0291262 |     0.0019724 |
| Beast of the East 3 | Mountain  | Trail      | Aluminum | \[ 415, 3500) |          0.0034965 |       0.0033223 |       0.0000000 |        0.0000000 |               0.0025381 |     0.0042735 |        0.0169492 |      0.0119048 |                0.0000000 |                0.0102848 |         0.0181504 |        0.0032051 |          0.0000000 |                 0.0050633 |            0.0042135 |             0.0152207 |          0.0202312 |               0.0043478 |       0.0049383 |                    0.0051948 |              0.0204082 |       0.0162086 |                    0.0026525 |        0.0201863 |          0.0073801 |             0.0322581 |              0.0000000 |              0.0078125 |   0.0097087 |     0.0000000 |
| CAAD Disc Ultegra   | Road      | Elite Road | Aluminum | \[ 415, 3500) |          0.0139860 |       0.0265781 |       0.0203252 |        0.0153453 |               0.0101523 |     0.0000000 |        0.0108648 |      0.0079365 |                0.0094044 |                0.0000000 |         0.0106598 |        0.0112179 |          0.0157233 |                 0.0278481 |            0.0210674 |             0.0182648 |          0.0375723 |               0.0152174 |       0.0172840 |                    0.0103896 |              0.0163265 |       0.0126850 |                    0.0026525 |        0.0139752 |          0.0073801 |             0.0053763 |              0.0026738 |              0.0078125 |   0.0000000 |     0.0098619 |

### K- Means Clustering

``` r
# Removing model features and arranging data into customers in rows and products in columns

km_df <- order_trend %>% 
  select(-c(1:5)) %>% 
  t()
```

``` r
# selecting optimal number of clusters between 4 and 10

# with minimum within sums of distance


wss <- vector()

for (i in 4:10){
  
  set.seed(11)
  
  km <- kmeans(km_df, centers = i, nstart = 50)
  
  wss = append(wss,km$tot.withinss)
}
```

``` r
# plot of total within sum of distance for number of clusters

ggplot(data.frame(x = 4:10, y = wss), aes(x = x, y = y))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = 4:10)+
  labs(x = "num of clusters",
       y = 'total sum of squares')
```

![](bike_clustering_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# with shilloute distance

library(cluster)

sil <- vector()

for (i in 4:10){
  
  set.seed(123)
  
  km <- kmeans(km_df, centers = i, nstart = 50)
  
  width <-  summary(silhouette(km[[1]], dist(km_df)))[4]
  
  sil <- append(sil, width)
}
```

``` r
# plot silhouette width

ggplot(data.frame(x = 4:10, y = unlist(sil)), aes(x = x, y = y))+
         geom_point()+
         geom_line()+
  labs(x = "num of clusters",
       y = 'total sum of squares')
```

![](bike_clustering_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
# performing kmeans clustering with 2 clusters

set.seed(11)

km <- kmeans(km_df, centers = 5, nstart = 50)

# create a cluster list with list of customers in each cluster

cluster_list <- list()

for (i in 1:5){
  
  cluster_list[[i]] <- names(km$cluster[km$cluster == i])
  
}

names(cluster_list) <- paste0("x",1:5)
```

### Inspecting each clusters

``` r
# Combine cluster centroids with bike models for feature inspection

custsegemts_centers <- t(km$centers)
colnames(custsegemts_centers) <- make.names(colnames(custsegemts_centers))

# add the model features

cluster_trends  <- bind_cols(order_trend[,1:5], data.frame(custsegemts_centers))
```

### cluster 1

``` r
cluster_trends %>% 
  select(c(1:5),X1) %>% 
  arrange(desc(X1)) %>% 
  kable()
```

| model                           | category1 | category2          | frame    | price          |        X1 |
| :------------------------------ | :-------- | :----------------- | :------- | :------------- | --------: |
| Scalpel-Si Carbon 3             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0342692 |
| Jekyll Carbon 4                 | Mountain  | Over Mountain      | Carbon   | \[ 415, 3500)  | 0.0302818 |
| Scalpel 29 Carbon Race          | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0280391 |
| Trigger Carbon 3                | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0259353 |
| Habit Carbon 2                  | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0233750 |
| Trigger Carbon 4                | Mountain  | Over Mountain      | Carbon   | \[ 415, 3500)  | 0.0232606 |
| Catalyst 4                      | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0215639 |
| Jekyll Carbon 2                 | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0210792 |
| Supersix Evo Hi-Mod Dura Ace 2  | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0210578 |
| Trigger Carbon 2                | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0210433 |
| F-Si Black Inc.                 | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0204015 |
| SuperX Hi-Mod CX1               | Road      | Cyclocross         | Carbon   | \[3500,12790\] | 0.0203452 |
| Habit Carbon SE                 | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0201959 |
| CAAD12 Disc Dura Ace            | Road      | Elite Road         | Aluminum | \[3500,12790\] | 0.0195695 |
| Scalpel-Si Black Inc.           | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0195387 |
| Beast of the East 2             | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0193108 |
| Fat CAAD1                       | Mountain  | Fat Bike           | Aluminum | \[3500,12790\] | 0.0192904 |
| Jekyll Carbon 1                 | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0185052 |
| Trail 5                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0184779 |
| Scalpel-Si Hi-Mod 1             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0180853 |
| Bad Habit 1                     | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0178429 |
| Scalpel-Si Carbon 4             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0177866 |
| F-Si 1                          | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0174094 |
| Habit Carbon 1                  | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0167522 |
| Slice Hi-Mod Black Inc.         | Road      | Triathalon         | Carbon   | \[3500,12790\] | 0.0164058 |
| Trail 4                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0159978 |
| F-Si Carbon 2                   | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0156043 |
| Scalpel 29 Carbon 2             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0145853 |
| Scalpel-Si 5                    | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0144923 |
| F-Si Hi-Mod 1                   | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0139290 |
| Scalpel 29 Carbon 3             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0135151 |
| Synapse Hi-Mod Dura Ace         | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0134742 |
| Jekyll Carbon 3                 | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0132881 |
| Trail 2                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0132727 |
| Scalpel 29 4                    | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0131379 |
| Trigger Carbon 1                | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0126881 |
| Synapse Carbon Disc Ultegra D12 | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0126105 |
| Scalpel-Si Carbon 2             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0122546 |
| Beast of the East 1             | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0121248 |
| Habit Hi-Mod Black Inc.         | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0115198 |
| CAAD12 Red                      | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0112561 |
| Habit 4                         | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0112510 |
| Slice Ultegra D12               | Road      | Triathalon         | Carbon   | \[ 415, 3500)  | 0.0111844 |
| Synapse Carbon Ultegra 3        | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0108780 |
| F-Si Hi-Mod Team                | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0107132 |
| Scalpel-Si Race                 | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0096379 |
| Synapse Hi-Mod Disc Black Inc.  | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0091677 |
| Habit Carbon 3                  | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0089603 |
| Supersix Evo Hi-Mod Utegra      | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0087692 |
| Supersix Evo Hi-Mod Team        | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0087538 |
| Supersix Evo Ultegra 3          | Road      | Elite Road         | Carbon   | \[ 415, 3500)  | 0.0087333 |
| F-Si Carbon 4                   | Mountain  | Cross Country Race | Carbon   | \[ 415, 3500)  | 0.0087324 |
| F-Si 3                          | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0084696 |
| Supersix Evo Black Inc.         | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0082263 |
| Synapse Carbon Disc Ultegra     | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0079626 |
| F-Si 2                          | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0078124 |
| Trail 3                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0078124 |
| Slice Hi-Mod Dura Ace D12       | Road      | Triathalon         | Carbon   | \[3500,12790\] | 0.0076417 |
| Fat CAAD2                       | Mountain  | Fat Bike           | Aluminum | \[ 415, 3500)  | 0.0076213 |
| Beast of the East 3             | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0075487 |
| Synapse Hi-Mod Disc Red         | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0074352 |
| Catalyst 2                      | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0073208 |
| Synapse Hi-Mod Disc Ultegra     | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0070008 |
| Catalyst 1                      | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0067943 |
| Supersix Evo Hi-Mod Dura Ace 1  | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0056669 |
| Trail 1                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0055534 |
| Supersix Evo Red                | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0054603 |
| Habit 5                         | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0052897 |
| Bad Habit 2                     | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0045762 |
| Habit 6                         | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0042348 |
| CAAD12 Black Inc                | Road      | Elite Road         | Aluminum | \[3500,12790\] | 0.0041418 |
| Catalyst 3                      | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0032004 |
| Synapse Disc Adventure          | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0025595 |
| Synapse Carbon 105              | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0022958 |
| CAAD8 Claris                    | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0021455 |
| CAAD8 105                       | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0014116 |
| Supersix Evo Ultegra 4          | Road      | Elite Road         | Carbon   | \[ 415, 3500)  | 0.0014116 |
| CAAD12 Ultegra                  | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0011479 |
| SuperX Ultegra                  | Road      | Cyclocross         | Carbon   | \[ 415, 3500)  | 0.0011479 |
| Synapse Carbon Disc 105         | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0011479 |
| Synapse Claris                  | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0011479 |
| CAAD8 Tiagra                    | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0010549 |
| CAAD Disc Ultegra               | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0008842 |
| Supersix Evo 105                | Road      | Elite Road         | Carbon   | \[ 415, 3500)  | 0.0008842 |
| Synapse Disc 105                | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0007911 |
| CAAD12 105                      | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0005274 |
| Synapse Sora                    | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0005274 |
| CAAD12 Disc 105                 | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0002637 |
| Supersix Evo Tiagra             | Road      | Elite Road         | Carbon   | \[ 415, 3500)  | 0.0002637 |
| Synapse Disc Tiagra             | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0002637 |
| CAAD8 Sora                      | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0000000 |
| Slice 105                       | Road      | Triathalon         | Carbon   | \[ 415, 3500)  | 0.0000000 |
| Slice Ultegra                   | Road      | Triathalon         | Carbon   | \[ 415, 3500)  | 0.0000000 |
| SuperX 105                      | Road      | Cyclocross         | Carbon   | \[ 415, 3500)  | 0.0000000 |
| SuperX Rival CX1                | Road      | Cyclocross         | Carbon   | \[ 415, 3500)  | 0.0000000 |
| Syapse Carbon Tiagra            | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0000000 |
| Synapse Carbon Ultegra 4        | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0000000 |

### cluster 2

``` r
cluster_trends %>% 
  select(c(1:5),X2) %>% 
  arrange(desc(X2)) %>% 
 knitr::kable()
```

| model                           | category1 | category2          | frame    | price          |        X2 |
| :------------------------------ | :-------- | :----------------- | :------- | :------------- | --------: |
| Slice Ultegra                   | Road      | Triathalon         | Carbon   | \[ 415, 3500)  | 0.0554531 |
| Trigger Carbon 4                | Mountain  | Over Mountain      | Carbon   | \[ 415, 3500)  | 0.0304147 |
| CAAD12 105                      | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0270792 |
| Beast of the East 3             | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0263331 |
| Trail 1                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0249397 |
| Bad Habit 1                     | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0229976 |
| F-Si Carbon 4                   | Mountain  | Cross Country Race | Carbon   | \[ 415, 3500)  | 0.0224490 |
| CAAD12 Disc 105                 | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0209568 |
| Synapse Disc 105                | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0209568 |
| Trail 2                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0203094 |
| Synapse Carbon Disc 105         | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0196621 |
| CAAD8 Claris                    | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0195633 |
| Supersix Evo Tiagra             | Road      | Elite Road         | Carbon   | \[ 415, 3500)  | 0.0175225 |
| CAAD8 Tiagra                    | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0169739 |
| Supersix Evo Ultegra 4          | Road      | Elite Road         | Carbon   | \[ 415, 3500)  | 0.0168751 |
| Slice Ultegra D12               | Road      | Triathalon         | Carbon   | \[ 415, 3500)  | 0.0162278 |
| Jekyll Carbon 2                 | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0149331 |
| Fat CAAD2                       | Mountain  | Fat Bike           | Aluminum | \[ 415, 3500)  | 0.0148343 |
| Habit 6                         | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0148343 |
| Scalpel 29 4                    | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0148343 |
| Catalyst 1                      | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0141870 |
| Catalyst 2                      | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0141870 |
| Habit 4                         | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0135396 |
| Scalpel-Si Black Inc.           | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0135396 |
| Supersix Evo 105                | Road      | Elite Road         | Carbon   | \[ 415, 3500)  | 0.0135396 |
| CAAD8 Sora                      | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0127935 |
| F-Si 2                          | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0127935 |
| Supersix Evo Hi-Mod Dura Ace 1  | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0127935 |
| Synapse Carbon 105              | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0127935 |
| Synapse Carbon Disc Ultegra     | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0127935 |
| Scalpel 29 Carbon 3             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0121461 |
| F-Si 3                          | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0114988 |
| SuperX Rival CX1                | Road      | Cyclocross         | Carbon   | \[ 415, 3500)  | 0.0114988 |
| Synapse Carbon Ultegra 3        | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0114988 |
| Synapse Carbon Ultegra 4        | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0114988 |
| Synapse Disc Adventure          | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0114988 |
| Synapse Disc Tiagra             | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0114988 |
| Synapse Sora                    | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0114988 |
| Trail 5                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0114988 |
| CAAD Disc Ultegra               | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0108514 |
| Scalpel 29 Carbon 2             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0108514 |
| Slice 105                       | Road      | Triathalon         | Carbon   | \[ 415, 3500)  | 0.0108514 |
| Supersix Evo Ultegra 3          | Road      | Elite Road         | Carbon   | \[ 415, 3500)  | 0.0108514 |
| Syapse Carbon Tiagra            | Road      | Endurance Road     | Carbon   | \[ 415, 3500)  | 0.0108514 |
| CAAD12 Ultegra                  | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0102041 |
| Habit Carbon 3                  | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0102041 |
| Catalyst 3                      | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0101053 |
| Catalyst 4                      | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0094580 |
| F-Si 1                          | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0094580 |
| Supersix Evo Hi-Mod Utegra      | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0094580 |
| Synapse Claris                  | Road      | Endurance Road     | Aluminum | \[ 415, 3500)  | 0.0094580 |
| Jekyll Carbon 1                 | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0088106 |
| SuperX Ultegra                  | Road      | Cyclocross         | Carbon   | \[ 415, 3500)  | 0.0088106 |
| Trail 4                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0081633 |
| CAAD12 Red                      | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0074172 |
| F-Si Carbon 2                   | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0074172 |
| Habit Carbon 2                  | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0074172 |
| Habit Carbon SE                 | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0074172 |
| Scalpel 29 Carbon Race          | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0074172 |
| Synapse Hi-Mod Disc Red         | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0074172 |
| Scalpel-Si Hi-Mod 1             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0067698 |
| Supersix Evo Black Inc.         | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0067698 |
| SuperX Hi-Mod CX1               | Road      | Cyclocross         | Carbon   | \[3500,12790\] | 0.0067698 |
| Beast of the East 2             | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0053763 |
| Habit Carbon 1                  | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0053763 |
| Jekyll Carbon 3                 | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0053763 |
| Synapse Hi-Mod Disc Black Inc.  | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0053763 |
| CAAD8 105                       | Road      | Elite Road         | Aluminum | \[ 415, 3500)  | 0.0047290 |
| Scalpel-Si 5                    | Mountain  | Cross Country Race | Aluminum | \[ 415, 3500)  | 0.0047290 |
| Scalpel-Si Carbon 2             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0047290 |
| Scalpel-Si Carbon 4             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0047290 |
| Trail 3                         | Mountain  | Sport              | Aluminum | \[ 415, 3500)  | 0.0047290 |
| CAAD12 Black Inc                | Road      | Elite Road         | Aluminum | \[3500,12790\] | 0.0040816 |
| Habit 5                         | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0040816 |
| Habit Hi-Mod Black Inc.         | Mountain  | Trail              | Carbon   | \[3500,12790\] | 0.0040816 |
| Jekyll Carbon 4                 | Mountain  | Over Mountain      | Carbon   | \[ 415, 3500)  | 0.0040816 |
| Scalpel-Si Carbon 3             | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0040816 |
| Scalpel-Si Race                 | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0040816 |
| Slice Hi-Mod Dura Ace D12       | Road      | Triathalon         | Carbon   | \[3500,12790\] | 0.0040816 |
| Synapse Hi-Mod Disc Ultegra     | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0040816 |
| Beast of the East 1             | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0026882 |
| CAAD12 Disc Dura Ace            | Road      | Elite Road         | Aluminum | \[3500,12790\] | 0.0026882 |
| F-Si Hi-Mod 1                   | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0026882 |
| F-Si Hi-Mod Team                | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0026882 |
| Slice Hi-Mod Black Inc.         | Road      | Triathalon         | Carbon   | \[3500,12790\] | 0.0026882 |
| Supersix Evo Hi-Mod Team        | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0026882 |
| Trigger Carbon 1                | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0026882 |
| Trigger Carbon 3                | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0026882 |
| Bad Habit 2                     | Mountain  | Trail              | Aluminum | \[ 415, 3500)  | 0.0020408 |
| F-Si Black Inc.                 | Mountain  | Cross Country Race | Carbon   | \[3500,12790\] | 0.0020408 |
| Fat CAAD1                       | Mountain  | Fat Bike           | Aluminum | \[3500,12790\] | 0.0020408 |
| Supersix Evo Hi-Mod Dura Ace 2  | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0020408 |
| Supersix Evo Red                | Road      | Elite Road         | Carbon   | \[3500,12790\] | 0.0020408 |
| SuperX 105                      | Road      | Cyclocross         | Carbon   | \[ 415, 3500)  | 0.0020408 |
| Synapse Carbon Disc Ultegra D12 | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0000000 |
| Synapse Hi-Mod Dura Ace         | Road      | Endurance Road     | Carbon   | \[3500,12790\] | 0.0000000 |
| Trigger Carbon 2                | Mountain  | Over Mountain      | Carbon   | \[3500,12790\] | 0.0000000 |

### Visualizing Kmeans clustering results with PCA

``` r
# creating cluster with 4 and 5 cluster centers

set.seed(11)

km_4 <- kmeans(km_df, centers = 4, nstart = 50)

set.seed(11)

km_5 <- kmeans(km_df, centers = 5, nstart = 50)
```

``` r
# applying PCA
#library(plotly)

pca <- prcomp(km_df, center = T, scale. = T)

 summary(pca)$importance %>% t() %>% 
  data.frame() %>% 
  rownames_to_column('summary') %>% 
  slice(1:9) %>% rename(pca = summary) %>% 
  ggplot(aes(x  = reorder(pca,-Proportion.of.Variance), y = Proportion.of.Variance))+
           geom_col(aes(fill = as.factor(Proportion.of.Variance)), show.legend = FALSE)+
  scale_fill_brewer(palette = 'Blues')+
  labs(x = "") 
```

![](bike_clustering_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# ggplotly(p1) %>% 
#   hide_legend()
```

``` r
library(ggfortify)
```

    ## Warning: package 'ggfortify' was built under R version 3.5.3

``` r
pca_fortify <- fortify(pca)

# add kmeans groups 

pca4_df <- cbind(pca_fortify, groups = km_4$cluster)

pca5_df <- cbind(pca_fortify, groups = km_5$cluster)
```

``` r
# plot the 4 clusters using PCA1 and PCA 2

 ggplot(pca4_df, aes(x = PC1, y = PC2, color = factor(groups)))+
  geom_point(aes(text = rownames(pca4_df)))+
  labs(color = "Clusters")
```

    ## Warning: Ignoring unknown aesthetics: text

![](bike_clustering_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
# ggplotly(p4, tooltip = c("text", "x", "y"))  %>%
#   layout(legend = list(x=.9, y=.99))
```

``` r
# plot the 5 clusters using PCA1 and PCA 2

 ggplot(pca5_df, aes(x = PC1, y = PC2, color = as.factor(groups)))+
  geom_point(aes(text = rownames(pca5_df)))+
  labs(color = "Clusters")
```

    ## Warning: Ignoring unknown aesthetics: text

![](bike_clustering_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
# ggplotly(p5, tooltip = c("text", "x", "y"))  %>%
#   layout(legend = list(x=.9, y=.99))
```

It seems that cluster 2 is being misclassied , so lets classify cluster
2 as 4 and 2 of the points far from cluster 4 as cluster 2.

``` r
pca_final <- pca5_df
pca_final[rownames(pca_final) %in% 
                c("San Antonio Bike Shop", "Philadelphia Bike Shop"), 128] <- 4
pca_final[rownames(pca_final) %in% 
                c("Denver Bike Shop", "Kansas City 29ers"), 128] <- 2

# visualize the final results
 ggplot(pca_final, aes(x = PC1, y = PC2, color = factor(groups)))+
  geom_point(aes(text = rownames(pca_final)))+
  labs(color = "Clusters")
```

    ## Warning: Ignoring unknown aesthetics: text

![](bike_clustering_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
# ggplotly(p_final, tooltip = c("text", "x", "y"))  %>%
#   layout(legend = list(x=.9, y=.99))
```
