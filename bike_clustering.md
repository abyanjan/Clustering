Bike Clustering
================
Ajay Byanjankar
2019-05-02

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

``` r
library(readxl)
```

### Read Data

``` r
customers <- read_excel('Bike data/bikeshops.xlsx')
products  <- read_excel('Bike data/bikes.xlsx')
orders    <- read_excel('Bike data/orders.xlsx') %>% select(-1)
```

    ## New names:
    ## * `` -> ...1

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
  DT::datatable()
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<div id="htmlwidget-eb46e62db883e61430c9" class="datatables html-widget" style="width:100%;height:auto;">

</div>

<script type="application/json" data-for="htmlwidget-eb46e62db883e61430c9">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97"],["Scalpel-Si Carbon 3","Jekyll Carbon 4","Scalpel 29 Carbon Race","Trigger Carbon 3","Habit Carbon 2","Trigger Carbon 4","Catalyst 4","Jekyll Carbon 2","Supersix Evo Hi-Mod Dura Ace 2","Trigger Carbon 2","F-Si Black Inc.","SuperX Hi-Mod CX1","Habit Carbon SE","CAAD12 Disc Dura Ace","Scalpel-Si Black Inc.","Beast of the East 2","Fat CAAD1","Jekyll Carbon 1","Trail 5","Scalpel-Si Hi-Mod 1","Bad Habit 1","Scalpel-Si Carbon 4","F-Si 1","Habit Carbon 1","Slice Hi-Mod Black Inc.","Trail 4","F-Si Carbon 2","Scalpel 29 Carbon 2","Scalpel-Si 5","F-Si Hi-Mod 1","Scalpel 29 Carbon 3","Synapse Hi-Mod Dura Ace","Jekyll Carbon 3","Trail 2","Scalpel 29 4","Trigger Carbon 1","Synapse Carbon Disc Ultegra D12","Scalpel-Si Carbon 2","Beast of the East 1","Habit Hi-Mod Black Inc.","CAAD12 Red","Habit 4","Slice Ultegra D12","Synapse Carbon Ultegra 3","F-Si Hi-Mod Team","Scalpel-Si Race","Synapse Hi-Mod Disc Black Inc.","Habit Carbon 3","Supersix Evo Hi-Mod Utegra","Supersix Evo Hi-Mod Team","Supersix Evo Ultegra 3","F-Si Carbon 4","F-Si 3","Supersix Evo Black Inc.","Synapse Carbon Disc Ultegra","F-Si 2","Trail 3","Slice Hi-Mod Dura Ace D12","Fat CAAD2","Beast of the East 3","Synapse Hi-Mod Disc Red","Catalyst 2","Synapse Hi-Mod Disc Ultegra","Catalyst 1","Supersix Evo Hi-Mod Dura Ace 1","Trail 1","Supersix Evo Red","Habit 5","Bad Habit 2","Habit 6","CAAD12 Black Inc","Catalyst 3","Synapse Disc Adventure","Synapse Carbon 105","CAAD8 Claris","CAAD8 105","Supersix Evo Ultegra 4","CAAD12 Ultegra","SuperX Ultegra","Synapse Carbon Disc 105","Synapse Claris","CAAD8 Tiagra","CAAD Disc Ultegra","Supersix Evo 105","Synapse Disc 105","CAAD12 105","Synapse Sora","CAAD12 Disc 105","Supersix Evo Tiagra","Synapse Disc Tiagra","CAAD8 Sora","Slice 105","Slice Ultegra","SuperX 105","SuperX Rival CX1","Syapse Carbon Tiagra","Synapse Carbon Ultegra 4"],["Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Road","Mountain","Mountain","Road","Mountain","Road","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Road","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Road","Mountain","Mountain","Mountain","Mountain","Road","Mountain","Mountain","Mountain","Road","Mountain","Road","Road","Mountain","Mountain","Road","Mountain","Road","Road","Road","Mountain","Mountain","Road","Road","Mountain","Mountain","Road","Mountain","Mountain","Road","Mountain","Road","Mountain","Road","Mountain","Road","Mountain","Mountain","Mountain","Road","Mountain","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road","Road"],["Cross Country Race","Over Mountain","Cross Country Race","Over Mountain","Trail","Over Mountain","Sport","Over Mountain","Elite Road","Over Mountain","Cross Country Race","Cyclocross","Trail","Elite Road","Cross Country Race","Trail","Fat Bike","Over Mountain","Sport","Cross Country Race","Trail","Cross Country Race","Cross Country Race","Trail","Triathalon","Sport","Cross Country Race","Cross Country Race","Cross Country Race","Cross Country Race","Cross Country Race","Endurance Road","Over Mountain","Sport","Cross Country Race","Over Mountain","Endurance Road","Cross Country Race","Trail","Trail","Elite Road","Trail","Triathalon","Endurance Road","Cross Country Race","Cross Country Race","Endurance Road","Trail","Elite Road","Elite Road","Elite Road","Cross Country Race","Cross Country Race","Elite Road","Endurance Road","Cross Country Race","Sport","Triathalon","Fat Bike","Trail","Endurance Road","Sport","Endurance Road","Sport","Elite Road","Sport","Elite Road","Trail","Trail","Trail","Elite Road","Sport","Endurance Road","Endurance Road","Elite Road","Elite Road","Elite Road","Elite Road","Cyclocross","Endurance Road","Endurance Road","Elite Road","Elite Road","Elite Road","Endurance Road","Elite Road","Endurance Road","Elite Road","Elite Road","Endurance Road","Elite Road","Triathalon","Triathalon","Cyclocross","Cyclocross","Endurance Road","Endurance Road"],["Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Aluminum","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Aluminum","Carbon","Aluminum","Aluminum","Carbon","Aluminum","Carbon","Aluminum","Carbon","Aluminum","Carbon","Carbon","Aluminum","Carbon","Carbon","Aluminum","Carbon","Carbon","Carbon","Carbon","Aluminum","Aluminum","Carbon","Carbon","Carbon","Aluminum","Carbon","Aluminum","Aluminum","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Aluminum","Carbon","Carbon","Aluminum","Aluminum","Carbon","Aluminum","Aluminum","Carbon","Aluminum","Carbon","Aluminum","Carbon","Aluminum","Carbon","Aluminum","Aluminum","Aluminum","Aluminum","Aluminum","Aluminum","Carbon","Aluminum","Aluminum","Carbon","Aluminum","Carbon","Carbon","Aluminum","Aluminum","Aluminum","Carbon","Aluminum","Aluminum","Aluminum","Aluminum","Carbon","Aluminum","Aluminum","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon"],["[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)"],[0.0342691993119161,0.0302818352642909,0.0280390754179069,0.0259353267491381,0.0233749512520283,0.0232605721297754,0.0215639145265098,0.0210791784957579,0.0210578334309651,0.0210433339994243,0.0204015287168339,0.0203452288163781,0.0201958948584622,0.0195694786680832,0.0195387004836603,0.0193108318998251,0.019290376495703,0.018505193227778,0.0184778921904038,0.0180853005564413,0.0178429325410654,0.0177866326406096,0.0174094300988585,0.0167522357240567,0.0164058113667285,0.0159978306405546,0.0156043493459215,0.01458534152158,0.0144923074641199,0.0139290368074487,0.0135151001086934,0.0134741893004491,0.0132881211855289,0.0132727320933175,0.0131378975669424,0.0126881163718535,0.0126104714066049,0.0122546139296467,0.0121248457151823,0.0115197745895961,0.0112560615094274,0.0112509951975166,0.0111843725167601,0.0108779693070057,0.0107132462568784,0.00963793853208108,0.00916770193286998,0.008960288753157,0.00876915432632612,0.00875376523411468,0.0087333098299925,0.00873242016932185,0.00846959674982373,0.00822633907377713,0.00796262599360835,0.00781240237502183,0.00781240237502183,0.00764172335231312,0.00762126794819094,0.00754868929485305,0.0074351998332708,0.0073208207110179,0.00700080773039332,0.00679428421135099,0.005666853237338,0.00555336377575574,0.00546032971829567,0.00528965069558697,0.00457615642032927,0.00423479837491186,0.00414176431745179,0.00320040145835894,0.00255948583643913,0.00229577275627036,0.00214554913768383,0.00141159945830395,0.00141159945830395,0.00114788637813518,0.00114788637813518,0.00114788637813518,0.00114788637813518,0.00105485232067511,0.000884173297966401,0.000884173297966401,0.000791139240506329,0.000527426160337553,0.000527426160337553,0.000263713080168776,0.000263713080168776,0.000263713080168776,0,0,0,0,0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>model<\/th>\n      <th>category1<\/th>\n      <th>category2<\/th>\n      <th>frame<\/th>\n      <th>price<\/th>\n      <th>X1<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":6},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

### cluster 2

``` r
cluster_trends %>% 
  select(c(1:5),X2) %>% 
  arrange(desc(X2)) %>% 
  DT::datatable()
```

<!--html_preserve-->

<div id="htmlwidget-719e1d15a11c232d4d4a" class="datatables html-widget" style="width:100%;height:auto;">

</div>

<script type="application/json" data-for="htmlwidget-719e1d15a11c232d4d4a">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97"],["Slice Ultegra","Trigger Carbon 4","CAAD12 105","Beast of the East 3","Trail 1","Bad Habit 1","F-Si Carbon 4","CAAD12 Disc 105","Synapse Disc 105","Trail 2","Synapse Carbon Disc 105","CAAD8 Claris","Supersix Evo Tiagra","CAAD8 Tiagra","Supersix Evo Ultegra 4","Slice Ultegra D12","Jekyll Carbon 2","Fat CAAD2","Habit 6","Scalpel 29 4","Catalyst 1","Catalyst 2","Habit 4","Scalpel-Si Black Inc.","Supersix Evo 105","CAAD8 Sora","F-Si 2","Supersix Evo Hi-Mod Dura Ace 1","Synapse Carbon 105","Synapse Carbon Disc Ultegra","Scalpel 29 Carbon 3","F-Si 3","SuperX Rival CX1","Synapse Carbon Ultegra 3","Synapse Carbon Ultegra 4","Synapse Disc Adventure","Synapse Disc Tiagra","Synapse Sora","Trail 5","CAAD Disc Ultegra","Scalpel 29 Carbon 2","Slice 105","Supersix Evo Ultegra 3","Syapse Carbon Tiagra","CAAD12 Ultegra","Habit Carbon 3","Catalyst 3","Catalyst 4","F-Si 1","Supersix Evo Hi-Mod Utegra","Synapse Claris","Jekyll Carbon 1","SuperX Ultegra","Trail 4","CAAD12 Red","F-Si Carbon 2","Habit Carbon 2","Habit Carbon SE","Scalpel 29 Carbon Race","Synapse Hi-Mod Disc Red","Scalpel-Si Hi-Mod 1","Supersix Evo Black Inc.","SuperX Hi-Mod CX1","Beast of the East 2","Habit Carbon 1","Jekyll Carbon 3","Synapse Hi-Mod Disc Black Inc.","CAAD8 105","Scalpel-Si 5","Scalpel-Si Carbon 2","Scalpel-Si Carbon 4","Trail 3","CAAD12 Black Inc","Habit 5","Habit Hi-Mod Black Inc.","Jekyll Carbon 4","Scalpel-Si Carbon 3","Scalpel-Si Race","Slice Hi-Mod Dura Ace D12","Synapse Hi-Mod Disc Ultegra","Beast of the East 1","CAAD12 Disc Dura Ace","F-Si Hi-Mod 1","F-Si Hi-Mod Team","Slice Hi-Mod Black Inc.","Supersix Evo Hi-Mod Team","Trigger Carbon 1","Trigger Carbon 3","Bad Habit 2","F-Si Black Inc.","Fat CAAD1","Supersix Evo Hi-Mod Dura Ace 2","Supersix Evo Red","SuperX 105","Synapse Carbon Disc Ultegra D12","Synapse Hi-Mod Dura Ace","Trigger Carbon 2"],["Road","Mountain","Road","Mountain","Mountain","Mountain","Mountain","Road","Road","Mountain","Road","Road","Road","Road","Road","Road","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Mountain","Road","Road","Mountain","Road","Road","Road","Mountain","Mountain","Road","Road","Road","Road","Road","Road","Mountain","Road","Mountain","Road","Road","Road","Road","Mountain","Mountain","Mountain","Mountain","Road","Road","Mountain","Road","Mountain","Road","Mountain","Mountain","Mountain","Mountain","Road","Mountain","Road","Road","Mountain","Mountain","Mountain","Road","Road","Mountain","Mountain","Mountain","Mountain","Road","Mountain","Mountain","Mountain","Mountain","Mountain","Road","Road","Mountain","Road","Mountain","Mountain","Road","Road","Mountain","Mountain","Mountain","Mountain","Mountain","Road","Road","Road","Road","Road","Mountain"],["Triathalon","Over Mountain","Elite Road","Trail","Sport","Trail","Cross Country Race","Elite Road","Endurance Road","Sport","Endurance Road","Elite Road","Elite Road","Elite Road","Elite Road","Triathalon","Over Mountain","Fat Bike","Trail","Cross Country Race","Sport","Sport","Trail","Cross Country Race","Elite Road","Elite Road","Cross Country Race","Elite Road","Endurance Road","Endurance Road","Cross Country Race","Cross Country Race","Cyclocross","Endurance Road","Endurance Road","Endurance Road","Endurance Road","Endurance Road","Sport","Elite Road","Cross Country Race","Triathalon","Elite Road","Endurance Road","Elite Road","Trail","Sport","Sport","Cross Country Race","Elite Road","Endurance Road","Over Mountain","Cyclocross","Sport","Elite Road","Cross Country Race","Trail","Trail","Cross Country Race","Endurance Road","Cross Country Race","Elite Road","Cyclocross","Trail","Trail","Over Mountain","Endurance Road","Elite Road","Cross Country Race","Cross Country Race","Cross Country Race","Sport","Elite Road","Trail","Trail","Over Mountain","Cross Country Race","Cross Country Race","Triathalon","Endurance Road","Trail","Elite Road","Cross Country Race","Cross Country Race","Triathalon","Elite Road","Over Mountain","Over Mountain","Trail","Cross Country Race","Fat Bike","Elite Road","Elite Road","Cyclocross","Endurance Road","Endurance Road","Over Mountain"],["Carbon","Carbon","Aluminum","Aluminum","Aluminum","Aluminum","Carbon","Aluminum","Aluminum","Aluminum","Carbon","Aluminum","Carbon","Aluminum","Carbon","Carbon","Carbon","Aluminum","Aluminum","Aluminum","Aluminum","Aluminum","Aluminum","Carbon","Carbon","Aluminum","Aluminum","Carbon","Carbon","Carbon","Carbon","Aluminum","Carbon","Carbon","Carbon","Aluminum","Aluminum","Aluminum","Aluminum","Aluminum","Carbon","Carbon","Carbon","Carbon","Aluminum","Carbon","Aluminum","Aluminum","Aluminum","Carbon","Aluminum","Carbon","Carbon","Aluminum","Aluminum","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Aluminum","Carbon","Carbon","Carbon","Aluminum","Aluminum","Carbon","Carbon","Aluminum","Aluminum","Aluminum","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Aluminum","Aluminum","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon","Aluminum","Carbon","Aluminum","Carbon","Carbon","Carbon","Carbon","Carbon","Carbon"],["[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[ 415, 3500)","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]","[3500,12790]","[ 415, 3500)","[3500,12790]","[3500,12790]","[3500,12790]"],[0.0554531490015361,0.0304147465437788,0.0270792187842879,0.0263331138907176,0.0249396532806671,0.0229975861312267,0.0224489795918367,0.0209567698046961,0.0209567698046961,0.0203094140882159,0.0196620583717358,0.0195633091946456,0.017522492868115,0.016973886328725,0.0168751371516348,0.0162277814351547,0.0149330700021944,0.0148343208251042,0.0148343208251042,0.0148343208251042,0.0141869651086241,0.0141869651086241,0.013539609392144,0.013539609392144,0.013539609392144,0.0127935044985736,0.0127935044985736,0.0127935044985736,0.0127935044985736,0.0127935044985736,0.0121461487820935,0.0114987930656133,0.0114987930656133,0.0114987930656133,0.0114987930656133,0.0114987930656133,0.0114987930656133,0.0114987930656133,0.0114987930656133,0.0108514373491332,0.0108514373491332,0.0108514373491332,0.0108514373491332,0.0108514373491332,0.0102040816326531,0.0102040816326531,0.0101053324555629,0.00945797673908273,0.00945797673908273,0.00945797673908273,0.00945797673908273,0.00881062102260259,0.00881062102260259,0.00816326530612245,0.00741716041255212,0.00741716041255212,0.00741716041255212,0.00741716041255212,0.00741716041255212,0.00741716041255212,0.00676980469607198,0.00676980469607198,0.00676980469607198,0.00537634408602151,0.00537634408602151,0.00537634408602151,0.00537634408602151,0.00472898836954137,0.00472898836954137,0.00472898836954137,0.00472898836954137,0.00472898836954137,0.00408163265306122,0.00408163265306122,0.00408163265306122,0.00408163265306122,0.00408163265306122,0.00408163265306122,0.00408163265306122,0.00408163265306122,0.00268817204301075,0.00268817204301075,0.00268817204301075,0.00268817204301075,0.00268817204301075,0.00268817204301075,0.00268817204301075,0.00268817204301075,0.00204081632653061,0.00204081632653061,0.00204081632653061,0.00204081632653061,0.00204081632653061,0.00204081632653061,0,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>model<\/th>\n      <th>category1<\/th>\n      <th>category2<\/th>\n      <th>frame<\/th>\n      <th>price<\/th>\n      <th>X2<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":6},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

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
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
pca <- prcomp(km_df, center = T, scale. = T)

p1 <- summary(pca)$importance %>% t() %>% 
  data.frame() %>% 
  rownames_to_column('summary') %>% 
  slice(1:9) %>% rename(pca = summary) %>% 
  ggplot(aes(x  = reorder(pca,-Proportion.of.Variance), y = Proportion.of.Variance))+
           geom_col(aes(fill = as.factor(Proportion.of.Variance)), show.legend = FALSE)+
  scale_fill_brewer(palette = 'Blues')+
  labs(x = "") 
  
ggplotly(p1) %>% 
  hide_legend()
```

<!--html_preserve-->

<div id="htmlwidget-787b5dc103f5637d9c88" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-787b5dc103f5637d9c88">{"x":{"data":[{"orientation":"v","width":0.899999999999999,"base":0,"x":[9],"y":[0.03205],"text":"as.factor(Proportion.of.Variance): 0.03205<br />reorder(pca, -Proportion.of.Variance): PC9<br />Proportion.of.Variance: 0.03205","type":"bar","marker":{"autocolorscale":false,"color":"rgba(247,251,255,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.03205","legendgroup":"0.03205","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.899999999999999,"base":0,"x":[8],"y":[0.03368],"text":"as.factor(Proportion.of.Variance): 0.03368<br />reorder(pca, -Proportion.of.Variance): PC8<br />Proportion.of.Variance: 0.03368","type":"bar","marker":{"autocolorscale":false,"color":"rgba(222,235,247,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.03368","legendgroup":"0.03368","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[7],"y":[0.03577],"text":"as.factor(Proportion.of.Variance): 0.03577<br />reorder(pca, -Proportion.of.Variance): PC7<br />Proportion.of.Variance: 0.03577","type":"bar","marker":{"autocolorscale":false,"color":"rgba(198,219,239,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.03577","legendgroup":"0.03577","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[6],"y":[0.04273],"text":"as.factor(Proportion.of.Variance): 0.04273<br />reorder(pca, -Proportion.of.Variance): PC6<br />Proportion.of.Variance: 0.04273","type":"bar","marker":{"autocolorscale":false,"color":"rgba(158,202,225,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.04273","legendgroup":"0.04273","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[5],"y":[0.0462],"text":"as.factor(Proportion.of.Variance): 0.0462<br />reorder(pca, -Proportion.of.Variance): PC5<br />Proportion.of.Variance: 0.04620","type":"bar","marker":{"autocolorscale":false,"color":"rgba(107,174,214,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.0462","legendgroup":"0.0462","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[4],"y":[0.04913],"text":"as.factor(Proportion.of.Variance): 0.04913<br />reorder(pca, -Proportion.of.Variance): PC4<br />Proportion.of.Variance: 0.04913","type":"bar","marker":{"autocolorscale":false,"color":"rgba(66,146,198,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.04913","legendgroup":"0.04913","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[3],"y":[0.0513],"text":"as.factor(Proportion.of.Variance): 0.0513<br />reorder(pca, -Proportion.of.Variance): PC3<br />Proportion.of.Variance: 0.05130","type":"bar","marker":{"autocolorscale":false,"color":"rgba(33,113,181,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.0513","legendgroup":"0.0513","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[2],"y":[0.18364],"text":"as.factor(Proportion.of.Variance): 0.18364<br />reorder(pca, -Proportion.of.Variance): PC2<br />Proportion.of.Variance: 0.18364","type":"bar","marker":{"autocolorscale":false,"color":"rgba(8,81,156,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.18364","legendgroup":"0.18364","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.9,"base":0,"x":[1],"y":[0.25621],"text":"as.factor(Proportion.of.Variance): 0.25621<br />reorder(pca, -Proportion.of.Variance): PC1<br />Proportion.of.Variance: 0.25621","type":"bar","marker":{"autocolorscale":false,"color":"rgba(8,48,107,1)","line":{"width":1.88976377952756,"color":"transparent"}},"name":"0.25621","legendgroup":"0.25621","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":25.5707762557078,"l":43.1050228310502},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,9.6],"tickmode":"array","ticktext":["PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9"],"tickvals":[1,2,3,4,5,6,7,8,9],"categoryorder":"array","categoryarray":["PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.0128105,0.2690205],"tickmode":"array","ticktext":["0.0","0.1","0.2"],"tickvals":[0,0.1,0.2],"categoryorder":"array","categoryarray":["0.0","0.1","0.2"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"Proportion.of.Variance","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":1},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"16543f627bd6":{"fill":{},"x":{},"y":{},"type":"bar"}},"cur_data":"16543f627bd6","visdat":{"16543f627bd6":["function (y) ","x"]},".hideLegend":true,"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->

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

p4 <- ggplot(pca4_df, aes(x = PC1, y = PC2, color = factor(groups)))+
  geom_point(aes(text = rownames(pca4_df)))+
  labs(color = "Clusters")
```

    ## Warning: Ignoring unknown aesthetics: text

``` r
ggplotly(p4, tooltip = c("text", "x", "y"))  %>%
  layout(legend = list(x=.9, y=.99))
```

<!--html_preserve-->

<div id="htmlwidget-0fd3b4af0f9162bc7498" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-0fd3b4af0f9162bc7498">{"x":{"data":[{"x":[-4.92692296853819,-5.30494595982227,-4.73999300622586,-5.54863321261405,-4.65260781865184,-4.97371842861661,-4.89636990074646,-4.86387808608233],"y":[3.63706404700509,1.09098381958151,2.35871996898182,1.23459731031489,1.49045197393246,2.10595662989786,1.90321235940486,3.55692280726753],"text":["Ann Arbor Speed<br />PC1: -4.926923<br />PC2:  3.637064","Austin Cruisers<br />PC1: -5.304946<br />PC2:  1.090984","Indianapolis Velocipedes<br />PC1: -4.739993<br />PC2:  2.358720","Miami Race Equipment<br />PC1: -5.548633<br />PC2:  1.234597","Nashville Cruisers<br />PC1: -4.652608<br />PC2:  1.490452","New Orleans Velocipedes<br />PC1: -4.973718<br />PC2:  2.105957","Oklahoma City Race Equipment<br />PC1: -4.896370<br />PC2:  1.903212","Seattle Race Equipment<br />PC1: -4.863878<br />PC2:  3.556923"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.818640954486994,-2.66388152723996,1.47119959496036,-1.20320627929888,1.42649633867707,-2.48377862308423,-2.60873133016289,-1.99134240524398,-1.92734622847081,-2.10346636128825,-1.8417009827668,-2.32384791861962,-1.42054912252287],"y":[-2.41869982101585,-4.07719086638929,-6.87491579454191,-1.61372648263337,-7.19827044441507,-3.10747734376947,-2.40625793174613,-2.11349951494081,-2.89103475418116,-3.09645262295974,-3.34512889949823,-2.74057101160882,-2.98433271458278],"text":["Albuquerque Cycles<br />PC1: -0.818641<br />PC2: -2.418700","Dallas Cycles<br />PC1: -2.663882<br />PC2: -4.077191","Denver Bike Shop<br />PC1:  1.471200<br />PC2: -6.874916","Detroit Cycles<br />PC1: -1.203206<br />PC2: -1.613726","Kansas City 29ers<br />PC1:  1.426496<br />PC2: -7.198270","Los Angeles Cycles<br />PC1: -2.483779<br />PC2: -3.107477","Minneapolis Bike Shop<br />PC1: -2.608731<br />PC2: -2.406258","New York Cycles<br />PC1: -1.991342<br />PC2: -2.113500","Philadelphia Bike Shop<br />PC1: -1.927346<br />PC2: -2.891035","Phoenix Bi-peds<br />PC1: -2.103466<br />PC2: -3.096453","Portland Bi-peds<br />PC1: -1.841701<br />PC2: -3.345129","Providence Bi-peds<br />PC1: -2.323848<br />PC2: -2.740571","San Antonio Bike Shop<br />PC1: -1.420549<br />PC2: -2.984333"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(124,174,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)"}},"hoveron":"points","name":"2","legendgroup":"2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.72285868162417,4.15384627129127,5.00136335647031,4.58108072564036,3.387349598334,4.83554406406703],"y":[6.83646395510078,5.32865310521725,6.52508260683642,6.22239174284389,7.10320887157744,6.01851446536024],"text":["Cincinnati Speed<br />PC1:  3.722859<br />PC2:  6.836464","Columbus Race Equipment<br />PC1:  4.153846<br />PC2:  5.328653","Las Vegas Cycles<br />PC1:  5.001363<br />PC2:  6.525083","Louisville Race Equipment<br />PC1:  4.581081<br />PC2:  6.222392","San Francisco Cruisers<br />PC1:  3.387350<br />PC2:  7.103209","Wichita Speed<br />PC1:  4.835544<br />PC2:  6.018514"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"3","legendgroup":"3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[10.383137250638,10.024376190514,12.3063090422663],"y":[-3.32985012512424,-2.5331433938605,-4.68167194205467],"text":["Ithaca Mountain Climbers<br />PC1: 10.383137<br />PC2: -3.329850","Pittsburgh Mountain Machines<br />PC1: 10.024376<br />PC2: -2.533143","Tampa 29ers<br />PC1: 12.306309<br />PC2: -4.681672"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(199,124,255,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)"}},"hoveron":"points","name":"4","legendgroup":"4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-6.44138032535807,13.1990561550103],"tickmode":"array","ticktext":["-5","0","5","10"],"tickvals":[-5,0,5,10],"categoryorder":"array","categoryarray":["-5","0","5","10"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"PC1","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-7.91334441021469,7.81828283737707],"tickmode":"array","ticktext":["-4","0","4"],"tickvals":[-4,8.88178419700125e-016,4],"categoryorder":"array","categoryarray":["-4","0","4"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"PC2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.99,"x":0.9},"annotations":[{"text":"Clusters","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"1654f0fe19":{"text":{},"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"1654f0fe19","visdat":{"1654f0fe19":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->

``` r
# plot the 5 clusters using PCA1 and PCA 2

p5 <- ggplot(pca5_df, aes(x = PC1, y = PC2, color = as.factor(groups)))+
  geom_point(aes(text = rownames(pca5_df)))+
  labs(color = "Clusters")
```

    ## Warning: Ignoring unknown aesthetics: text

``` r
ggplotly(p5, tooltip = c("text", "x", "y"))  %>%
  layout(legend = list(x=.9, y=.99))
```

<!--html_preserve-->

<div id="htmlwidget-9a4fa61cfaa35f4ca524" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-9a4fa61cfaa35f4ca524">{"x":{"data":[{"x":[10.383137250638,10.024376190514,12.3063090422663],"y":[-3.32985012512424,-2.5331433938605,-4.68167194205467],"text":["Ithaca Mountain Climbers<br />PC1: 10.383137<br />PC2: -3.329850","Pittsburgh Mountain Machines<br />PC1: 10.024376<br />PC2: -2.533143","Tampa 29ers<br />PC1: 12.306309<br />PC2: -4.681672"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-1.92734622847081,-1.42054912252287],"y":[-2.89103475418116,-2.98433271458278],"text":["Philadelphia Bike Shop<br />PC1: -1.927346<br />PC2: -2.891035","San Antonio Bike Shop<br />PC1: -1.420549<br />PC2: -2.984333"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(163,165,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(163,165,0,1)"}},"hoveron":"points","name":"2","legendgroup":"2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-4.92692296853819,-5.30494595982227,-4.73999300622586,-5.54863321261405,-4.65260781865184,-4.97371842861661,-4.89636990074646,-4.86387808608233],"y":[3.63706404700509,1.09098381958151,2.35871996898182,1.23459731031489,1.49045197393246,2.10595662989786,1.90321235940486,3.55692280726753],"text":["Ann Arbor Speed<br />PC1: -4.926923<br />PC2:  3.637064","Austin Cruisers<br />PC1: -5.304946<br />PC2:  1.090984","Indianapolis Velocipedes<br />PC1: -4.739993<br />PC2:  2.358720","Miami Race Equipment<br />PC1: -5.548633<br />PC2:  1.234597","Nashville Cruisers<br />PC1: -4.652608<br />PC2:  1.490452","New Orleans Velocipedes<br />PC1: -4.973718<br />PC2:  2.105957","Oklahoma City Race Equipment<br />PC1: -4.896370<br />PC2:  1.903212","Seattle Race Equipment<br />PC1: -4.863878<br />PC2:  3.556923"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,125,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,125,1)"}},"hoveron":"points","name":"3","legendgroup":"3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.818640954486994,-2.66388152723996,1.47119959496036,-1.20320627929888,1.42649633867707,-2.48377862308423,-2.60873133016289,-1.99134240524398,-2.10346636128825,-1.8417009827668,-2.32384791861962],"y":[-2.41869982101585,-4.07719086638929,-6.87491579454191,-1.61372648263337,-7.19827044441507,-3.10747734376947,-2.40625793174613,-2.11349951494081,-3.09645262295974,-3.34512889949823,-2.74057101160882],"text":["Albuquerque Cycles<br />PC1: -0.818641<br />PC2: -2.418700","Dallas Cycles<br />PC1: -2.663882<br />PC2: -4.077191","Denver Bike Shop<br />PC1:  1.471200<br />PC2: -6.874916","Detroit Cycles<br />PC1: -1.203206<br />PC2: -1.613726","Kansas City 29ers<br />PC1:  1.426496<br />PC2: -7.198270","Los Angeles Cycles<br />PC1: -2.483779<br />PC2: -3.107477","Minneapolis Bike Shop<br />PC1: -2.608731<br />PC2: -2.406258","New York Cycles<br />PC1: -1.991342<br />PC2: -2.113500","Phoenix Bi-peds<br />PC1: -2.103466<br />PC2: -3.096453","Portland Bi-peds<br />PC1: -1.841701<br />PC2: -3.345129","Providence Bi-peds<br />PC1: -2.323848<br />PC2: -2.740571"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,176,246,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,176,246,1)"}},"hoveron":"points","name":"4","legendgroup":"4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.72285868162417,4.15384627129127,5.00136335647031,4.58108072564036,3.387349598334,4.83554406406703],"y":[6.83646395510078,5.32865310521725,6.52508260683642,6.22239174284389,7.10320887157744,6.01851446536024],"text":["Cincinnati Speed<br />PC1:  3.722859<br />PC2:  6.836464","Columbus Race Equipment<br />PC1:  4.153846<br />PC2:  5.328653","Las Vegas Cycles<br />PC1:  5.001363<br />PC2:  6.525083","Louisville Race Equipment<br />PC1:  4.581081<br />PC2:  6.222392","San Francisco Cruisers<br />PC1:  3.387350<br />PC2:  7.103209","Wichita Speed<br />PC1:  4.835544<br />PC2:  6.018514"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(231,107,243,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(231,107,243,1)"}},"hoveron":"points","name":"5","legendgroup":"5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-6.44138032535807,13.1990561550103],"tickmode":"array","ticktext":["-5","0","5","10"],"tickvals":[-5,0,5,10],"categoryorder":"array","categoryarray":["-5","0","5","10"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"PC1","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-7.91334441021469,7.81828283737707],"tickmode":"array","ticktext":["-4","0","4"],"tickvals":[-4,8.88178419700125e-016,4],"categoryorder":"array","categoryarray":["-4","0","4"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"PC2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.99,"x":0.9},"annotations":[{"text":"Clusters","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"165431a0593e":{"text":{},"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"165431a0593e","visdat":{"165431a0593e":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->

IT seems that cluster 2 is being misclassied , so lets classify cluster
2 as 4 and 2 of the points far from cluster 4 as cluster 2.

``` r
pca_final <- pca5_df
pca_final[rownames(pca_final) %in% 
                c("San Antonio Bike Shop", "Philadelphia Bike Shop"), 128] <- 4
pca_final[rownames(pca_final) %in% 
                c("Denver Bike Shop", "Kansas City 29ers"), 128] <- 2

# visualize the final results

p_final <- ggplot(pca_final, aes(x = PC1, y = PC2, color = factor(groups)))+
  geom_point(aes(text = rownames(pca_final)))+
  labs(color = "Clusters")
```

    ## Warning: Ignoring unknown aesthetics: text

``` r
ggplotly(p_final, tooltip = c("text", "x", "y"))  %>%
  layout(legend = list(x=.9, y=.99))
```

<!--html_preserve-->

<div id="htmlwidget-0e60bf86aa5fc14e297b" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-0e60bf86aa5fc14e297b">{"x":{"data":[{"x":[10.383137250638,10.024376190514,12.3063090422663],"y":[-3.32985012512424,-2.5331433938605,-4.68167194205467],"text":["Ithaca Mountain Climbers<br />PC1: 10.383137<br />PC2: -3.329850","Pittsburgh Mountain Machines<br />PC1: 10.024376<br />PC2: -2.533143","Tampa 29ers<br />PC1: 12.306309<br />PC2: -4.681672"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"1","legendgroup":"1","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.47119959496036,1.42649633867707],"y":[-6.87491579454191,-7.19827044441507],"text":["Denver Bike Shop<br />PC1:  1.471200<br />PC2: -6.874916","Kansas City 29ers<br />PC1:  1.426496<br />PC2: -7.198270"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(163,165,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(163,165,0,1)"}},"hoveron":"points","name":"2","legendgroup":"2","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-4.92692296853819,-5.30494595982227,-4.73999300622586,-5.54863321261405,-4.65260781865184,-4.97371842861661,-4.89636990074646,-4.86387808608233],"y":[3.63706404700509,1.09098381958151,2.35871996898182,1.23459731031489,1.49045197393246,2.10595662989786,1.90321235940486,3.55692280726753],"text":["Ann Arbor Speed<br />PC1: -4.926923<br />PC2:  3.637064","Austin Cruisers<br />PC1: -5.304946<br />PC2:  1.090984","Indianapolis Velocipedes<br />PC1: -4.739993<br />PC2:  2.358720","Miami Race Equipment<br />PC1: -5.548633<br />PC2:  1.234597","Nashville Cruisers<br />PC1: -4.652608<br />PC2:  1.490452","New Orleans Velocipedes<br />PC1: -4.973718<br />PC2:  2.105957","Oklahoma City Race Equipment<br />PC1: -4.896370<br />PC2:  1.903212","Seattle Race Equipment<br />PC1: -4.863878<br />PC2:  3.556923"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,125,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,125,1)"}},"hoveron":"points","name":"3","legendgroup":"3","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.818640954486994,-2.66388152723996,-1.20320627929888,-2.48377862308423,-2.60873133016289,-1.99134240524398,-1.92734622847081,-2.10346636128825,-1.8417009827668,-2.32384791861962,-1.42054912252287],"y":[-2.41869982101585,-4.07719086638929,-1.61372648263337,-3.10747734376947,-2.40625793174613,-2.11349951494081,-2.89103475418116,-3.09645262295974,-3.34512889949823,-2.74057101160882,-2.98433271458278],"text":["Albuquerque Cycles<br />PC1: -0.818641<br />PC2: -2.418700","Dallas Cycles<br />PC1: -2.663882<br />PC2: -4.077191","Detroit Cycles<br />PC1: -1.203206<br />PC2: -1.613726","Los Angeles Cycles<br />PC1: -2.483779<br />PC2: -3.107477","Minneapolis Bike Shop<br />PC1: -2.608731<br />PC2: -2.406258","New York Cycles<br />PC1: -1.991342<br />PC2: -2.113500","Philadelphia Bike Shop<br />PC1: -1.927346<br />PC2: -2.891035","Phoenix Bi-peds<br />PC1: -2.103466<br />PC2: -3.096453","Portland Bi-peds<br />PC1: -1.841701<br />PC2: -3.345129","Providence Bi-peds<br />PC1: -2.323848<br />PC2: -2.740571","San Antonio Bike Shop<br />PC1: -1.420549<br />PC2: -2.984333"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,176,246,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,176,246,1)"}},"hoveron":"points","name":"4","legendgroup":"4","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.72285868162417,4.15384627129127,5.00136335647031,4.58108072564036,3.387349598334,4.83554406406703],"y":[6.83646395510078,5.32865310521725,6.52508260683642,6.22239174284389,7.10320887157744,6.01851446536024],"text":["Cincinnati Speed<br />PC1:  3.722859<br />PC2:  6.836464","Columbus Race Equipment<br />PC1:  4.153846<br />PC2:  5.328653","Las Vegas Cycles<br />PC1:  5.001363<br />PC2:  6.525083","Louisville Race Equipment<br />PC1:  4.581081<br />PC2:  6.222392","San Francisco Cruisers<br />PC1:  3.387350<br />PC2:  7.103209","Wichita Speed<br />PC1:  4.835544<br />PC2:  6.018514"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(231,107,243,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(231,107,243,1)"}},"hoveron":"points","name":"5","legendgroup":"5","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-6.44138032535807,13.1990561550103],"tickmode":"array","ticktext":["-5","0","5","10"],"tickvals":[-5,0,5,10],"categoryorder":"array","categoryarray":["-5","0","5","10"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"PC1","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-7.91334441021469,7.81828283737707],"tickmode":"array","ticktext":["-4","0","4"],"tickvals":[-4,8.88178419700125e-016,4],"categoryorder":"array","categoryarray":["-4","0","4"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"PC2","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.99,"x":0.9},"annotations":[{"text":"Clusters","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"16543a707952":{"text":{},"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"16543a707952","visdat":{"16543a707952":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->
