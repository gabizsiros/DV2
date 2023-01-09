source('http://bit.ly/CEU-R-shoes')
ls()
str(students)
plot(students$shoe, students$math)
abline(lm(math ~ shoe, students), col = 'red')
plot(students)

library(GGally)
ggpairs(students)

rm(list=ls())
ls(all.names = TRUE)
.secret

readLines('http://bit.ly/CEU-R-shoes')

##

download.file('https://bit.ly/hun-cities-distance', 'cities.xls')
library(readxl)
cities <- read_excel('cities.xls')

cities <- cities[, -1]
cities <- cities [-nrow(cities),]

mds <- cmdscale(as.dist(cities))
plot (mds)

text(mds[, 1],mds[,2],names(cities))

mds[,1] <- -1 * mds[,1]

mds <-  as.data.frame(mds)
mds$cities <- names(cities)

ggplot(mds) +
  geom_text_repel(aes(V1,V2, label = cities))+
  theme_classic()

library(ggrepel)


##

eurocities <- eurodist
mds <- cmdscale(as.dist(eurocities))
mds <- as.data.frame(mds)

mds$cities <- row.names(mds)

mds[,2] <- -1 * mds[,2]

ggplot(mds,aes(V1,V2)) +
   geom_point() +
    geom_text_repel(label = mds$cities) +
    theme_bw()

mtcars

mds <- as.data.frame(cmdscale(dist(scale(mtcars))))
mds$type <- row.names(mds)

ggplot(mds,aes(V1,V2)) +
  geom_point() +
  geom_text_repel(label = mds$type) +
  theme_bw()

mtcars

scale(mtcars)

berkeley <- as.data.frame(UCBAdmissions)

ggplot(berkeley, aes(Gender, Freq, fill = Admit)) +
  geom_col(position = 'fill') +
  scale_fill_manual(values = c("Admitted" = 'darkgreen',
                               "Rejected" = 'darkred'))


ggplot(berkeley, aes(Gender, Freq, fill = Admit)) +
  geom_col() +
  facet_wrap(~Dept) +
  scale_fill_manual(values = c("Admitted" = 'darkgreen',
                               "Rejected" = 'darkred'))


rm(list=ls())

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
  facet_wrap (~Species) +
  geom_smooth(method = 'lm')

iris


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))  +
  scale_fill_manual(values = c("setosa" = "green",
                               "versicolor" = "red",
                               "virginica" = 'blue')) +
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species),method = 'lm') +
  geom_smooth(method = 'lm', color = 'black') +
  theme_minimal() + 
  guides()


library(data.table)
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')

#bookings[i,j,by]

bookings[1]
bookings [price < 100 & holiday == 1]
bookings [price < 100][holiday == 1][1:5] 

#number of rows in "j" operator
bookings [price < 100 & holiday == 1, .N]

bookings [price < 100 & holiday == 1, mean(price)]

bookings [price < 100 & holiday == 1, summary(price)]

bookings [price < 100 & holiday == 1, hist(price)]



##functions, avg price of bookings on weekends
bookings[weekend == 1, mean(price)]
bookings[weekend == 0, mean(price)]
# by 
bookings[,mean(price), by = list(weekend,holiday,nnights)]

# "." instead of list. naming return column with list
bookings[,.( price = mean(price), 
             min =min(price), 
             max = max(price)), 
         by = .(weekend,holiday,nnights)]


bookings[, price_per_night := price / nnights]



features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
merge(bookings,features, all.x = TRUE)[is.na(city)]

#todo bookings per country level aggregated data on avg rating
countries <- features[, .(rating = mean(rating, na.rm = TRUE)), by = country][!is.na(country)]
countries[order(country)]

library(ggmap)
library(tidygeocoder)
countries <- data.table(geocode(countries,'country'))

library(maps)
data(worldMapEnv)
map('world', fill = TRUE, col=  1:20)

world <- map_data('world')

str(world)

map <- ggplot()+
  geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
  theme_void() +
  coord_fixed(1.3)

map + geom_point(data = countries, aes(long, lat, size = rating), color = 'orange')



map <- ggmap(get_stamenmap(bbox = c(left = min(countries$long)-5, 
                             bottom = min(countries$lat)-5, 
                             right = max(countries$long)+5, 
                             top = max(countries$lat)+5),
              zoom = 4,
              maptype = "watercolor"))

map + geom_point(data = countries, aes(long, lat, size = rating), color = 'red')


# anscombe

df <- anscombe

split(df,list(df$x1,df$x2,df$x3,df$x4))

plot(anscombe[,c(1,5)])
plot(anscombe[,c(2,6)])

mean(anscombe[,c(1)])


lapply(1:4, function(i) mean(anscombe[,c(i)]))

lapply(1:4, function(i) cor(anscombe[,c(i)],anscombe[,c(i+4)]))

lapply(1:4, function(i) data.frame(x = anscombe[,c(i)], y= anscombe[,c(i+4)], dataset = paste0("dataset",i)))


anscombe <- rbindlist(lapply(1:4, function(i) data.frame(x = anscombe[,c(i)], y= anscombe[,c(i+4)], dataset = paste0("dataset",i))))

ggplot(anscombe, aes (x,y)) +
  geom_point()+
  geom_smooth(method = 'lm', se= FALSE) +
  facet_wrap( ~ dataset) +
  theme_bw()


df <-datasauRus::datasaurus_dozen_wide


lapply(seq(from = 1, to = 26, by = 2), 
       function(i) data.frame (x = df[,c(i)]), y = df[,c(i+1)])




dino_df <- rbindlist(lapply(seq(from = 1, to = 26, by = 2), function(i) {
  data.frame(
    x = df[ ,c(i)], 
    y = df[ ,c(i+1)],
    dataset = substr(names(df)[i], 1,nchar(names(df)[i])-2))
}
))


ggplot(dino_df, aes (away_x,away_y)) +
  geom_point()+
  geom_smooth(method = 'lm', se= FALSE) +
  facet_wrap( ~ dataset) +
  theme_bw()

       