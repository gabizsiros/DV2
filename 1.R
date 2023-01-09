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

admit <- as.data.frame(UCBAdmissions)

                