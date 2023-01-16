download.file(
  'https://stacks.stanford.edu/file/druid:rc343vz5889/data.zip',
  'Austria_boundary.zip')
download.file(
  'https://stacks.stanford.edu/file/druid:yv617vc9132/data.zip',
  'Austria_divisions.zip')
unzip('Austria_boundary.zip')
unzip('Austria_divisions.zip')

library(rgdal)
ogrInfo('.')

adm0 <- readOGR('.', 'AUT_adm0') #this cannot be passed on to ggplot

adm0 <- fortify(adm0) #transforms into dataframe

adm2 <- readOGR('.', 'AUT_adm2')

ggplot() +
  geom_path(data = adm2, aes(long,lat, group = group), color = 'orange') +
  geom_path(data = adm0, aes(long,lat, group = group), color = 'gray') +
  coord_fixed(1.3)


df$price_usd <- df$Price /380
df$price_eur <- df$Price /400

df[, price_usd := Price / 380]
df[, price_usd := Price / 400]


df[,c('price_usd','price_eur') := list(Price / 380, Price / 400)]
df[, ':=' (
  price_usd = Price / 380,
  price_eur = Price / 400
)]



library(data.table)
df <- fread('http://bit.ly/CEU-R-numbers-set')
str(df)

plot(df)

ggplot (df, aes(as.factor(x),y)) +
  geom_violin()

df[,as.list(summary(y)),by = x]
  

df2 <- read.csv('http://bit.ly/CEU-R-numbers')
plot(df2)

ggplot (df2, aes(x,y)) +
  geom_point(alpha = 0.005)

ggplot (df2, aes(x,y)) +
  geom_point(size = 0.1, alpha = .1)

ggplot (df2, aes(x,y)) +
  geom_hex(binwidth= 25)

dm <- dist(iris[, 1:4])
hc <- hclust(dm)


plot(hc)

rect.hclust(hc, k= 3)


##

for (i in 2:8) {
  plot(hc)
  rect.hclust(hc, k = i)
  Sys.sleep(0.5)
}

library(animation)
ani.options(interval = 1)
saveGIF({
  for (i in 2:8) {
    plot(hc)
    rect.hclust(hc, k = i)
  }
})

library(dendextend)
d <- as.dendrogram(hc)
d <- color_branches(d, k = 3)
ggplot(d)


saveGIF({
  for (i in 2:8) {
    d <- as.dendrogram(hc)
    d <- color_branches(d, k = 3)
    print(ggplot(d))
  }
})


cutree(hc,3)

iris$cluster <- cutree(hc,3)

library(NbClust)
NbClust(iris[,1:4], method = 'complete', index = 'all')

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = as.factor(cluster), shape =  as.factor(cluster))) +
  geom_point() +
  theme(legend.position = 'top')

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = as.factor(cluster))) +
  geom_point() +
  theme(legend.position = 'top') +
  facet_wrap(~Species)

library(gganimate)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = as.factor(cluster))) +
  geom_point() +
  theme(legend.position = 'top') +
  transition_states(Species) + 
  labs(title = '{closest_state}') # glue, f-string


library(datasauRus)
df <- datasaurus_dozen
str(datasaurus_dozen)

ggplot(df,aes(x,y, color = dataset)) +
  geom_point() +
  theme(legend.position = 'top') +
  transition_states(dataset) +
  labs(title = '{closest_state}', subtitle = '{stats(subset(df, dataset == closest_state))}' )

stats <- function(df) {paste0(
'mean of x = ',round(mean(df$x), 4),'\n',
'mean of y = ',round(mean(df$y), 4),'\n',
'std of x = ',round(sd(df$x), 4), '\n',
'std of y = ',round(sd(df$y), 4), '\n',
'corr of x and y = ',round(cor(df$x,df$y), 4)
)}

cat(stats(df))

ggplot(mtcars, aes(wt,qsec, color = factor(am))) + geom_point()
str(mtcars)

p <- last_plot()

library(ggthemes)

p + theme_excel_new() +scale_color_excel_new()
p + theme_tufte()

custome_theme <- function() {
  theme(
    text  = element_text(
      family = 'Comic Sans MS',
      color = 'green',
      size = 16
    )
  )
}

p + custome_theme()

library(ggthemr)

ggthemr('pale', layout = 'scientific', spacing = 2, type = 'inner')
p

ggthemr_reset()


#https://www.shanelynn.ie/themes-and-colours-for-r-ggplots-with-ggthemr/
#https://github.com/Mikata-Project/ggthemr

library(ggiraph) # interactivity
girafe(ggobj=p)


p <- ggplot(mtcars, 
            aes(wt,qsec, color = factor(am), 
                data_id = factor(gear),
                tooltip = rownames(mtcars))
            ) + 
  geom_point_interactive()


girafe(ggobj = p, options = list(opts_hover(css = 'fill: black:')))

## image

download.file('http://bit.ly/r-intro-nasa','image.jpg', mode = 'wb')
library(jpeg)
img <- readJPEG('image.jpg')
img[1,1,]
col <- do.call(rgb,as.list(img[1,1,]))
barplot(1, col = col)

str(img)
dim(img)
h <- dim(img)[1]
w <- dim(img)[2]

img1d <- matrix(img, h* w)

str(img1d)

## PCA 
pca <- prcomp(img1d)
str(pca)

str(pca$x[,1])

str(matrix(pca$x[,1],h))

image(matrix(pca$x[,1],h))
image(matrix(pca$x[, 1], h))


library(jpeg)
img <- readJPEG('image.jpg')
h <- dim(img)[1]
w <- dim(img)[2]
img1d <- matrix(img, h * w)
pca <- prcomp(img1d)
image(matrix(pca$x[, 1], h))