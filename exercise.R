dat <- read.csv("Dataset_S1_sub.txt")
head(dat)
class(dat)

write.table(dat, file="table_write.txt")
dat <- read_excel("Exp_data.xls", sheet=1, skip = 0, col_names=T)

?airquality
ozone_complete1 <- airquality[!is.na(airquality$Ozone),]
ozone_complete2 <- subset(airquality, !is.na(Ozone))

airquality[!is.na(airquality$Ozone),]



library(UsingR)
head(babies)
## a simple way to checkout the data
plot(babies$gestation)  
babies$gestation[babies$gestation>900] <- NA
str(babies)

new_babies <- within(babies, {
  gestation[gestation==999] <- NA
  dwt[dwt==999] <- NA
})
str(new_babies)

str(babies$smoke)
new_babies <- within(babies, {
  gestation[gestation==999] <- NA
  dwt[dwt==999] <- NA
  smoke = factor(smoke)
  levels(smoke) = list(
    "never" = 0, 
    "smoke now" = 1, 
    "until current pregnancy" = 2,
    "once did, not now" = 3)
})
str(new_babies$smoke)

fit <- lm(gestation~smoke, new_babies)
summary(fit) ## t-test 결과 
anova(fit)


newdf <- subset(new_babies, (smoke=="smoke now" | smoke == "never") & age < 25, select=c(id, gestation, age, wt, smoke))
ggplot(newdf, aes(x=wt, y=gestation, color=smoke)) +
  geom_point(size=3, alpha=0.5) +
  facet_grid(.~smoke) +
  theme_bw()






## =============================

library(UsingR)
head(babies)
df <- subset(babies, select=c(gestation, wt, dwt))
colMeans(df, na.rm=T)
apply(df, 2, mean, na.rm=T)




apply(df, 2, sd, na.rm=T)
apply(df, 2, function(x){ 
  xmean <- mean(x, na.rm=T) 
  return(xmean)
})



x <- list(a=1:10, b=exp(-3:3), logic=c(T,T,F,T))
mean(x$a)
lapply(x, mean)
sapply(x, mean)

x <- data.frame(a=1:10, b=exp(-4:5))
sapply(x, mean)

x <- c(4, 9, 16)
sapply(x, sqrt)
sqrt(x)

y <- c(1:10)
sapply(y, function(x){2*x})
y*2

airquality
g <- factor(airquality$Month)
airq_split <- split(airquality, g)

df <- airq_split[[1]]

ozone_func <- function(x){
  z <- mean(x$Ozone, na.rm=T)
  return(z)
}

temp_func <- function(x){
  z <- mean(x$Temp, na.rm=T)
  return(z)
}

ozone_means <- lapply(airq_split, ozone_func)
temp_means <- lapply(airq_split, temp_func)
unlist(ozone_means)
air_means <- data.frame(unlist(ozone_means), unlist(temp_means))

x <- c(1:100)
y <- x*2 + rnorm(100)
myxy <- data.frame(x,y)
plot(myxy)
plot(myxy$x, myxy$y)
plot(x=myxy$x, y=myxy$y)
plot(y~x, data=myxy)

z <- sample(1:100, 100, replace =T)
points(x, z)
points(x, z, col="red")

x <- rnorm(100)
hist(x, br=20, xlim=c(-3,3), main="Main text", xlab="X label")
hist(x)

hist(airquality$Wind, br=50)
hist(airquality$Wind, br=10)

x <- rnorm(100)
boxplot(x)

r <- boxplot(airquality$Wind)

airquality$Wind[which(airquality$Wind > (1.5*(r$stats[4]-r$stats[2])+r$stats[4]))]

with(airquality, {
  Wind[which(Wind > (1.5*(r$stats[4]-r$stats[2])+r$stats[4]))]
})

with(airquality, {
  val <- (1.5*(r$stats[4]-r$stats[2])+r$stats[4])
  Wind[which(Wind > val)]
})

with(airquality, {
  iqr <- quantile(Wind, 3/4) - quantile(Wind, 1/4)
  val <- 1.5 * iqr + quantile(Wind, 3/4)
  Wind[which(Wind > val)]
})

y <- rnorm(100, 1, 1)
#boxplot(y)
xy <- data.frame(x, y)
boxplot(xy)
class(xy)

##
mean_vals <- sample(10)
mymat <- sapply(mean_vals, function(x){rnorm(100, x)})
dim(mymat)
boxplot(mymat)


x <- sample(1:12, 200, replace = T)
tab_x <- table(x)
y <- sample(1:12, 200, replace = T)
tab_y <- table(y)
tab_xy <- rbind(tab_x, tab_y)
barplot(tab_xy)
barplot(tab_xy, beside = T)
barplot(tab_xy, beside = T, col=c("darkblue","red"))
barplot(tab_xy, beside = T, col=c("darkblue","red"), xlab="Month")
barplot(tab_xy, beside = T, col=c("darkblue","red"), xlab="Month", horiz=TRUE)




plot(iris$Sepal.Length, iris$Sepal.Width)

subdat <- subset(iris, Species=="setosa", select=c(Sepal.Length, Sepal.Width))
plot(subdat, col="red", pch=19)

hist(iris$Sepal.Length)
hist(iris$Petal.Length)

boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Width)
mydf <- data.frame(iris$Sepal.Width, iris$Petal.Width)
boxplot(mydf)
mydf <- subset(iris, select=c(Sepal.Width, Petal.Width))
boxplot(mydf)

with(iris, {
  boxplot(data.frame(Sepal.Width, Petal.Width))
})


mydf <- data.frame(subset(iris, Species=="setosa", select=Sepal.Length),
                   subset(iris, Species=="versicolor", select=Sepal.Length),
                   subset(iris, Species=="virginica", select=Sepal.Length))
boxplot(mydf)


iris_list <- split(iris, iris$Species)
mydf <- data.frame(iris_list$setosa$Sepal.Length, 
                   iris_list$versicolor$Sepal.Length, 
                   iris_list$virginica$Sepal.Length)
boxplot(mydf)

# 3. formula 
boxplot(Sepal.Length~Species, data=iris)

