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


