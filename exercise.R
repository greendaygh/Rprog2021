dat <- read.csv("Dataset_S1_sub.txt")
head(dat)
class(dat)

write.table(dat, file="table_write.txt")
dat <- read_excel("Exp_data.xls", sheet=1, skip = 0, col_names=T)

?airquality
ozone_complete1 <- airquality[!is.na(airquality$Ozone),]
ozone_complete2 <- subset(airquality, !is.na(Ozone))

airquality[!is.na(airquality$Ozone),]
