# for generating some arbitrary gcp for soil core photos
# to be processed with Agisoft Photoscan
#
# EDIT the following file directory and search pattern
fd <- "C:/Users/ybcheng/Documents/data2016/20160822 -- pictures/"
fp <- "Test*"
fn <- list.files(fd, pattern=fp)

y <- rep(1, length(fn))
z <- y
x <- seq(150, (150-(length(fn)-1)*5), by=-5)

df <- cbind(fn, x, y, z)
write.csv(df, paste0(fd, "gcp.csv"), row.names = FALSE, quote = FALSE)