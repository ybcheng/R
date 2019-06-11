rm(list=ls())

library(smooth)
library(zoo)
#library(ggplot2)

fd <- "Z:/Vapor55/20180726_Imp_Dam_BDF/05_TerraSolid/15_analysis"
if (file.exists(fd) != TRUE){
  stop("File folder does not exist")
}
setwd(fd)

bdf_filename <- "20180724_BDF_1_tscan_clean_bay3_v2.txt"
#bdf_filename <- "20180724_BDF_1_tscan_clean_intake1_v2.txt"
#bdf_filename <- "20180726_BDF_1_tscan_clean_intake4_v2.txt"
if (file.exists(bdf_filename) != TRUE){
  stop("File folder does not exist")
}

ws_cls <- 9
grnd_cls <- 2

new_filename <- gsub(".txt", "_proc.csv", bdf_filename)

bdf_data_raw <- as.data.frame(read.csv(bdf_filename, header=FALSE, sep=""))
colnames(bdf_data_raw) <- c("class", "E", "N", "Z")
bdf_data <- subset(bdf_data_raw, class==ws_cls | class==grnd_cls)
#bdf_data$sum <- bdf_data$E + bdf_data$N
min_E <- min(bdf_data$E)
min_N <- min(bdf_data$N)
print(paste0("min E : ", min_E))
print(paste0("min N : ", min_N))
bdf_data$dist <- sqrt((bdf_data$E - min_E)^2 + (bdf_data$N - min_N)^2)
head(bdf_data)


new_data <- data.frame()
new_names <- c("dist", "avg_ws", "avg_grnd")
#colnames(new_data) <- new_names

step <- 0.5
min_d <- floor(min(bdf_data$dist))
max_d <- ceiling(max(bdf_data$dist))

for (i in seq(from=min_d, to=max_d, by=step)){
  d <- mean(i,i+step)
  ws <- subset(bdf_data, class==ws_cls & dist>=i & dist<i+step)
  ws_z <- mean(ws$Z)
  rb <- subset(bdf_data, class==grnd_cls & dist>=i & dist<i+step)
  rb_z <- min(rb$Z)
  tmp <- cbind.data.frame(d, ws_z, rb_z)
  colnames(tmp) <- new_names
  new_data <- rbind(new_data, tmp)
}

new_data$avg_grnd[is.nan(new_data$avg_grnd)] <- NA
new_data$avg_grnd[is.infinite(new_data$avg_grnd)] <- NA
new_data$avg_ws[is.nan(new_data$avg_ws)] <- NA
new_data$avg_ws[is.infinite(new_data$avg_ws)] <- NA

smoo_ws <- with(new_data[!is.na(new_data$avg_ws),],smooth.spline(dist,avg_ws))
result_ws <- with(new_data, predict(smoo_ws,dist[is.na(avg_ws)]))
new_data$avg_ws[is.na(new_data$avg_ws)] <- result_ws$y
result_ws2 <- with(new_data, predict(smoo_ws,dist))
new_data$fit_ws <- result_ws2$y
#print(mean(new_data$fit_ws))

smoo_grnd <- with(new_data[!is.na(new_data$avg_grnd),],smooth.spline(dist,avg_grnd))
result_grnd <- with(new_data, predict(smoo_grnd, dist[is.na(avg_grnd)]))
new_data$avg_grnd[is.na(new_data$avg_grnd)] <- result_grnd$y
result_grnd2 <- with(new_data, predict(smoo_grnd,dist))
new_data$fit_grnd <- result_grnd2$y
new_data$sma_grnd <- rollapply(new_data$fit_grnd, width=50, function(...) {round(mean(...), digits = 3)}, partial = TRUE)

#plot(new_data$dist, new_data$fit_grnd, "l")
plot(new_data$dist, new_data$sma_grnd, "l", ylim=c(160,185), lwd=2,
     xlab="Distance (US Survey Ft)", ylab="Height (US Survey Ft)")
abline(h=mean(new_data$fit_ws),lwd=1.5, lty=2)
grid(nx=100, ny=25)

write.csv(new_data, new_filename, row.names=FALSE)

#dev.copy(png, gsub("txt","png",basename(bdf_filename)))
#dev.off()
