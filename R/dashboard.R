library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dash)
library(plotly)
library(data.table)
library(lubridate)

int2time <- function (x) {

  h <- floor(x)
  m <- round((x-h)*60)
  m <- sprintf("%02d",m)

  return(paste0(h,":",m))
}

time2int <- function(x) {
  x <- as.numeric(unlist(strsplit(x,":")))
  return(x[1]+x[2]/60)
}

#app <- Dash$new()


setwd("D:/Git/timetrackR")

files <- list.files (getwd(),full.names = TRUE)
report <- files[grepl("report.*", files,ignore.case = TRUE)]
data <- fread(file=report,header = TRUE)
data <- data[,1:4]
colnames(data) <- c("activity","duration","from","to")

data[,from := parse_date_time(from, "mdY HS")]
data[,to := parse_date_time(to, "mdY HS")]

for(i in 1:nrow(data)) {

  from <- data[i,from]
  to <- data[i,to]

  if (date(from) != date(to)) {
    midnight.from <- parse_date_time(paste(date(from),"23:59:59"),"Ymd HMS")
    midnight.to <- parse_date_time(paste(date(to),"00:00:00"),"Ymd HMS")

    dur.from <- as.numeric(difftime(midnight.from,from,units="h"))
    dur.to <- as.numeric(difftime(to,midnight.to,units="h"))

    from.row <- to.row <- data[i,]
    from.row$duration <- int2time(dur.from)
    from.row$to <- midnight.from
    data[i,] <- from.row
    to.row$duration <- int2time(dur.to)
    to.row$from <- midnight.to
    data <- rbind(data,to.row)
  }
}

data[,date := date(from)]
data[,duration := sapply(duration,time2int)]

out <- data[, .(duration = sum(duration)), by = c('date','activity')]

outliers <- data[, .(duration = sum(duration)), by = c('date')]

p <- out %>%
  ggplot( aes(x=date, y=duration, fill=activity)) +
  geom_area(aes(group = activity))

# Turn it interactive
p <- ggplotly(p, tooltip="text")
p
