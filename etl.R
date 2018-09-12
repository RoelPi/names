library(data.table)
library(ggplot2)
library(httr)
library(XML)
library(rvest)

#########################################################################
# NAME DATA #############################################################
#########################################################################

years <- as.character(seq(1995, 2017))

j <- list() # list for boys
m <- list() # list for girls

# Read all years from xls files
# If I had known this would take so long to read XLS'es I would have unioned them manually #fml

for (i in years) {
    j[[i]] <- read.xlsx(file='jongens.xls', sheetName=i, encoding="UTF-8", stringsAsFactors=F)
    m[[i]] <- read.xlsx(file='meisjes.xls', sheetName=i, encoding="UTF-8", stringsAsFactors=F)
}

# Add year to each record so I can rbind all list elements in one big data table
for (i in 1:length(years)) {
    year <- rep(as.numeric(years[i]),nrow(j[[i]]))
    j[[i]] <- cbind(j[[i]],year)
    year <- rep(as.numeric(years[i]),nrow(m[[i]]))
    m[[i]] <- cbind(m[[i]],year)
    print(i)
}

m[[18]][13] <- NULL
j_full <- rbindlist(j)
m_full <- rbindlist(m)

write.csv(m_full,'meisjes.csv',row.names=F)
write.csv(j_full,'jongens.csv', row.names=F)

m_full <- read.csv('meisjes.csv', stringsAsFactors=F)
j_full <- read.csv('jongens.csv', stringsAsFactors=F)

tidyNames <- function(dt,gender) {
    dt_bx <- dt[,4:6]
    dt_bx[,1] <- rep('Brussels',nrow(dt))
    dt_vl <- dt[,7:9]
    dt_vl[,1] <- rep('Flanders',nrow(dt))
    dt_wa <- dt[,10:12]
    dt_wa[,1] <- rep('Wallonie',nrow(dt))
    
    dt_bx <- cbind(dt[,13],dt_bx)
    dt_vl <- cbind(dt[,13],dt_vl)
    dt_wa <- cbind(dt[,13],dt_wa)
    dt <- rbindlist(list(dt_bx,dt_vl,dt_wa))
    dt <- dt[complete.cases(dt),]
    dt <- cbind(rep(gender,nrow(dt)),dt)
    colnames(dt) <- c('gender','year','region','name','count')
    dt
}

# Make names 
j_full <- tidyNames(j_full,'male')
m_full <- tidyNames(m_full,'female')

names <- rbind(j_full,m_full)
write.csv(names, 'names.csv', row.names=F)
rm(h,j,m,m_full,j_full)

#########################################################################
# MUSIC DATA ############################################################
#########################################################################

scrapeYear <- function(year) {
    link <- paste0('https://www.ultratop.be/nl/annual.asp?year=',year)
    page <- read_html(link)
    artist <- page %>% html_nodes('.CR_artist') %>% html_text()
    weeks <- page %>% html_nodes('.CR_weeks') %>% html_text()
    year <- data.table(artist_name=artist, num_weeks=weeks)
}

scrapeYears <- function(range) {
    dt <- data.table(year=numeric(), artist_name=character(), weeks=numeric())
    for (year in range) {
        dty <- scrapeYear(year)
        dty <- cbind(rep(year,100),dty)
        colnames(dty) <- c('year', 'artist_name', 'weeks')
        dt <- rbind(dt,dty)
    }
    dt
}
hits <- scrapeYears(1995:2017)
write.csv(hits,'hits.csv',row.names=F)
