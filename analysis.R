library(data.table)
library(ggplot2)
library(httr)
library(XML)
library(rvest)

rm(list=ls())

##########################################
# Theme ##################################
##########################################

t <- theme(plot.title = element_text(face="bold", margin=margin(t = 15, r = 0, b = 15, l = 0, unit = "pt")),
           axis.text.x = element_text(size=10,color='#000000',angle=45,hjust=1),
           axis.text.y = element_text(size=10,color='#000000'),
           axis.title.x = element_text(face="bold", size=10,color='#000000',margin=margin(t = 10, r = 0, b = 0, l = 0, unit = "pt")),
           axis.title.y = element_text(face="bold", size=10,color='#000000',margin=margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")),
           panel.background = element_rect(fill='#ffffff', color='#a5a5a5',size=0.5),
           panel.ontop = F,
           panel.grid.major = element_line(color='#a5a5a5', linetype='dashed',size=0.2),
           panel.grid.minor = element_line(color='#a5a5a5', linetype='dashed', size=0),
           legend.text = element_text(size=10,color='#000000'),
           legend.title = element_text(face='bold',size=10,color='#000000'),
           legend.box.background = element_rect(fill='#ffffff', color='#ffffff', size=1.5),
           strip.text = element_text(size=10,color='#000000', face='bold'),
           strip.background = element_rect(colour = NA, fill = '#ffffff'))

pal <- c('#E02128','#ff0072','#1B2C3F','#3e21aa','#2672A4','#43A39E','#EB9E0F','#333745','#8f8f8f','#515151','#000000')

names <- fread('names.csv',stringsAsFactors=F)
names <- names[order(year, region, gender, count)]
names <- names[region=='Flanders']

hits <- fread('hits.csv', stringsAsFactors=F)
pseudo <- fread('pseudo.csv', stringsAsFactors=F)

hits <- merge(x=hits,y=pseudo,by='artist_name',all.y=T)
rm(pseudo)

britney <- names[gender == 'female' & year == 2000]
britney <- britney[order(-count)]

susan <- names[gender == 'female' & name %in% c('Susan','Suzanne','Suzanna')]

hits <- hits[,.(year=min(year)), by=.(artist_name,name1)]
names <- merge(names,hits,by.x='name',by.y='name1',all.y=T)
setnames(names,c('year.x','year.y'),c('year','start_year'))
names <- names[,relative_year := year - start_year]
names <- names[,label := paste0(name,' (',artist_name,')')]

big_selection <- c('Aaliyah','Alana Dante','Alicia Keys','Anouk','Belle Perez',
               'Brahim','Britney Spears','Emilia','Spice Girls','Jennifer Lopez',
               'Jessica Simpson','Kylie Minogue','Leona Lewis','Lily Allen','Lady Linn And Her Magnificent Seven',
               'Natalia','Paris Hilton','Rihanna','Ronan Keating','Shakira','Shania Twain','Tonya')

selection <- c('Aaliyah','Alana Dante','Belle Perez','Anouk','Emilia','Lily Allen','Paris Hilton','Rihanna','Shakira','Shania Twain','Ronan Keating','Britney Spears')

names <- names[artist_name %in% selection]

g <- ggplot(names[name=='Britney'],aes(x=as.character(year),y=count)) + 
    geom_bar(stat='identity', fill='#ff0072') + 
    t + 
    xlab('year') +
    ylab('Britneys born') +
    ggtitle('Plot 1: Babies given the name Britney')
g
ggsave('britney.png',g,width=48.8,height=27.4, units='cm')

g1 <- ggplot(names[!(name %in% c('Ronan','Britney'))],aes(x=relative_year,y=count,fill=label, label=year)) + 
    geom_bar(stat='identity') + 
    geom_text(size=3,angle=90,nudge_y=5) + 
    geom_vline(xintercept=0, size=1.5) + 
    t + 
    scale_fill_manual(values=pal,name='name') + 
    facet_wrap(~label,ncol=5) +
    xlim(-10,20) +
    xlab('relative year') +
    ylab('babies born with given name') +
    ggtitle('Plot 2: Names given for a selection of artists') +
    theme(legend.position="none")
g1

ggsave('artists.png',g1,width=48.8,height=27.4, units='cm')

g2 <- ggplot(names[name=='Ronan'],aes(x=as.character(year),y=count)) + 
    geom_bar(stat='identity', fill='#1B2C3F') + 
    t + 
    xlab('year') +
    ylab('Ronans born') +
    ggtitle('Plot 3: Babies given the name Ronan')
g2
ggsave('ronan.png',g2,width=48.8,height=27.4, units='cm')
