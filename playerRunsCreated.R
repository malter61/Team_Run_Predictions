# glm model for team and individual runs created.  
#install.packages("data.table")
#install.packages("Lahman")
#install.packages("dplyr")
#install.packages("ggplot2")
library(Lahman)
library(data.table)
library(dplyr)
library(ggplot2)


start <- Sys.time()

## Batting is from the Lahman library.  We don't want to alter that data frame, 
##so I make a copy and call it "batting".
batting <- Batting
batting[is.na(batting)] <- 0
batting <- batting %>% mutate(X1B=H-X2B-X3B-HR,TB=X1B+2*X2B+3*X3B+4*HR,
            PA=AB+BB+HBP+SH+SF)
batting$teamID <- as.character(batting$teamID)

### We're going to build a glm model using the team stats, and then apply the 
### model cofficients to the player stats.
team.stats <- batting %>% group_by(teamID,yearID) %>% 
           mutate(AB=sum(AB)/sum(PA),R=sum(R)/sum(PA),H=sum(H)/sum(PA),
           X1B=sum(X1B)/sum(PA),X2B=sum(X2B)/sum(PA),X3B=sum(X3B)/sum(PA),
           HR=sum(HR)/sum(PA),TB=sum(TB)/sum(PA),RBI=sum(RBI)/sum(PA),
           SB=sum(SB)/sum(PA),CS=sum(CS)/sum(PA),BB=sum(BB)/sum(PA),
           SO=sum(SO)/sum(PA),IBB=sum(IBB)/sum(PA),HBP=sum(HBP)/sum(PA),
           SH=sum(SH)/sum(PA),SF=sum(SF)/sum(PA),GIDP=sum(GIDP)/sum(PA),
           oppa=1-H-BB-HBP) %>% 
           distinct(yearID,teamID) %>% ungroup()

player.stats <- batting %>% group_by(playerID,yearID) %>% 
           mutate(obp=(H+BB+HBP)/(AB+BB+HBP+SF),slg=(X1B+2*X2B+3*X3B+4*HR)/AB,
           ops=obp+slg,AB=sum(AB)/sum(PA),R=sum(R)/sum(PA),H=sum(H)/sum(PA),
           X1B=sum(X1B)/sum(PA),X2B=sum(X2B)/sum(PA),X3B=sum(X3B)/sum(PA),
           HR=sum(HR)/sum(PA),TB=sum(TB)/sum(PA),RBI=sum(RBI)/sum(PA),
           SB=sum(SB)/sum(PA),CS=sum(CS)/sum(PA),BB=sum(BB)/sum(PA),
           SO=sum(SO)/sum(PA),IBB=sum(IBB)/sum(PA),HBP=sum(HBP)/sum(PA),
           SH=sum(SH)/sum(PA),SF=sum(SF)/sum(PA),GIDP=sum(GIDP)/sum(PA),
           oppa=1-H-BB-HBP) %>% 
           filter(PA>=250) %>%
           distinct(yearID,playerID) %>% ungroup()

### oppa is outs per at bat.  This value is used for individual RC calculations 
###per 27 outs. The number of plate appearances per game is 27/oppa.


## Ideally, we should make more than two era splits.
team.stats.modern.era <- team.stats %>% filter(yearID >= 1954)
team.stats.old.era <- team.stats %>% filter(yearID < 1954)

player.stats.modern.era <- player.stats %>% filter(yearID >= 1954)
player.stats.old.era <- player.stats %>% filter(yearID < 1954)

### split team.stats data into train and test for both old and modern eras
set.seed(123)
smp_size <- floor(0.70 * nrow(team.stats.modern.era))
train_ind <- sample(seq_len(nrow(team.stats.modern.era)), size = smp_size)
train.modern.era <- team.stats.modern.era[train_ind, ]
test.modern.era <- team.stats.modern.era[-train_ind,]

smp_size <- floor(0.70 * nrow(team.stats.old.era))
train_ind <- sample(seq_len(nrow(team.stats.old.era)), size = smp_size)
train.old.era <- team.stats.old.era[train_ind, ]
test.old.era <- team.stats.old.era[-train_ind,]

### MODERN ERA
fit.modern.era <- glm(R ~ 0+X1B+X2B+X3B+HR+SB+CS+BB+SO+HBP+SH+SF+GIDP,
                      data=train.modern.era,family=gaussian())

summary(fit.modern.era)
### RC = runs created (predicted) by the glm model
train.modern.era$RC <- predict(fit.modern.era,newdata=train.modern.era,
                                  type='response')
test.modern.era$RC <- predict(fit.modern.era,newdata=test.modern.era,
                                 type='response')

### OLD ERA
### There were no stats for IBB or SF in old era
fit.old.era <- glm(R ~ 0+X1B+X2B+X3B+HR+SB+CS+BB+SO+HBP+SH+GIDP,
                      data=train.old.era,family=gaussian())
summary(fit.old.era)
train.old.era$RC <- predict(fit.old.era,newdata=train.old.era,
                               type='response')
test.old.era$RC <- predict(fit.old.era,newdata=test.old.era,
                              type='response')

test.all <- rbind(test.old.era,test.modern.era)



### now let's add in Bill James predictions from his formula
test.all <- test.all %>% mutate(jamesRC=(H+BB-CS+HBP-GIDP)*
              (TB+(0.26*(BB-IBB+HBP))+(0.52*(SH+SF+SB)))/(AB+BB+HBP+SH+SF))

### Add the squared error
test.all <- test.all %>% mutate(RC.SE=(RC-R)^2,
                          jamesRC.SE=(jamesRC-R)^2)


test.modern.era <- test.all %>% filter(yearID>=1954)
head(test.modern.era)
sum(test.modern.era$RC.SE)
sum(test.modern.era$jamesRC.SE)


### now we run the predict function with the model parameters on the player data
player.stats.modern.era$RC <- predict(fit.modern.era,
                                newdata=player.stats.modern.era,type='response')
player.stats.old.era$RC <- predict(fit.old.era,newdata=player.stats.old.era,
                                type='response')
player.stats <- rbind(player.stats.old.era,player.stats.modern.era)

player.stats <- player.stats %>% mutate(jamesRC=(H+BB-CS+HBP-GIDP)*
                  (TB+(0.26*(BB-IBB+HBP))+(0.52*(SH+SF+SB)))/(AB+BB+HBP+SH+SF))

### predicted runs per 27 plate outs and per year
player.stats <- player.stats %>% mutate(RC.per27=27*RC/oppa,RC.per502=502*RC,
                              jamesRC.per27=27*jamesRC/oppa,RCpy=RC*PA,
                              jamesRCpy=jamesRC*PA)


### Now we merge with salaries and master to pick up biographical info on each player.
player.stats <- left_join(player.stats, Salaries,by=c('playerID','yearID'))
player.stats <- left_join(player.stats,Master,by='playerID',all.x=TRUE)

### The Master file has this player's birth year as 1961.  It should be 1861.
player.stats$birthYear[player.stats$playerID=='johnsbi01'] <- 1861

player.stats <- player.stats %>% mutate(name=paste(nameFirst,nameLast),
                                        cost=round(salary/RC))

player.stats <- player.stats %>% mutate(age=yearID-birthYear,
                              yrs.exper=1+yearID-as.numeric(substr(debut,1,4)))


### now we normalize rc and salary info by year
player.stats <- player.stats %>% group_by(yearID) %>% 
                                 mutate(mean.salary=mean(salary,na.rm=T),
                                 sd.salary=sd(salary,na.rm=T),
                                 mean.RC=mean(RCpy),sd.RC=sd(RCpy))

player.stats <- player.stats %>% mutate(norm.sal=(salary-mean.salary)/sd.salary,
                                        norm.RC=(RCpy-mean.RC)/sd.RC)


fielding <- data.table(Fielding)
fielding <- fielding[,.(G=sum(G)),by=.(playerID,POS,yearID)]## take care of mid-season trades
fielding <- fielding[,r:=rank(-G,ties.method='first'), ##Use the most frequent position
                        by=.(playerID,yearID)][r==1]
fielding <- fielding %>% mutate(POS=ifelse(POS %in% c('LF','CF','RF'),'OF',POS))

player.stats <- merge(player.stats,fielding[,c('playerID','POS','yearID'),with=F],
                      by=c('playerID','yearID'),all.x=T)
player.stats$birthCountry <- ifelse(!player.stats$birthCountry %in% 
                              c('Cuba','D.R.','P.R.','USA','Venezuela'),
                              'other',player.stats$birthCountry)

## we use the lag function to get prior year's performance to see how aging affects
## performance.
player.stats.list <- split(player.stats,player.stats$playerID)
player.stats.list <- lapply(player.stats.list,function(x){
  x <- x[order(x$yearID),]
  x <- x %>% mutate(priorRC=lag(RC),priorHR=lag(HR),priorOBP=lag(obp),
                    priorSLG=lag(slg),priorOPS=lag(ops),priorSB=lag(SB))
})
player.stats <- do.call('rbind',player.stats.list)
player.stats.list <- NULL
Sys.time()-start

colnames(player.stats) <- gsub('\\.x','',colnames(player.stats))
player.stats <- player.stats[,c('playerID','name','yearID','teamID','lgID',
                      'POS','G','PA','oppa','AB','R','H','X1B','X2B','X3B','HR',
                      'RBI','SB','CS','BB','SO','IBB','HBP','SH','SF','GIDP',
                      'TB','RC','jamesRC','RC.per502','RC.per27','jamesRC.per27',
                      'RCpy','jamesRCpy','salary','birthCountry','weight',
                      'height','bats','throws','cost','age','yrs.exper','mean.salary',
                      'sd.salary','mean.RC','sd.RC','norm.sal','norm.RC',
                      'priorRC','priorHR','priorOBP','priorSLG','priorOPS','priorSB',
                      'obp','slg','ops')]

yrs.diff <- player.stats[player.stats$yrs.exper!=1 & 
                           !is.na(player.stats$priorRC) &
                           !is.na(player.stats$priorHR) &
                           !is.na(player.stats$priorSB) &
                           !is.na(player.stats$priorOPS),]
yrs.diff <- yrs.diff %>% mutate(RCdiff=100*(RC-priorRC)/priorRC,
                        HRdiff=HR-priorHR,
                        SBdiff=SB-priorSB,
                        obpdiff=100*(obp-priorOBP)/priorOBP,
                        slgdiff=100*(slg-priorSLG)/priorSLG,
                        opsdiff=100*(ops-priorOPS)/priorOPS,
                        age=age,yrs.exper=yrs.exper)
write.csv(player.stats,'C:/baseball/player.stats.csv',row.names=F)
player.stats$bats <- factor(player.stats$bats)


ggplot(player.stats[!is.na(player.stats$height),],aes(height%/%3*3,RC,fill=bats)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge")+ 
  xlab('height in inches') + ylab('runs created') + ggtitle('runs created per plate appearance by height')

ggplot(player.stats[complete.cases(player.stats),],aes(birthCountry,RC.per27,fill=bats)) +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge",width=0.5)+ 
  xlab('birth country') + ylab('runs per 27 outs') + ggtitle('runs per 27 outs by country')


weight.plot <- player.stats %>% mutate(weight.bucket=weight%/%5*5,bats=bats) %>% 
                group_by(weight.bucket,bats) %>% summarize(meanRC=mean(RC)) %>% 
                ungroup()
weight.plot <- weight.plot[!is.na(weight.plot$bats),]

height.plot <- player.stats %>% mutate(height.bucket=height%/%2*2,bats=bats) %>% 
  group_by(height.bucket,bats) %>% summarize(meanRC=mean(RC)) %>% 
  ungroup()
height.plot <- height.plot[!is.na(height.plot$bats),]

age.rc.plot <- yrs.diff %>% group_by(age,bats) %>% 
  summarise(rcdiff=mean(RCdiff)) %>% filter(!is.na(bats)) %>% ungroup()

age.hr.plot <- yrs.diff %>% group_by(age,bats) %>% 
  summarise(HRdiff=mean(HRdiff)) %>% filter(!is.na(bats)) %>% ungroup()

ggplot(data=age.hr.plot[age.hr.plot$age>=22 & age.hr.plot$age<=42,],
       aes(x=age, y=HRdiff*500, colour=bats)) +
  geom_line() + ylab('Home runs per 500 plate appearances')+
  ggtitle('Runs per 27 outs vs. age')

ggplot(data=age.rc.plot[age.rc.plot$age>=22 & age.rc.plot$age<=42,],
       aes(x=age, y=rcdiff, colour=bats)) +
  geom_line()+ylab('year over year runc created change')+
  ggtitle('Change in runs created by age')

ggplot(data=weight.plot,
       aes(x=weight.bucket, y=meanRC,col=bats)) +
  geom_line()+xlab('weight')+ylab('runc sreated per plate appearance')+
  ggtitle('Runs created vs. weight')

ggplot(data=height.plot,
       aes(x=height.bucket, y=meanRC,col=bats)) +
  geom_line()

