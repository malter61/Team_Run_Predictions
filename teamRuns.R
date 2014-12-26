batting <- Batting[which(Batting$yearID>=1955),]

for(i in 8:24){
  batting[,i]<- ifelse(is.na(batting[,i]),0,batting[,i])
}
#batting <- batting[complete.cases(batting),]
batting$X1B <- batting$H-batting$X2B-batting$X3B-batting$HR
#batting$lgID <- ifelse(batting$lgID=="NL",0,1)
#batting$lgID <- as.numeric(batting$lgID)

##  need to convert team names to numbers for the aggregate function
batting$teamID <- as.character(batting$teamID)
#batting$teamID <- match(batting$teamID,unique(batting$teamID))


### rc = Runs Created
rc <- batting[,c(2,4,8,9,11:23,25)]

rc.agg <- ddply(rc,c('yearID','teamID'),summarise,AB=sum(AB),R=sum(R),X2B=sum(X2B),X3B=sum(X3B),
                HR=sum(HR),RBI=sum(RBI),SB=sum(SB),CS=sum(CS),BB=sum(BB),SO=sum(SO),IBB=sum(IBB),
                HBP=sum(HBP),SH=sum(SH),SF=sum(SF),GIDP=sum(GIDP),X1B=sum(X1B))

#rc.agg <- aggregate(rc,by=list(rc$yearID,rc$teamID),FUN=sum,na.rm=TRUE)


#rc.agg <- rc.agg[,c(5,7:19,20,6)]


#rc.agg$pa <- rc.agg$AB+rc.agg$BB+rc.agg$HBP+rc.agg$SH+rc.agg$SF   ### pa = plate appearances
#rc.agg$per.pa <- 1/rc.agg$pa  

### convert all values to per plate appearance
#for(i in 1:16){
#  rc.agg[,i] <- rc.agg[,i]*rc.agg$per.pa
#}

#rc.agg <- rc.agg[,c(1:11,13:15)]


### split data into train and test
set.seed(123)
smp_size <- floor(0.70 * nrow(rc.agg))
train_ind <- sample(seq_len(nrow(rc.agg)), size = smp_size)
train <- rc.agg[train_ind, ]
test <- rc.agg[-train_ind,]
all <- rbind(test, train)




fit <- glm(R ~.,data=train[,-c(1,2,8)],family=gaussian())
summary(fit)
train$predict <- predict(fit,newdata=train,type='response')
test$predict <- predict(fit,newdata=test,type='response')
all$predict <- predict(fit,newdata=all,type='response')

test$H<- test$X1B+test$X2B+test$X3B+test$HR
test$TB <- test$X1B+2*test$X2B+3*test$X3B+4*test$HR
test$A <- test$H+test$BB-test$CS+test$HBP-test$GIDP
test$B <- 1.125*test$X1B+1.69*test$X2B+3.02*test$X3B+3.73*test$HR+
  0.29*(test$BB-test$IBB+test$HBP)+0.492*(test$SH+test$SF+test$SB)-0.04*test$SO
test$C <- test$AB+test$BB+test$HBP+test$SH+test$SF
test$RC <- ((2.4*test$C+test$A)*(3*test$C+test$B)/(9*test$C)) - 0.9*test$C
test$RCnew <- (test$H+test$BB-test$CS+test$HBP-test$GIDP)*(test$TB+(0.26*(test$BB-test$IBB+test$HBP))+(0.52*(test$SH+test$SF+test$SB)))/(test$AB+test$BB+test$HBP+test$SH+test$SF)
test$MMsqerr <- (test$predict-test$R)^2
test$wikisqerr <- (test$RC-test$R)^2
test$wiki.new.sqerr <- (test$RCnew-test$R)^2
test$predict <- as.numeric(format(round(test$predict,0),nsmall=0))
test$RC <- as.numeric(format(round(test$RC,0),nsmall=0))
test$RCnew <- as.numeric(format(round(test$RCnew,0),nsmall=0))
test$MMsqerr <- as.numeric(format(round(test$MMsqerr,2),nsmall=2))
test$wikisqerr <- as.numeric(format(round(test$wikisqerr,2),nsmall=2))
test$wiki.new.sqerr <- as.numeric(format(round(test$wiki.new.sqerr,2),nsmall=2))

batting <- batting[,c(1,2,4,8:23,25)]
batting$pa <- batting$AB+batting$BB+batting$HBP+batting$SH+batting$SF
batting <- batting[which(batting$pa>=200),]
batting$opb <- (batting$pa-batting$H-batting$BB-batting$HBP)/batting$pa
batting$per.pa <- 1/batting$pa

### convert all values to per plate appearance
for(i in 4:21){
  batting[,i] <- batting[,i]*batting$per.pa
  batting[,i] <- ifelse(is.na(batting[,i]),0,batting[,i])
}

### need to group players by year to account for mid-season trade
#batting <- ddply(batting,c('playerID','yearID'),summarise,AB=sum(AB),X2B=sum(X2B),X3B=sum(X3B),HR=sum(HR),SB=sum(SB),
#            CS=sum(CS),BB=sum(BB),SO=sum(SO),IBB=sum(IBB),HBP=sum(HBP),SH=sum(SH),GIDP=sum(GIDP),X1b=sum(X1B),R=sum(R))
batting$predict <- predict(fit,newdata=batting[,c(4,7:9,11:17,19,20)],type='response')
batting$predict.per.27 <- 27*batting$predict/batting$opb   ### predicted runs per 27 plate appearances

