library(shiny)
require(ggplot2)
require(dplyr)
library(data.table)
library(repmis)

#devtools::install_github('rstudio/rscrypt')
#addAuthorizedUser("lighthouse")



file <- 'player.stats.csv'
mykey <- 'xhb2891co2pafdg'
runs.df <- source_DropboxData(file,key=mykey, sep=",", header=TRUE)


print.money <- function(x, ...) {
  print.default(paste0("$", formatC(as.numeric(x), format="f", digits=0, big.mark=",")))
}

print(names(runs.df))

shinyServer(
  function(input,output){
      
    output$player.runs <- renderTable({
      stats <- runs.df[,c('name','yearID','RC','RC.per27','RCpy','norm.RC')]
      if(input$name %in% stats$name){stats <- stats[tolower(stats$name)==tolower(input$name),]
      }else{stats <- stats[stats$name=='Mike Trout',]}
      if(stats$name=='Frank Thomas'){stats <- stats[stats$yearID>1990,]}
      colnames(stats) <- c('name','year','RCperPA','RCper27outs','RunsCreated','RCnormalized')
      stats <- stats[,c('name','year','RunsCreated','RCperPA','RCper27outs','RCnormalized')]
      colnames(stats)[1] <- stats[1,1]
      stats[,1] <- ''
      return (stats)
      
    },digits=4)
    
    output$all.players <- renderDataTable({
      stats <- runs.df[,c('name','yearID','RC','RC.per27','POS','bats','yearID',
                          'age','yrs.exper','weight','height','PA','RCpy',
                          'salary','mean.RC','norm.RC','norm.sal','ops','birthCountry')]
      stats$bats <- as.character(stats$bats)
      stats$bats <- ifelse(stats$bats=='B','Switch',stats$bats)
      if(input$position!='All'){stats <- stats[stats$POS==input$position,]}
      if(input$bats!='All'){stats <- stats[stats$bats==input$bats,]}
      
      if(nchar(input$startYear)!=4 | !is.numeric(as.numeric(input$startYear))){start <- 2015
      }else{start <- as.numeric(input$startYear)}
      if(start>2015){start <- 2015}
      
      if(nchar(input$endYear)!=4 | !is.numeric(as.numeric(input$endYear)) | as.numeric(input$endYear<start)){end <- start
      }else{end <- as.numeric(input$endYear)}
      
      #if(nchar(input$endYear)!=4 | !is.numeric(input$endYear) | as.numeric(endYear)<start){end <- start)
      #}else{end <- as.numeric(input$endYear)}
      #if(end<start){end <- start}
      stats <- stats[stats$yearID>=start & stats$yearID<=end,]# & stats$yearID<=end,]
      #if(input$endYear==''){stats <- stats[stats$yearID=='start',]}
      #if(input$endYear!='' & as.numeric(input$endYear)<=start){stats <- stats[stats$yearID=='start',]}
      #if(input$endYear!='' & as.numeric(input$endYear)>start){
       #     stats <- stats[stats$yearID>=start & stats$yearID<=as.numeric(input$endYear),]}
      stats <- stats[stats$age>=input$age[1] & stats$age<=input$age[2],]
       #stats <- stats[stats$yrs.exper>=input$yrs.exp[1] & stats$yrs.exper<=input$yrs.exp[2],]
       stats <- stats[stats$weight>=input$weight[1] & stats$weight<=input$weight[2],]
       stats <- stats[stats$height>=input$height[1] & stats$height<=input$height[2],]
      if(input$country!='All'){stats <- stats[stats$birthCountry==input$country,]}
      stats <- stats[order(-stats$RC),]
      stats <- stats[!is.na(stats$name),]

      stats$norm.value <- stats$norm.RC/stats$norm.sal
      stats$salary <- print.money(stats$salary)
      stats <- stats[,c('name','yearID','PA','RCpy','RC','RC.per27','ops',
               'salary','mean.RC','norm.RC')]
      colnames(stats) <- c('player','year','PA','RunsCreated','RCperPA','RCper27outs','ops',
                           'salary','RCmean','RCnormalized')
      
      stats$RunsCreated <- round(stats$RunsCreated,1)
      stats$RCperPA <- round(stats$RCperPA,3)
      stats$RCper27 <- round(stats$RCper27,3)
      stats$ops <- round(stats$ops,3)
      stats$RCperPA <- round(stats$RCperPA,3)
      stats$RCmean<- round(stats$RCmean,2)
      stats$RCnormalized<- round(stats$RCnormalized,2)
      return (head(stats,100)) 
    })
    

    
    
#     output$chart2 <- renderPlot({
#       
#                             
#       }else                               {plot <- ggplot(stolen.bases,aes(x=time,y=scoring.prob))+
#                                              geom_bar(aes(fill=time),stat='identity')+facet_grid(.~state) +
#                                              scale_size_continuous(guide="none") +
#                                              theme(legend.position='none') +
#                                              ggtitle  ("Probability of scoring at least one run before and after stolen base attempts") + 
#                                              xlab("situation and steal type (see explanations below)") + 
#                                              ylab("probability of scoring at least one run remainder of inning")}
#       
#       
#       return(plot)
      
      
    #})
    
    
    
    
    
    
    
    
})
