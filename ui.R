library(shiny)


shinyUI(pageWithSidebar(
  
  headerPanel(h5("Baseball Runs Created Analysis")),
  
    sidebarPanel(      
      br(),
      textInput("name","Player name:",value="Mike Trout"),
      br(),

      selectInput("position","Position:",
                  c("All","C","1B","2B","3B","SS","OF"),selectize=F,multiple=F),
      br(),

      selectInput("bats","Bats:",c("All","R","L","Switch"),selectize=F,multiple=F),

      br(),
      
      textInput("startYear","Start year (2016 stats coming soon):",value="2015"),          
      br(),
      
      textInput("endYear","End year (must be blank or greater than start year:",value="2015"),          
      br(),

      
      sliderInput("age","Age:",
                  min=20,max=44,format="##",
                  value=c(20,44),step=1),

      sliderInput("weight","Weight:",
                  min=150,max=300,format="###",
                  value=c(150,310),step=10),          
      br(),
      
      sliderInput("height","Height (inches):",
                  min=64,max=84,format="##",
                  value=c(64,84),step=2),          
      br(),
      
      selectInput("country","country:",c('All','USA','D.R.','P.R.','Venezuela','Cuba'),
                                         ,selectize=F,multiple=F),
      
      br()
      
         
    ),
     
    mainPanel(
      tabsetPanel(type="tab",
          tabPanel("Introduction",HTML("<div><br></br>Please allow about thirty 
            seconds for the data to download before you move to another tab. 
            <br></br>After reading Bill James' Runs Created formula,
            <br>
              RC=((H+BB-CS+HBP-GIDP)*(TB+(0.26*(BB-IBB+HBP))+(0.52*(SH+SF+SB))))/
                (AB+BB+HBP+SH+SF)
            <br></br></br>
            I decided to create one of my own.  I built a model using a glm, and came 
            up with <br>
            RC=0.313*X1B+0.611*X2B+0.883*X3B+1.421*HR+0.199*SB-0.359*CS+0.305*BB-0.117*SO
              +0.426*HBP-0.276*SH+0.668*SF-0.598*GIDP
            <br></br></br>
            I trained the model on team statistics since we know exactly how many runs 
            each team scored in every season.  I then applied the model to every player
            for all seasons in which he had at least 250 plate appearances.
            <br></br>
            The 'Runs Created' tab allows the user to type in the name of any player 
            in the 'Player name' box, and a table appears with the player's runs created values
            for all seasons in which he had at least 250 plate appearances.
            <br></br>
            The 'Top Players' tab allows the viewing of all of the top players based on various
            user selected filters.
            <br></br>
                          
            Please play around with the app and enjoy.  Feel free to provide feedback to malter61@gmail.com <br></br>
            Mark
            </div>")),
                                   
                  tabPanel("Runs Created",HTML("<div> </div>"),
                           HTML
                           ("<div><br></br></div>"),

                           tableOutput("player.runs"),
                           HTML
                           ("<div><br></br>
                            <b>Type any player in the 'Player name' box at the top left.</b><br></br>
                           'Runs Created', or RC,  is the number of runs the player created over an entire season, based on his
                            RC per plate appearance and his total number of plate appearances on the season.
                            RCperPA is the RC per plate appearance.
                            <br></br>
                            RCper27 is a more fun stat to look at.  This is the number of runs the player would 
                            create if he were allowed to bat for all 27 of his team's outs.
                            Note that players are rewarded not only for their stats, but that a player with a 
                            high on base percentage would have more plate appearances per 27 outs.
                            While the mean number of plate appearances over all of the qualifying player/seasons is 40.5, 
                            Barry Bonds would have batted 69 times per game in 2004, and would have created 19.2 runs per game.
                            <br></br>
                            RCper27 is defined as 27*RCperPA/oppa, where oppa equals outs per plate appearance.
                            <br></br>
                            The RCnormalized column adjusts for yearly overall offensive changes by normalizing
                            the RC for each player as the number of standard deviations above and below the league mean for that season.
                            </div>")),
                  
                  tabPanel("Top Players",HTML("<div> 
                            <br></br>
                            Here we can view the top players by runs created.  We have added in the salary and mean 
                            runs created by all qualifying players (250 plate appearances) in that season for comparison's sake.
                            <br></br>
                            Play around with the filters at left to see who the best players are by position, bats, year (or year range), 
                            age, weight, height, or country of birth.
                            <br></br>
                                            </div>"),
                           dataTableOutput("all.players"),
                           HTML
                           ("<div><br></br>
                            <br></br>
                            
                           </div>"))

                  
                  
      )
    )
                
  ))


