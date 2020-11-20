library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(knitr)
library(readr)

# Data Preparation Steps

characters = read_csv("data/characters.csv")
episodes = read_csv("data/episodes.csv")
scenes = read_csv("data/scenes.csv")
appearances = read_csv("data/appearances.csv")




ui <- fluidPage(
                navbarPage(
                  "Game of thrones",
                  tabPanel("Temps de présence par épisode",
                           sidebarPanel(
                             selectInput(inputId="characters",label="Choose Characters",choices = c("Jon Snow", "Daenerys Targaryen","Arya Stark","Sandor Clegane","Cersei Lannister","Sansa Stark","Lord Varys","Jaime Lannister","Tyrion Lannister" ),selected = "Jon Snow",multiple = F),
                             
                             ),
                           
                           mainPanel(
                             plotOutput("distPlot" )
                             ),
                          value ="Temps de présence par épisode" ),
                  
                   
                  tabPanel("Number of deaths",
                           sidebarPanel(
                             sliderInput("bins",
                                         "Nombre de saisons:",
                                         min = 1,
                                         max = 8,
                                         value = 8)
                             ),
                           mainPanel(
                             plotOutput("distPlot01")
                             ),
                           value="Number of deaths")
                  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  ########################################
 
  output$distPlot <- renderPlot({
    
    p1 <-ggplot(appearances  %>%  filter(name=="Jon Snow") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) + 
      geom_line(aes(x=episodeId,y=time))+
      theme_bw()+
      xlab("épisode")+ylab("temps")
    if(input$characters == "Jon Snow"){
      p1 <- p1
    }else if(input$characters=="Arya Stark"){
      p1 <- ggplot(appearances  %>%  filter(name=="Arya Stark") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    else if(input$characters=="Daenerys Targaryen"){
      p1 <- ggplot(appearances  %>%  filter(name=="Daenerys Targaryen") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    else if(input$characters=="Sandor Clegane"){
      p1 <- ggplot(appearances  %>%  filter(name=="Sandor Clegane") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    else if(input$characters=="Sandor Clegane"){
      p1 <- ggplot(appearances  %>%  filter(name=="Sandor Clegane") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    else if(input$characters=="Cersei Lannister"){
      p1 <- ggplot(appearances  %>%  filter(name=="Cersei Lannister") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    else if(input$characters=="Sansa Stark"){
      p1 <- ggplot(appearances  %>%  filter(name=="Sansa Stark") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    else if(input$characters=="Lord Varys"){
      p1 <- ggplot(appearances  %>%  filter(name=="Lord Varys") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    else if(input$characters=="Jaime Lannister"){
      p1 <- ggplot(appearances  %>%  filter(name=="Jaime Lannister") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    else if(input$characters=="Tyrion Lannister"){
      p1 <- ggplot(appearances  %>%  filter(name=="Tyrion Lannister") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) 
    }
    p1 <- p1 + 
      geom_line(aes(x=episodeId,y=time))+
      theme_bw()+
      xlab("épisode")+ylab("temps")+theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Épisode",y="Temps de présence par épisode",title=paste("Temps de présence par épisode de",input$characters))
    
    p1
})
  #### sencond panel 
  
  output$distPlot01 <- renderPlot({
    s <- scenes
    e <- episodes
    
    if(input$bins == 8){
      deaths = s %>% select(nbdeath,duration,location,episodeId) %>% mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
      season_t = e  %>% mutate(t=cumsum(total_duration)) %>% 
        filter(episodeNum==1) %>% pull(t)
      p <- ggplot(deaths) + geom_line(aes(x=t/3600,y=tdeath)) +
        scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                           labels =   paste("Saison",1:8),)+
        scale_y_continuous("Nombre de morts cumulés", expand=c(0,0))
    }else if(input$bins==7){
      s <-s[(s$episodeId <= 66),]
      e <-e[(e$seasonNum <= 7),]
      
      deaths = s %>% select(nbdeath,duration,location,episodeId) %>% mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
      season_t = e  %>% mutate(t=cumsum(total_duration)) %>% 
        filter(episodeNum==1) %>% pull(t)
      p <- ggplot(deaths)+ geom_line(aes(x=t/3600,y=tdeath)) +
        scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                           labels =   paste("Saison",1:7),)+
        scale_y_continuous("Nombre de morts cumulés", expand=c(0,0))
    }else if(input$bins <= 6){
      s <-s[(s$episodeId <= input$bins*10),]
      e <-e[(e$seasonNum <= input$bins),]
      
      deaths = s %>% select(nbdeath,duration,location,episodeId) %>% mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
      season_t = e  %>% mutate(t=cumsum(total_duration)) %>% 
        filter(episodeNum==1) %>% pull(t)
      p <- ggplot(deaths)+ geom_line(aes(x=t/3600,y=tdeath)) +
        scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                           labels =   paste("Saison",1:input$bins),)+
        scale_y_continuous("Nombre de morts cumulés", expand=c(0,0))
    }
    
    
    
    
    
    p+theme_bw() + theme(axis.text.x=element_text(angle=90))+ ggtitle("Evolution du nombre de mort au cours du temps")

    
})
  

  
}

shinyApp(ui = ui, server = server)

