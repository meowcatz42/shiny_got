    library(shiny)
    library(shinythemes)
    library(ggplot2)
    library(latticeExtra)
    library(rjson)
    library(sf)
    library(devtools)
    library(ggpubr)
    
    jstime = appearances %>% filter('name' == "Jon Snow") %>% 
      left_join(scenes) %>% 
      group_by(episodeId) %>% 
      summarise(time=sum(duration))
    
    y1 = ggplot(jstime, aes(x=episodeId, y=time)) + geom_point(aes(size=time)) + 
      geom_text(label = jstime$episodeId, hjust = -0.7, vjust=1.5, size = 1.9) + 
      coord_cartesian(xlim = c(30,75)) 
    
    t1 = ggplot(jstime, aes(x=episodeId, y=time)) + geom_point(aes(size=time)) + 
      geom_text(label = jstime$episodeId, hjust = -0.7, vjust=1.5, size = 1.9) + 
      coord_cartesian(xlim = c(0,30)) 
    
    # description des caract?res majeures de GOT
    
    dsc1 = "Jon Snow, born Aegon Targaryen, is the son of Lyanna Stark and Rhaegar Targaryen, the late Prince of Dragonstone. From infancy, Jon is presented as the bastard son of Lord Eddard Stark, Lyanna's brother, and raised alongside Eddard's lawful children at Winterfell, the seat of House Stark. However, Jon's true parentage is kept secret from everyone, including Jon himself, in order to protect him from those that sought the complete annihilation of House Targaryen after Robert's Rebellion and to maintain order in the realm."
    dsc2 = "Tyrion Lannister is a member of House Lannister and is the third and youngest child of Lord Tywin Lannister and the late Lady Joanna Lannister. His older siblings are Cersei Lannister, the queen of King Robert I Baratheon, and Ser Jaime Lannister, a knight of Robert's Kingsguard. Tyrion is a dwarf; because of this he is sometimes called the Imp and the Halfman. He is one of the major POV characters in the books. In the television adaptation Game of Thrones, Tyrion is played by Peter Dinklage."
    dsc3 = "A young woman in her early teens, Daenerys has the classical Valyrian look; She has violet eyes, pale skin, and long, pale silver-gold hair. Daenerys is slender of frame, with small breasts. She is said to resemble Queen Naerys Targaryen, though Daenerys is taller. Daenerys has been described as fair, and beautiful. Beside the Common Tongue, Daenerys speaks the bastard Valyrian of the Free Cities with a Tyroshi accent, and High Valyrian. She quickly learns the Dothraki language, and the Ghiscari tongue."
    dsc4 = "Sansa is traditionally beautiful, taking after her mother's family (House Tully) with her high cheekbones, vivid blue eyes, and thick auburn hair. She is eleven years old at the start of A Song of Ice and Fire. As she has grown up, her figure has been described as tall, graceful, and womanly. Sansa is described as soft-spoken and sweet-smelling. Raised as a lady, Sansa possesses the traditional feminine graces of her milieu, with a keen interest in music, poetry, singing, dancing, embroidery, and other traditional feminine activities. Like many girls her age, Sansa is enthralled by songs and stories of romance and adventure, particularly those depicting handsome princes, honorable knights, chivalry, and love. She keeps faith with both the old gods and the Seven."
    dsc5 = "Queen Cersei Lannister is the only daughter and eldest child of Lord Tywin Lannister of Casterly Rock and his wife, Lady Joanna Lannister. She is the twin of her younger brother, Ser Jaime Lannister. In the television adaptation Game of Thrones Cersei is played by Lena Headey and is portrayed when she is a child by Nell Williams in a Season 5 flashback. After Robert's Rebellion, Cersei married King Robert I Baratheon and became Queen of the Seven Kingdoms. She is the mother of Prince Joffrey, Princess Myrcella, and Prince Tommen of House Baratheon of King's Landing. Cersei becomes a POV character in A Feast for Crows."
    dsc6 = "Arya Stark is the third child and second daughter of Lord Eddard Stark and Lady Catelyn Tully. A member of House Stark, she has five siblings: brothers Robb, Bran, Rickon, half-brother Jon Snow, and older sister Sansa. Arya is one of the major POV character in A Song of Ice and Fire. In the television adaptation Game of Thrones, Arya is portrayed by Maisie Williams. Like some of her siblings, Arya sometimes dreams that she is a direwolf. Her own direwolf is Nymeria, who is named in reference to the Rhoynar warrior-queen of the same name."
    
    #Row data
    
    appearances = read.csv("data/appearances.csv")
    characters = read.csv("data/characters.csv")
    episodes = read.csv("data/episodes.csv")
    populations = read.csv("data/populations.csv")
    scenes = read.csv("data/scenes.csv")
    characters_image = subset(characters, select = -c(image))
    scenes_locations=st_read("./data/GoTRelease/ScenesLocations.shp",crs=4326)
    main_char= c("Jon Snow", "Tyrion Lannister","Daenerys Targaryen","Sansa Stark","Cersei Lannister","Arya Stark")
    landpol = st_union(st_geometry(land)) 
    islandpol = st_union(st_geometry(islands))
    backpol=st_union(landpol,islandpol)
    background = st_as_sf(data.frame(name=main_char,geometry=rep(backpol,6)))
    loc_time=appearances %>% filter(name %in% main_char) %>% left_join(scenes) %>% group_by(location,name) %>% summarize(duration=sum(duration,na.rm=TRUE)) 
    loc_time_mc = scenes_locations %>% left_join(loc_time)
    
    ui <- fluidPage(theme = shinytheme("lumen"),
                     
                     titlePanel(h2("Shiny App | GotData")), align = 'center',
                     sidebarLayout(
                       hr(),
                       tabsetPanel(
                         tabPanel(p(icon("database"),"Raw Data"),
                                  tags$blockquote(h5(" Un petit paragraphe ne ferait pas de mal ici")),
                                  sidebarPanel( 
                                    selectInput(inputId = "dataset",
                                                label = "Choose a dataset:",
                                                choices = c("appearances", "episodes", "characters","populations","scenes")),
                                    
                    
                                    numericInput(inputId = "obs",
                                                 label = "Number of observations to view:",
                                                 value = 5)
                                    
                                  ),
                                  
                                  mainPanel(
                                    
                                     verbatimTextOutput("summary"),
                                    
                                    fluidRow(
                                      column(4, offset = 1, 
                                             tableOutput("view"))
                                    )
                                    
                            )
                        ),
                        
                         tabPanel(p(icon("users"), "Major characters | Visualization & Analysis"),
                                  tags$blockquote(h5("Cette rubrique s'interesse aux caractères majeures de la s?rie t?l?vis?e Game Of thrones. Le choix de ces caract?res 
                                  est bas? d'abord sur la durée totale d'apparition des diff?rents caract?res de la s?rie. En effet, les diff?rentes donn?es fournies nous ont permis 
                                  de faire le classement des caract?res en termes de dur?e totale d'apparition dans la s?rie.
                                  Ce choix est valid? ?galement par l'avis de deux cin?philes de l'?cole Centrale Casablanca ; credits go to : Hamza El Hassnaoui/Dakir.")),
                                  
                                                     
                                  
                                  sidebarPanel(
                                    
                                    selectInput(inputId = "character",
                                                label = "Choose a character:",
                                                choices = c("Tyrion Lannister", "Jon Snow", "Daenerys Targaryen","Sansa Stark","Cersel Lannister", "Arya Stark"))
                                  ),
                          
                          
                    
                                  mainPanel(
                                    imageOutput("image2"),
                                    textOutput('dsc'),
                                    tags$blockquote(h4('La figure ci-dessous repr?sente le temps de pr?sence
                                       du caract?re choisi en fonction des diff?rents ?pisodes de
                                       la s?rie.')),
                                    plotOutput('show')
                                  )
                                  
                                  
                                  ),
                        tabPanel(p(icon("inbox"), "Appearance time by episode"),
                                 sidebarPanel(
                                   selectInput(inputId="characters",label="Choose Characters",choices = c("Jon Snow", "Daenerys Targaryen","Arya Stark","Sandor Clegane","Cersei Lannister","Sansa Stark","Lord Varys","Jaime Lannister","Tyrion Lannister" ),selected = "Jon Snow",multiple = F),
                                   
                                 ),
                                 
                                 mainPanel(
                                   plotOutput("distPlot" )
                                 )      
                        ),
                        
                        tabPanel(p(icon("info"),"Number of deaths"),
                                 
                                 sidebarPanel(
                                   sliderInput("bins",
                                               "Nombre de saisons:",
                                               min = 1,
                                               max = 8,
                                               value = 8)
                                 ),
                                 mainPanel(
                                   plotOutput("distPlot01")
                                 )
                        ),
                        
                        tabPanel(p(icon("map-pin"),"Map of appearances by character"),
                                 
                                 sidebarPanel(
                                     selectInput(inputId = "character1",
                                                 label = "Choose a character:",
                                                 choices = c("Tyrion Lannister", "Jon Snow", "Daenerys Targaryen","Sansa Stark","Cersel Lannister", "Arya Stark"))
                                 ),
                                 mainPanel(
                                     plotOutput("mapPlot")
                                 )
                        )
                        
                       )
                     )
    )
    
    server <- function(input, output) {
      
      datasetInput = reactive(
        
        switch(input$dataset, "appearances"=appearances, "episodes"=episodes, "characters"=characters_image, "populations"=populations, "scenes"=scenes))
      
      output$summary = renderPrint({
        dataset = datasetInput()
          summary(dataset)
    })
      
      output$view = renderTable({
          head(datasetInput(), n = input$obs)
      })
      
      output$image2 = renderImage({
        
        if ( input$character == "Jon Snow"){
          return(list(
            src = "./www/aaa.jpg",
            contentType = 'image/jpg', 
            width = 560, 
            height = 350
          ))
        }
        if ( input$character == "Tyrion Lannister"){
          return(list(
            src = "./www/bb.png",
            contentType = 'image/png', 
            width = 450, 
            height = 350
          ))
        }
        if ( input$character == "Daenerys Targaryen"){
          return(list(
            src = "./www/cc.jpeg",
            contentType = 'image/jpeg', 
            width = 530, 
            height = 330
          ))
        }
        if ( input$character == "Sansa Stark"){
          return(list(
            src = "./www/bbb.jpg",
            contentType = 'image/jpg', 
            width = 530, 
            height = 350
          ))
        }
        if ( input$character == "Cersel Lannister"){
          return(list(
            src = "./www/ccc.jpg",
            contentType = 'image/jpg', 
            width = 520, 
            height = 350
          ))
        }
        if ( input$character == "Arya Stark"){
          return(list(
            src = "./www/dd.jpg",
            contentType = 'image/jpg', 
            width = 530, 
            height = 370
          ))
        }
        
        
      }, deleteFile = FALSE)
      
      output$dsc = renderText({
        
        if (input$character == "Jon Snow"){
          return(paste(dsc1))}
        else if (input$character == "Tyrion Lannister"){
          return(paste(dsc2))}
        else if (input$character == "Daenerys Targaryen" ){
          return(paste(dsc3))}
        else if (input$character == "Sansa Stark"){
          return(paste(dsc4))
        }
        else if (input$character == "Cersel Lannister"){
          return(paste(dsc5))
        }
        else if (input$character == "Arya Stark"){
          return(paste(dsc6))
        }
      })
        
      output$show = renderPlot({
        
        jstime = appearances %>% filter(name == input$character) %>% 
          left_join(scenes) %>% 
          group_by(episodeId) %>% 
          summarise(time=sum(duration))
        
        y1 = ggplot(jstime, aes(x=episodeId, y=time)) + geom_point(aes(size=time)) + 
          geom_text(label = jstime$episodeId, hjust = -0.7, vjust=1.5, size = 1.9) + 
          coord_cartesian(xlim = c(30,75)) 
        
        t1 = ggplot(jstime, aes(x=episodeId, y=time)) + geom_point(aes(size=time)) + 
          geom_text(label = jstime$episodeId, hjust = -0.7, vjust=1.5, size = 1.9) + 
          coord_cartesian(xlim = c(0,30))
        
        
        figure = ggarrange(t1, y1, nrow = 2)
        
        print(figure)
          
      })
      
        
      output$distPlot = renderPlot({
          
          p1 <- ggplot(appearances  %>%  filter(name=="Jon Snow") %>%left_join(scenes) %>% group_by(episodeId) %>% summarise(time=sum(duration))) + 
            geom_line(aes(x=episodeId,y=time))+
            theme_bw()+
            xlab("?pisode")+ylab("temps")
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
            xlab("?pisode")+ylab("temps")+theme_bw()+
            theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"))+
            labs(x="?pisode",y="Temps de pr?sence par ?pisode",title=paste("Temps de pr?sence par ?pisode de",input$characters))
          
          print(p1)
      })
      
      output$distPlot01 = renderPlot({
        
        s = scenes
        e = episodes
        
        if(input$bins == 8){
          deaths = s %>% select(nbdeath,duration,location,episodeId) %>% mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
          season_t = e  %>% mutate(t=cumsum(total_duration)) %>% 
            filter(episodeNum==1) %>% pull(t)
          p <- ggplot(deaths) + geom_line(aes(x=t/3600,y=tdeath)) +
            scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                               labels =   paste("Saison",1:8),)+
            scale_y_continuous("Nombre de morts cumul?", expand=c(0,0))
          print(p)
        }
        else if(input$bins==7){
          s <-s[(s$episodeId <= 66),]
          e <-e[(e$seasonNum <= 7),]
          
          deaths = s %>% select(nbdeath,duration,location,episodeId) %>% mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
          season_t = e  %>% mutate(t=cumsum(total_duration)) %>% 
            filter(episodeNum==1) %>% pull(t)
          p <- ggplot(deaths)+ geom_line(aes(x=t/3600,y=tdeath)) +
            scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                               labels =   paste("Saison",1:7),)+
            scale_y_continuous("Nombre de morts cumul?", expand=c(0,0))
          print(p)
        }
        else if(input$bins <= 6){
          s <-s[(s$episodeId <= input$bins*10),]
          e <-e[(e$seasonNum <= input$bins),]
          
          deaths = s %>% select(nbdeath,duration,location,episodeId) %>% mutate(t=cumsum(duration),tdeath=cumsum(nbdeath))
          season_t = e  %>% mutate(t=cumsum(total_duration)) %>% 
            filter(episodeNum==1) %>% pull(t)
          p <- ggplot(deaths)+ geom_line(aes(x=t/3600,y=tdeath)) +
            scale_x_continuous("",expand = c(0,0),breaks = season_t/3600,
                               labels =   paste("Saison",1:input$bins),)+
            scale_y_continuous("Nombre de morts cumul?", expand=c(0,0))
          print(p)
        }
        })
      
      output$mapPlot = renderPlot({
          
          char_name = input$character1
                ggplot()+geom_sf(data=background,color=borderland,fill=colland)+
                      geom_sf(data=loc_time_mc%>% filter(!is.na(duration))%>%filter(name==char_name),aes(size=duration/60,color=name))+
                      geom_sf_text(data=loc_time_mc%>% filter(duration>60*60),aes(label=location),color="#000000",vjust="bottom",family="Palatino", fontface="italic")+
                      coord_sf(expand = 0,ndiscr = 0)+
                      scale_color_discrete(guide="none")+
                      scale_size_area("Durée (min) :",max_size = 12,breaks=c(30,60,120,240))+
                      #facet_wrap(~name)+
                      theme(panel.background = element_rect(fill = colriver,color=NA),
                            text = element_text(family="Palatino",face = "bold",size = 14),
                            legend.key = element_rect(fill="#ffffff"),
                      ) +
                      labs(title = paste("Répartition spatiale des scènes de", char_name, sep=" "),caption = "@comeetie, 2020",x="",y="")
      })
    }
    shinyApp (ui = ui, server = server)