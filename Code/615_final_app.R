#data preparation
library(tidyverse)
library(magrittr)
terrorism<-read.csv("globalterrorismdb_0718dist.csv")
terrorism_2017<-terrorism %>% filter(iyear==2017)
happiness_2017<-read.csv("2017.csv")
library(leaflet)
library(rgdal)
library(RColorBrewer)
library(plotly)
#wordcloud
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
terrorism_2017$summary<-as.character(terrorism_2017$summary)
text<-terrorism_2017 %>%
  select(summary)
colnames(text)<-"text"
text$text<-str_sub(text$text, start = 13) 

tidy_text<-text %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

#pal = brewer.pal(9,"Dark2")
tidy_text %<>%
  count(word) %>%
  arrange(desc(n))

#map

## Download data from Natural Earth
url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"

tmp <- tempdir()

file <- basename(url)

download.file(url, file)

unzip(file, exdir = tmp)

## Read the data into R
countries <- readOGR(dsn=tmp,
                     layer = "ne_50m_admin_0_countries", 
                     encoding = "UTF-8")
country_name<-intersect(countries$SUBUNIT,terrorism_2017$country_txt)
country_name<-intersect(country_name,happiness_2017$Country)

t_2017<-sp::merge(countries,happiness_2017%>%filter(Country%in%country_name),
                  by.y="Country",by.x="SUBUNIT",sort=FALSE,duplicateGeoms =
                    TRUE,all.x=FALSE)
#boxplot
attack_happy<-left_join(happiness_2017,terrorism_2017,by = c("Country" = "country_txt"))
attack_happy %<>%
  group_by(Country,Happiness.Score) %>%
  summarise(count = n())
attack_happy$count[attack_happy$Country %in% setdiff(happiness_2017$Country,terrorism_2017$country_txt)]<-0

attack_num<-rep("0",dim(attack_happy)[1])
logical<-(attack_happy$count>=1) & (attack_happy$count<=5)
attack_num[logical]<-"(0,5]"
logical<-(attack_happy$count>5) & (attack_happy$count<=15)
attack_num[logical]<-"(5,15]"
logical<-(attack_happy$count>15) & (attack_happy$count<=55)
attack_num[logical]<-"(15,55]"
logical<-(attack_happy$count>55)
attack_num[logical]<-"(55,2466]"
#attack_num<-as.data.frame(attack_num)
attack_happy["attack_num"]<-attack_num
ave<-attack_happy%>%
  group_by(attack_num) %>%
  summarise(ave_happy = mean(Happiness.Score))

## set the levels in order we want
attack_happy <- within(attack_happy, 
                       attack_num <- factor(attack_num,
                                            levels=c("0","(0,5]","(5,15]","(15,55]","(55,2466]")))

#radar plot
radar_data<-left_join(happiness_2017,terrorism_2017,by = c("Country" = "country_txt"))
radar_data %<>%
  group_by(Country,Happiness.Score,Economy..GDP.per.Capita.,Family,Health..Life.Expectancy.,Freedom,Generosity,Trust..Government.Corruption.) %>%
  summarise(count = n())
radar_data$count[radar_data$Country %in% setdiff(happiness_2017$Country,terrorism_2017$country_txt)]<-0

attack_num<-rep("0",dim(radar_data)[1])

logical<-(radar_data$count>=1) & (radar_data$count<=5)
attack_num[logical]<-"(0,5]"
logical<-(radar_data$count>5) & (radar_data$count<=15)
attack_num[logical]<-"(5,15]"
logical<-(radar_data$count>15) & (radar_data$count<=55)
attack_num[logical]<-"(15,55]"
logical<-(radar_data$count>55)
attack_num[logical]<-"(55,2466]"
#attack_num<-as.data.frame(attack_num)
radar_data["attack_num"]<-attack_num
radar_data %<>%
  group_by(attack_num) %>%
  summarise_all(mean)%>%
  select(attack_num,Economy..GDP.per.Capita.,Family,Health..Life.Expectancy.,Freedom,Generosity,Trust..Government.Corruption.)
colnames(radar_data)<-c("attack_num","Economy","Family","Health","Freedom","Generosity","Trust to Government")

plotdata<-gather(radar_data,key=dimensions,value = score,-attack_num) 
plotdata<-plotdata %>% arrange(dimensions)
## set the levels in order we want
plotdata <- within(plotdata, 
                   attack_num <- factor(attack_num,
                                        levels=c("0","(0,5]","(5,15]","(15,55]","(55,2466]")))


#ui.R
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Terrorism & Happiness"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName = "introduction",icon = icon("air-freshener")),
      menuItem("EDA", tabName = "EDA", icon = icon("broom"),
               menuItem("Map",tabName="map",icon = icon("chess-queen")),
               menuItem("Wordcloud",tabName="word",icon = icon("comment")),
               menuItem("Terrorism vs Happiness",tabName="happiness",icon = icon("globe-americas")),
               menuItem("Terrorism vs Social Scores",tabName="other",icon = icon("connectdevelop")),
               menuItem("Benford Law",tabName="benford",icon = icon("bomb"))
      ),
      menuItem("Raw Data",tabName = "data",icon = icon("dashboard")),
      menuItem("Background",tabName = "background",icon = icon("key"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "introduction",
              fluidRow(
                box(title = "Welcome to Guangyan's app",width = 10,
                    solidHeader = TRUE,
                    status = "info",
                    h4("This app is for you to explore the Global Terrorism Data and Happiness Score Data of 2017. 
                    Besides, you can find some relationship between the number of terrorism one country have and the happiness of people in that country."),
                    h4("In EDA, there are 5 aspects to explore the data: map, word cloud, terrorism vs happiness, terrorism vs social scores, and Benford Law."),
                    h4("In Raw Data, you can see the original terrorism and happiness data.")
                    )
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                column(width = 12,
                       box(title = "Terrorism and Happiness Score",
                           "(The higher the happiness score, the happier people are)",
                           width=NULL,leafletOutput("map",height = 400))
                ),
                tabBox(width = 12, title = "Bar plot",
                       tabPanel("Countries",
                                sliderInput("top_n", "Top n countries with high attacks count", 1, 20, 10),
                                plotlyOutput("attack_country_p")),
                       tabPanel("Regions",plotlyOutput("type_region_p"))
                       )

              )
      ),
      tabItem(tabName = "word",
              fluidRow(
                  box(title = "Wordcloud for descriptions of terrorism",
                      sliderInput("word_num", "Number of word", 1, 200, 100),
                      wordcloud2Output("wordcloud",width=600),width = 10)

              )
      ),
      tabItem(tabName = "happiness",
              fluidRow(
                tabBox(width = 10, title = "happiness & #attacks",
                       tabPanel("Box Plot",
                                checkboxInput("mean","Mean"),
                                plotlyOutput("boxplot")),
                       tabPanel("Distribution",
                                plotlyOutput("distribution_age"))
                       )
              )
      ),
      tabItem(tabName = "other",
              fluidRow(
                
                       box(width = 9,
                         title = "Terrorism & scores related to happiness",
                         plotOutput("radar_plot"),
                         "Note: For these 6 factors, higher scores mean better situation."),
                
                       box( width = 3,
                         title = "#Terrorism",
                         checkboxGroupInput("group","Group of #Terrorism",
                                            choices = c("0","(0,5]","(5,15]","(15,55]","(55,2466]"),
                                            selected = c("0","(55,2466]")))
                       
              )
      ),
      tabItem(tabName = "benford",
              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    title = "Benfor Law",
                    plotOutput("benford"))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                tabBox(width = 12,height = 700, title = "Data",
                       tabPanel("Terrorism",
                                DT::dataTableOutput("terrorism")),
                       tabPanel("Happiness",
                                DT::dataTableOutput("happiness"))
                )                
              )
      ),
      tabItem(tabName = "background",
              fluidRow(
                box(title = "Terrorism",width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    h4("Definition of terrorism:",size = 12),
                    h4("The threatened or actual use of illegal force and violence by a non-state actor to attain a political, economic, religious, or social goal through fear, coercion, or intimidation.",
                       size = 12),
                    h4("The Global Terrorism Database (GTD) is an open-source database including information on terrorist attacks around the world from 1970 through 2017. 
The GTD includes systematic data on domestic as well as international terrorist incidents that have occurred during this time period and now includes more than 180,000 attacks. 
The database is maintained by researchers at the National Consortium for the Study of Terrorism and Responses to Terrorism (START), headquartered at the University of Maryland.",size = 10)),
                box(title = "Happiness",width = 6,
                    solidHeader = TRUE,
                    status = "primary",
                    h4("The World Happiness 2017, which ranks 155 countries by their happiness levels, was released at the United Nations at an event celebrating International Day of Happiness on March 20th. 
The happiness scores and rankings use data from the Gallup World Poll.",size = 12),
                    h4("The columns following the happiness score estimate the extent to which each of six factors – economic production, social support, life expectancy, freedom, absence of corruption, and generosity – contribute to making life evaluations higher in each country than they are in Dystopia, a hypothetical country that has values equal to the world’s lowest national averages for each of the six factors. 
They have no impact on the total score reported for each country, but they do explain why some countries rank higher than others.
",size = 12)
                    )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd",domain = t_2017$Happiness.Score)
    labels <- sprintf(
      "<strong>%s</strong><br/>%s Happiness",
      t_2017$SUBUNIT, round(t_2017$Happiness.Score,digits=2)) %>%
      lapply(htmltools::HTML)
    terrorism_2017_plot<-terrorism_2017 %>% filter(country_txt %in% country_name)
    m<-leaflet() %>%
      addTiles() %>%
      addPolygons(data=t_2017,
                  fillColor = ~pal(t_2017$Happiness.Score),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5,
                  label = labels) %>%
      addLegend(pal = pal, values = t_2017$Happiness.Score, opacity = 0.7, 
                title = "Happiness Score",bins = 5,
                position = "bottomleft") %>%
      addCircles(data = terrorism_2017_plot,
                 radius = ~(terrorism_2017_plot$nkill/5)^3, 
                 weight =1, fill=TRUE,color = "darkorchid",
                 popup= paste("<strong>Date: </strong>",
                              terrorism_2017_plot$iday,"/",terrorism_2017_plot$imonth,"/",
                              terrorism_2017_plot$iyear,
                              "<br><br><strong>Place: </strong>",
                              terrorism_2017_plot$city,"-",terrorism_2017_plot$country_txt,
                              "<br><strong>Killed: </strong>", terrorism_2017_plot$nkill,
                              "<br><strong>Wounded: </strong>", terrorism_2017_plot$nwound
                 ))
    m
    
  })
  
  output$attack_country_p<-renderPlotly({
    num_attack<-terrorism_2017 %>%
      group_by(country_txt) %>%
      summarise(count = n())
    plotdata<-num_attack %>%
      ungroup() %>%
      top_n(input$top_n,count)
    p1<-ggplot(plotdata) + geom_bar(aes(x = reorder(country_txt, -count),y = count),stat="Identity",fill = "skyblue") +
      labs(title = "Top10 countries with highest number of terrorism",
           x = "country",
           y = "number of terrorism") +
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5))
    ggplotly(p1)
  })
  
  output$type_region_p<-renderPlotly({
    p2<-ggplot(terrorism_2017) + geom_bar(aes(x = region_txt, fill = attacktype1_txt)) +
      guides(fill=guide_legend(
        keywidth=0.1,
        keyheight=0.3,
        default.unit="inch")) +
      labs(title = "Attack type and region",
           x = "region",
           y = "count") + 
      scale_fill_discrete(name="attack type") +
      theme(axis.text.x = element_text(angle = 30, vjust = 0.5))
    ggplotly(p2)
  })
  
  output$wordcloud<-renderWordcloud2({
    wordcloud2(tidy_text[1:input$word_num,])
  })
  
  output$boxplot<-renderPlotly({
    p<-ggplot(attack_happy) + geom_boxplot(aes(x = attack_num, y = Happiness.Score,fill = attack_num)) + 
      labs(title = "Number of terrorism & happiness score",
           x = "Count of attacks",
           y = "Happiness Score")
    if(input$mean)
      p <- p + geom_point(data = ave, aes(x = attack_num,y= ave_happy)) 
    ggplotly(p)
  })
  
  output$distribution_age<-renderPlotly({
    pp<-ggplot(attack_happy) + geom_bar(aes(x = attack_num,fill = attack_num)) +
      labs(title = "Distribution of the count of terrorism")
    ggplotly(pp)
  })
  
  output$radar_plot<-renderPlot({
    plotdata<-plotdata %>% filter(attack_num %in% input$group)
    ggplot(plotdata)+
      geom_polygon(aes(x = dimensions, y = score, color = attack_num, group = attack_num),alpha=0.5,fill = NA) +
      coord_polar(theta = "x", start=0, direction = 1) +
      theme_minimal()+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
#      theme(axis.title.y = element_text(margin = unit(c(0, 1, 0, 0), "mm"), angle = 0),
#            plot.margin = margin(0, 0, 0, 1, "cm"))+
      labs(x="scores")
  })
  
  output$benford<-renderPlot({
    library(benford.analysis)
    generosity<-happiness_2017$Generosity
    bfd.generosity <- benford(generosity)
    plot(bfd.generosity)
  })
  
  output$terrorism<-DT::renderDataTable({
    table_terro<-terrorism_2017 %>% select(eventid,iyear,imonth,iday,country_txt,region_txt,city,
                              longitude,latitude,attacktype1_txt,targtype1_txt,nkill,nwound)
   DT::datatable(table_terro,options = list(searching = TRUE,pageLength = 50,scrollX = T,scrollY = "300px"),rownames= FALSE)
  })
  
  output$happiness<-DT::renderDataTable({
    DT::datatable(happiness_2017,options = list(searching = TRUE,pageLength = 50,scrollX = T,scrollY = "300px"),rownames= FALSE)
  })
}

shinyApp(ui, server)
