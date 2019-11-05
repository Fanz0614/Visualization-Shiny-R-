#Guohao Zong
#27995356

#Please make sure you have these packages already
#install.packages("plot3D")
#install.packages("dplyr")
#install.packages("RColorBrewer")

#loading packages
library(plot3D)
library(dplyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(RColorBrewer)


#read data from working forlder
#accident <- read.csv("./accident.csv")
#setwd('C:/Users/DELL/Desktop/shiny project')
casualty <- read.csv('C:/Users/DELL/Desktop/shiny project/casualty.csv')
#extract pedestrian data
ped <- filter(casualty, Casualty_Type == "Pedestrian" )

#read z-axis files
z_all1 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_all(a).csv')
z_all2 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_all(b).csv')
z_ped1 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_ped(a).csv')
z_ped2 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_ped(b).csv')
z_car1 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_car(a).csv')
z_car2 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_car(b).csv')
z_bus1 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_bus(a).csv')
z_bus2 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_bus(b).csv')
z_cyc1 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_cyc(a).csv')
z_cyc2 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_cyc(b).csv')
z_mot1 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_mot(a).csv')
z_mot2 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_mot(b).csv')
z_van1 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_van(a).csv')
z_van2 <- read.csv('C:/Users/DELL/Desktop/shiny project/3D_z_axis/z_van(b).csv')
#put z_axis in a list
mylist = list(z_all1, z_all2, z_ped1, z_ped2,
              z_car1, z_car2, z_bus1, z_bus2,
              z_cyc1, z_cyc2, z_mot1, z_mot2,
              z_van1, z_van2)

#create x, y axis for 3Dplot
x <- range(casualty$Age)
x <- seq(x[1], x[2], length.out=104)  
y <- range(casualty$Time)
y <- seq(y[1], y[2], length.out=24)


#ui

ui <- navbarPage("Traffic Accidents",
       #create tab "Based on Age & Time"
       tabPanel("Based on Age & Time",
          sidebarLayout(
            sidebarPanel(
              h4("The theme of this page is the distribution of casualties and the corresponding proportion of seriously injured casualties based on conbination of ages and time."),
              br(),
              radioButtons(
                inputId = "Type3D",
                label = "Type of Casualty: ",
                c("All" = "1",
                  "Pedestrian" = "3",
                  "Car driver/passenger" = "5",
                  "Bus driver/passenger" = "7",
                  "Van driver/passenger" = "9",
                  "Cyclist" = "11",
                  "Motor rider" = "13"
                  ),
                selected = "3"
              ), br(),br(),
              
              h3("Animation (click the play button to find out more):"),
              sliderInput("View",
                          "Angle of view:",
                          min = 0,
                          max = 360,
                          value = 0,
                          step = 3,
                          animate=
                          animationOptions(
                            interval= 400,
                            loop=TRUE)),
              br(),
              h3("Statistic summary of plot 1:"),
              tableOutput("summary3d1"),
              br(),
              h3("Statistic summary of plot 2:"),
              tableOutput("summary3d2")
            ),
            mainPanel(
              h3("Plot 1: No. of casualties"),
              plotOutput("plot3D1", width = 450, height = 450),
              br(),br(),
              h3("Plot 2: Proportion of seriously injured"),
              plotOutput("plot3D2", width = 450, height = 450),
              br(),br(),
              h3("Summary & Interesting findings:"),
              pre(includeText("./summaryTxt/summary3D.txt"))
            )
          )
        ),
       #create tab "Based on Geographic Locations"
       tabPanel("Based on Geographic Locations",
                sidebarLayout(
                  sidebarPanel(
                    h4("The theme of this page is the geographic distribution of accident data on map. The red pot gives the position and the cluster circle gives the numbers of accident in that region"),
                    sliderInput("rangeYear",
                                "Range of year:",
                                min = 2010,
                                max = 2015,
                                value = c(2011,2014)),
                    br(),br(),
                    checkboxGroupInput(
                                  inputId = "Severity",
                                  label = "Severity level (tick on 'Slight' will slow down your computer seriously!!): ",
                                  c("Slight" = "Slight",
                                    "Serious" = "Serious",
                                    "Fatal" = "Fatal"),
                                  selected = c("Fatal")
                                ), br(),
                    checkboxGroupInput(
                      inputId = "TypeMap",
                      label = "Type of Casualty: ",
                      c(
                        "Pedestrian" = "Pedestrian",
                        "Car driver/passenger" = "Car",
                        "Bus driver/passenger" = "Bus",
                        "Van driver/passenger" = "Van",
                        "Cyclist" = "Cycleist",
                        "Motor rider" = "Motor"
                      ),
                      selected = c("Car","Pedestrian")
                    ), br(),
                    sliderInput("rangeTime",
                                "Range of Time:",
                                min = 0,
                                max = 23,
                                value = c(8,10)),
                    br(),br(),
                    sliderInput("rangeAge",
                                "Range of Age:",
                                min = 0,
                                max = 103,
                                value = c(6,16)),
                    br(),br(),
                    h3("Summary:"),
                    h4("As expected, most of the data gathered at big cities, like London, Manchester, Edinburgh and Birmingham. The user may set the filter to find the location of some explicit groups, 
e.g.By setting Age in (6,16), type = 'Pedestrian', most of the data will gathered arround schools")
                  ),
                  mainPanel(
                    h3("Geographic locations:"),
                    leafletOutput("mymap",height = 600)
                  )
                )
                
       ),
       #create tab "Based on year"
       tabPanel("Based on Year",
                sidebarLayout(
                  sidebarPanel(
                    h4("The theme of this page is the trends of different types of causualties through the five years"),
                    plotOutput("plotYear"),
                    br(),br(),
                    checkboxGroupInput(
                      inputId = "TypeYear",
                      label = "Type of Casualty: ",
                      c("Pedestrian" = "Pedestrian",
                        "Car driver/passenger" = "Car",
                        "Bus driver/passenger" = "Bus",
                        "Van driver/passenger" = "Van",
                        "Cyclist" = "Cycleist",
                        "Motor rider" = "Motor",
                        "others" = "others"
                      ),
                      selected = c("Pedestrian", "Car","Bus")
                    )),
                  mainPanel(
                    h3("Trends categorized by types"),
                    plotOutput("plotggYear"),
                    h3("Summary & interesting findings:"),
                    pre(includeText("./summaryTxt/summaryYear.txt"))
                  )
                  
                )),
       #create tab "Pedestrian concern"
       tabPanel("Pedestrian concern",
                tabsetPanel(
                  #create sub-tab "Pedestrian Movement"
                  tabPanel("Pedestrian Movement",
                           sidebarLayout(
                             sidebarPanel(
                               h4("The theme of this subpage is pedestrian movement situation when the accident happened"),
                               br(),
                               radioButtons(
                                 inputId = "BarMethod",
                                 label = "Type of Bars: ",
                                 c("Fill" = "fill",
                                   "Stack" = "stack",
                                   "Dodge" = "dodge"),
                                 selected = "fill"
                               ),
                               br(),
                               
                               h4("Expanatory for movement index : "),
                               pre(includeText("./explanationTxt/expMove.txt")),
                               h3("Summary :"),
                               pre(includeText("./summaryTxt/summaryPedMove.txt"))
                             
                               ),
                             mainPanel(
                               h3("Proportions of injury levels categorized by movement cases :"),
                               plotOutput("plotPedMove"),
                               br(),br(),br(),br(),br(),
                               h3("Combined with light conditions :"),
                               plotOutput("plotPedMoveLight")
                             )
                           )),
                  #create sub-tab "Pedestrian Location"
                  tabPanel("Pedestrian Location",
                           sidebarLayout(
                             sidebarPanel(
                               h4("The theme of this subpage is pedestrian location situation when the accident happened"),
                               br(),
                               radioButtons(
                                 inputId = "BarMethod2",
                                 label = "Type of Bars: ",
                                 c("Fill" = "fill",
                                   "Stack" = "stack",
                                   "Dodge" = "dodge"),
                                 selected = "fill"
                               ),
                               br(),
                               h4("Expanatory for location index : "),
                               pre(includeText("./explanationTxt/expLoca.txt")),
                               h3("Summary :"),
                               pre(includeText("./summaryTxt/summaryPedLoca.txt"))
                               
                             ),
                             mainPanel(
                               h3("Proportions of injury levels categorized by location types :"),
                               plotOutput("plotPedLoca"),
                               br(),br(),br(),br(),br(),
                               h3("Combined with light conditions :"),
                               plotOutput("plotPedLocaLight")
                             )
                           ))
                )
                )
)



# server
server <- function(input, output, session){
 #construct the leaflet map
 output$mymap <- renderLeaflet(
    leaflet() %>% addTiles() %>% addMarkers(data = subset(casualty, Year %in% (input$rangeYear[1]:input$rangeYear[2]) & Time %in% (input$rangeTime[1]:input$rangeTime[2]) & Casualty_Type %in% c(input$TypeMap) & Age %in% (input$rangeAge[1]:input$rangeAge[2]) & Accident_Severity %in% c(input$Severity)), ~Longitude, ~Latitude,clusterOptions = markerClusterOptions(), popup = ~as.character(Accident_Severity)) %>% addCircleMarkers(data = subset(casualty, Year %in% (input$rangeYear[1]:input$rangeYear[2]) & Time %in% (input$rangeTime[1]:input$rangeTime[2]) & Casualty_Type %in% c(input$TypeMap) & Age %in% (input$rangeAge[1]:input$rangeAge[2]) & Accident_Severity %in% c(input$Severity)), ~Longitude, ~Latitude, color = "red", opacity = 0.2, radius = 0.1 )
)

 # create a subset "buffer" of the data
 buffer <-  reactive(filter(casualty, Casualty_Type %in% c(input$TypeYear)))

 #create a stack bar plot for year
 output$plotYear <- renderPlot(
   ggplot(buffer(), aes(x = Year, fill = factor(Casualty_Type))) + geom_bar(position = "stack", color = "black") + scale_fill_brewer(palette = "Set2") + theme_minimal() + theme(legend.position="top") + labs(title = "Percentages of different types of casualties")
)
 
 #create a set of lines for year
 output$plotggYear <- renderPlot(
   ggplot(buffer(), aes(x = factor(Year), group = 6)) + geom_line( stat = "count") + facet_wrap(~buffer()$Casualty_Type, scales = "free_y") + theme_minimal()
 )
 
 #create a bar plot for pedestrian
 output$plotPedMove <- renderPlot({
    ggplot(ped, aes(x = factor(Ped_Movement), fill = factor(Casualty_Severity))) + geom_bar(position = input$BarMethod, color = "black") + scale_fill_brewer(palette = "Set1") + theme_minimal() + labs( x = "Pedestrian Movement",  y = "Proportion of different injury level")}, height = 500, width = 700)
 
 #create a bar plot for pedestrian
 output$plotPedLoca <- renderPlot({
   ggplot(ped, aes(x = factor(Ped_Location), fill = factor(Casualty_Severity))) + geom_bar(position = input$BarMethod2, color = "black") + scale_fill_brewer(palette = "Set1") + theme_minimal() + labs( x = "Pedestrian Location",  y = "Proportion of different injury level")}, height = 500, width = 700)
 
 #create a set of bar plot for pedestrian
 output$plotPedLocaLight <- renderPlot({
   ggplot(ped, aes(x = factor(Ped_Location), fill = factor(Casualty_Severity))) + geom_bar(position = input$BarMethod2, color = "black") + facet_wrap(~Light_Conditions) + scale_fill_brewer(palette = "Set1") + theme_minimal() +labs( x = "Pedestrian Location",  y = "Light Conditions")}, height = 500, width = 800)
 
 #create a set of bar plot for pedestrian
 output$plotPedMoveLight <- renderPlot({
   ggplot(ped, aes(x = factor(Ped_Movement), fill = factor(Casualty_Severity))) + geom_bar(position = input$BarMethod, color = "black") + facet_wrap(~Light_Conditions) + scale_fill_brewer(palette = "Set1") + theme_minimal() + labs( x = "Pedestrian Movement",  y = "Light Conditions")}, height = 500, width = 800)
 
    #create a 3D plot
    output$plot3D1 <- renderPlot({persp(x,y,z = as.matrix(as.data.frame(mylist[as.numeric(input$Type3D)])), theta=input$View, phi=40, r=2, shade=0.4, axes=TRUE,scale=TRUE, box=TRUE, nticks=5, ticktype="detailed", col="cyan", xlab="Age", ylab="Time", zlab="Number of casualties")}, width = 900, height = 510)
    
    #create a 3D plot
    output$plot3D2 <- renderPlot({persp3D(x,y,z = as.matrix(as.data.frame(mylist[as.numeric(input$Type3D)+1])), zlim = range(0,1.6),theta=input$View, phi=40, axes=TRUE,scale=5, box=TRUE, nticks=5, ticktype="detailed",xlab="Age", ylab="Time", zlab="Prob. of seriously injured")},width = 900, height = 510)
    

    #create 3D plot statistic summary
    output$summary3d1 <- renderTable( data.frame(
      Index = c("Min", 
               "Median",
               "Mean",
               "Max",
               "Sum"),
      No.casualty = c(min(as.vector(unlist(mylist[as.numeric(input$Type3D)]))), 
                median(as.vector(unlist(mylist[as.numeric(input$Type3D)]))),
                mean(as.vector(unlist(mylist[as.numeric(input$Type3D)]))),
                max(as.vector(unlist(mylist[as.numeric(input$Type3D)]))),
                sum(as.vector(unlist(mylist[as.numeric(input$Type3D)])))
      )
    ))
    
    #create 3D plot statistic summary
    output$summary3d2 <- renderTable(data.frame(
      Index = c("Min", 
                "Median",
                "Mean",
                "Max"),
      RatioSeriously = c(min(as.vector(unlist(mylist[as.numeric(input$Type3D)+1])), na.rm = TRUE), 
                         median(as.vector(unlist(mylist[as.numeric(input$Type3D)+1])),na.rm = TRUE),
                         mean(as.vector(unlist(mylist[as.numeric(input$Type3D)+1])),na.rm = TRUE),
                         max(as.vector(unlist(mylist[as.numeric(input$Type3D)+1])),na.rm = TRUE)
      )
    ))
  
}

#run the shiny app
shinyApp(ui = ui, server = server)

