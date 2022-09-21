library(shiny)
library(tseries)
library(dplyr)
library(readxl)
library(ggplot2)
library(shinythemes)
sale <- read_excel("C:/Users/officiel/Desktop/Projects/sale/Alcohol_Sales.xlsx")
View(sale)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = "bootstrap.css",
  
  # Application title
  titlePanel("Alcohool Price"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      
      
      # add options to choose the visalisation type
      radioButtons('meth','meth',
                   choiceNames = c("compos saisonniere", "compos tendancielle","visualiser la serie","statistique decriptive"),
                   choiceValues = c("compos saisonniere", "compos tendancielle","visualiser la serie","statistique decriptive")),
      
      dateRangeInput("date_range", "Date range:",
                     start = min(sale$DATE),
                     end   = max(sale$DATE))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("trendPlot"),
    div(  tags$img(src="essai.jpg",height = "350px",width="850px",alt= "aaa",deleteFile=FALSE,style= "text-align:right;" ))    )
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$trendPlot <- renderPlot({
    
    
sale1 <- subset(sale, DATE>= input$date_range[1] & DATE <= input$date_range[2])
    date=as.Date(sale1$DATE)
    
    a<- filter(sale1$price, filter=array(1/10,dim=10), method = c("convolution"),
               sides = 2, circular = FALSE)
   
    if (input$meth == "compos tendancielle"){
      d<-a
      ggplot(data = as.data.frame(sale1), mapping = aes(x = date, y = d )) +  geom_line() +
        labs(title = "Composante tendancielle de la serie",
             x = "Date",
             y = "Composante tendancielle") + theme(text=element_text(size = 16))
      
      
    } else if (input$meth == "compos saisonniere"){
      d <-sale1$price - a
      ggplot(data = as.data.frame(sale1), mapping = aes(x = date, y = d)) +  geom_line() +
        labs(title = "Composante saisonniere de la serie",
             x = "Date",
             y = "Composante saisonniere")+ theme(text=element_text(size = 16))
    } else if (input$meth == "visualiser la serie"){
      d<- sale1$price
      ggplot(data = as.data.frame(sale1), mapping = aes(x = date, y = d)) +  geom_line() +
        labs(title = "Prix de l' Alcohool pour une periode choisi",
             x = "Date",
             y = "Price") + theme(text=element_text(size = 16))
    }
    else if (input$meth == "statistique decriptive"){
      par(mfrow = c(1, 2))
      p3 <- hist(sale1$price)
      p4<- boxplot(sale1$price, col = c("yellow"),main = "alcohool price Boxplot", ylab = "Quantiles")
     
      
    }
    
  })}

# Run the application 
shinyApp(ui = ui, server = server)

