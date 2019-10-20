setwd("C:\\Users\\user\\Google 雲端硬碟\\石百達\\盧佳琪\\data")

library(dplyr)
library(shiny)
library(ggplot2)
library(ggrepel)
library(directlabels)

source("EM & AQ 計算.R")


ui <- fluidPage(    
  
  # Title of the Shiny
  titlePanel("AQ & EM Matrix"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Year","Year:",choices= unique(AQ_EM_tse_final$年.月)),
      selectInput("Industry","Industry:",choices= unique(AQ_EM_tse_final$TSE新產業_名稱)),
      
      selectInput("AQ","AQ:",choices= c("SD_accruals","DD_measure","Persistence_resid","Diff_persistence_resid"
                                        ,"yoy_ebit_t","yoy_operating_t")),
      
      selectInput("EM", "EM:", choices = c("Jones_model_measure","Modified_Jones_model_measure","Performance_matching_measure"))
      
    ),
    # Create a spot for the barplot
    mainPanel(
      plotOutput("plot")  
    )
  )
)



server <- function(input, output) {
  
  
  year = reactive({input$Year})          
  industry = reactive({input$Industry})         
  AQ = reactive({input$AQ})             
  EM = reactive({input$EM})              
  
  
  output$plot<-renderPlot({
    

    plot_data <- AQ_EM_tse_final %>% filter(年.月 == year(), TSE新產業_名稱 == industry()) 
    
    plot <- qplot(plot_data[,AQ()] , plot_data[,EM()], xlab = AQ(), ylab = EM(), 
                  main = paste0("AQ EM Matrix of ",industry()," ",year())) + 
      geom_point(color = "blue", size = 3)+
      geom_label_repel(aes(label = plot_data$簡稱),
                       box.padding   = 0.02, 
                       point.padding = 0.5,
                       segment.color = 'grey50')

      
      
    #plot <- qplot(test_plot_data[,AQ()] , test_plot_data[,EM()], xlab = AQ(), ylab = EM()) + 
    #  geom_text(aes(label=test_plot_data$簡稱),hjust=0, vjust=0)

      #plot(x= test_plot_data[,AQ()], y = test_plot_data[,EM()])
      
      #qplot(x = AQ() , y = EM(), data = test_plot_data) 
 
    return(plot)
    })

  }

shinyApp(ui=ui,server=server)



