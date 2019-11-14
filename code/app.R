

require(dplyr)
require(shiny)
require(ggplot2)
require(ggrepel)
require(directlabels)
require(tidyr)
require(broom)
require(plotly)
require(TTR)

# Clear the Global Environment
rm(list = ls())


options_backup <- options()
options(encoding = "UTF-8")


source("EM & AQ Calculate.R")


ui <- fluidPage(    
  
  # Title of the Shiny
  titlePanel("AQ & EM Matrix"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Year","Year:",choices= unique(AQ_EM_tse_final$yyyymm)),
      selectInput("Industry","Industry:",choices= c(as.character(unique(AQ_EM_tse_final$tse_industry_name)), "All Sectors")),
      
      selectInput("Index_Factor","Index Factor:",choices= c("TWN50", "TM100","TF001")),
      
      
      selectizeInput("Company_Code", "Company Code:", choices = c(" ",as.character(AQ_EM_tse_final %>%  distinct(company) %>% select(company)  %>% .[[1]]))),
      
      
      selectInput("Persistence","Persistence:",choices= c("SD_accruals","DD_measure","Persistence_resid","Diff_persistence_resid"
                                        ,"yoy_ebit_t","yoy_operating_t","opacity_operating_slope","opacity_ebit_slope")),
      
      selectInput("EM", "EM:", choices = c("Jones_model_measure","Modified_Jones_model_measure","Performance_matching_measure"
                                           ,"opacity_Jones_model_measure","opacity_modified_Jones_model_measure",
                                           "opacity_performance_matching")),
      
      
      
      #selectInput("Company_Tag", "Company Tag:", choices = c("Y","N"))
      
    ),
    # Create a spot for the barplot
    mainPanel(
      plotlyOutput("plot")  
    )
  )
)



server <- function(input, output, session) {
  
  
  
  # The company code list based on yyyymm & industry
  
  observe({
    
    
    
    update_code <- AQ_EM_tse_final %>% 
      
      filter(
        
        if(input$Industry != "All Sectors"){
          
          yyyymm == input$Year & tse_industry_name  == input$Industry
          
        }else{
          
          yyyymm == input$Year
          
        }
        
        
        ) %>%
      
      drop_na(input$Persistence, input$EM) %>%
      distinct(company) %>%
      select(company)  %>% .[[1]]
    
    
    update_code <- c(" ", as.character(update_code))
    
    
    updateSelectizeInput(
      
      session,
      
      "Company_Code",
      
      choices = update_code
    )
    
    
    
    
  })
  
  
  
  # Dataset for plotting
  plot_data <- reactive({
    
    
    year =input$Year      
    industry = input$Industry      
    
    index_factor_input = input$Index_Factor 
    AQ_input = input$Persistence           
    EM_input = input$EM         
    
    company_tag_input = input$Company_Tag 
    
    
    
    plot_data <- if(industry != "All Sectors"){
      AQ_EM_tse_final %>% filter(yyyymm == year, tse_industry_name  == industry, !is.na(AQ_input), !is.na(EM_input)) %>%
        select(AQ_input, EM_input, index_factor_input, company,company_abbreviation) %>% 
        rename(AQ = AQ_input, EM = EM_input , index_factor = index_factor_input)
      
    }else{
        
      AQ_EM_tse_final_all_sectors %>% filter(yyyymm == year, !is.na(AQ_input), !is.na(EM_input)) %>%
        select(AQ_input, EM_input, index_factor_input, company,company_abbreviation) %>% 
        rename(AQ = AQ_input, EM = EM_input, index_factor = index_factor_input)
      
      }
    
  
    
    
    
    
    return(plot_data) 
    
  })
    
  
  
  
  output$plot <- renderPlotly({
    
    
   
      
      
      plot_data <- plot_data() 
      
      select_company_data <- plot_data %>% filter(company == input$Company_Code)
      
      company_selected_arrow <- list(
        x = select_company_data$AQ,
        y = select_company_data$EM,
        text = select_company_data$company,
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowwidth= 3,
        arrowhead = 50,
        ax = 20,
        ay = -40
      )
      
      
      
      
      matrix_plot <- plot_ly(plot_data, x = ~AQ, y = ~EM, type = 'scatter', mode = 'markers',
             color = ~index_factor, colors = c("blue", "red"),
             alpha = 0.3,
             size = 3,
             hoverinfo = 'text', 
             
             text = ~paste("company abbreviation :", company_abbreviation, '\n',
                          "company code :" , company ,'\n',
                          "AQ :", AQ ,'\n',
                          "EM :", EM
                                              
                                              
             )) %>% layout(showlegend = FALSE)

      
      
      if(input$Company_Code != " "){
      matrix_plot <- matrix_plot %>%  layout(annotations = company_selected_arrow)
      }else{
        
        NULL
      }
      
      
      return(matrix_plot)
      
      # ggplot(plot_data, aes(x = AQ, y = EM , text = paste("company abbreviation :", company_abbreviation,
      #                                                     
      #                                                      '\n',"company code :" , company 
      #                                                     
      #                                                     
      #                                                     ))) +
      # 
      #   geom_point(alpha = 0.5, size = 2, aes(color = index_factor)) +  
      #   scale_color_manual(values = c("blue", "red")) +
      #   theme(legend.position = "none")
      
      
      
      #geom_point(plot_data[which(is.na(plot_data$index_factor)) ,], mapping = aes(AQ, EM) , color =  "blue" , size = 2) +
      #geom_point(plot_data[which(plot_data$index_factor == 1) ,], mapping = aes(AQ, EM)  , color = "red" , size = 2) 
      
      
 
        
      # + if(input$Company_Tag == "Y"){
      #   
      #   if(input$Industry != "All Sectors"){
      #     geom_label_repel(aes(label = plot_data$company),
      #                      box.padding   = 0.02, 
      #                      point.padding = 0.5,
      #                      segment.color = 'grey50')}else{
      #                        
      #                        
      #                        geom_label_repel(
      #                          data = subset(plot_data, !is.na(index_factor)),
      #                          aes(label = company),
      #                          
      #                          box.padding   = 0.02, 
      #                          point.padding = 0.5,
      #                          segment.color = 'grey50')
      #                        
      #                      }
      #   
      # }else{
      #   
      #   NULL
      # }
    

    
  })
  
  
}

options(options_backup)

shinyApp(ui=ui,server=server)



