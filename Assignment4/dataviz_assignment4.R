library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggridges)
library(ggforce)
library(DT)

data_por <- read.csv("student-por.csv")
data_math <- read.csv("student-mat.csv")
data_combined <- rbind(data_por, data_math)


ui <- dashboardPage(
  dashboardHeader(title = "Student performance analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("overview of data", tabName = "page1", icon = icon("dashboard")),
      menuItem("family related factors influence", tabName = "page2", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .info-text {
          font-size: 14px;
        }
        .subtitle-box {
          background-color: #3c8dbc;
          color: white;
          padding: 10px;
          font-size: 20px;
          font-weight: bold;
          text-align: center;
          margin-bottom: 15px;
          border-bottom: none;
        }
  
        
        .grid-background {
          background-color: white; /* Change to the desired background color */
        }
        
      "))
    ),
    tabItems(
      tabItem(tabName = "page1",
              fluidRow(
                column(12,
                       tags$img(src = "PP_logotyp_ANG_CMYK.jpg",
                                alt = 'Poznan University of Technology Logo',
                                height = "auto", width = "100%"))
              ),
              fluidRow(
                class = "grid-background",
                column(width = 12, div(class = "subtitle-box", "Data Overview"))),
              fluidRow(
                class = "grid-background",
                box(title = "About the page", status = "info", solidHeader = FALSE, collapsible = TRUE, width = 12,
                    'In this section presented are the overall statistics about
                    the dataset.',
                    br(), br(),
                    'In each of the following visualisations you can
                    choose which grades to take into account, by selecting the
                    desired option. The grades are normalised such that the
                    values are in range (0, 20).',
                    br(), br(),
                    'Below you can also find an interactive version of the full
                    dataset, if you desire to have insight to the raw data.')),
              fluidRow(
                class = "grid-background",
                box(
                    title = "Students numbers", status = "info", solidHeader = FALSE, collapsible = FALSE, width = 4, 
                    div(class = "info-text", uiOutput("text1")),
                    radioButtons(inputId = "dataset0page1", 
                                 label = "Choose subject:", 
                                 choices = c("Portuguese" = "data_por", 
                                             "Maths" = "data_math"),
                                 selected = "data_por",
                                 inline = TRUE),
                    plotOutput("plot1",height = "100px")),
                box(title = "Average grade", status = "info", solidHeader = FALSE, collapsible = FALSE, width = 4,
                    div(class = "info-text", uiOutput("text2")),
                    radioButtons(inputId = "dataset1page1", 
                                 label = "Choose subject:", 
                                 choices = c("Portuguese" = "data_por", 
                                             "Maths" = "data_math", 
                                             "Both Combined" = "data_combined"),
                                 selected = "data_por",
                                 inline = TRUE),
                    plotOutput("plot2", height = "100px")
                    ),
                box(title= "Median grade",status = "info",solidHeader=FALSE,collapsible = FALSE, width=4, 
                    div(class = "info-text", uiOutput("text3")),
                    radioButtons(inputId = "dataset2page1", 
                                 label = "Choose subject:", 
                                 choices = c("Portuguese" = "data_por", 
                                             "Maths" = "data_math", 
                                             "Both Combined" = "data_combined"),
                                 selected = "data_por",
                                 inline = TRUE),
                    plotOutput("plot3", height = "100px"))
                ),
              fluidRow(
                class = "grid-background",
                box(title = "Table of records", status = "info", solidHeader = FALSE, collapsible = FALSE, width = 12, height = 650,
                    div(class = "info-text"),
                    radioButtons(inputId = "dataset3page1", 
                                 label = "Choose subject:", 
                                 choices = c("Portuguese" = "data_por", 
                                             "Maths" = "data_math", 
                                             "Both Combined" = "data_combined"),
                                 selected = "data_por",
                                 inline = TRUE),
                    DTOutput("plot4"))
              ))
      ,
      tabItem(tabName = "page2",
              fluidRow(
                column(12,
                       tags$img(src = "PP_logotyp_ANG_CMYK.jpg",
                                alt = 'Poznan University of Technology Logo',
                                height = "auto", width = "100%"))
              ),
              fluidRow(
                class = "grid-background",
                column(width = 12, div(class = "subtitle-box", "Family Related Factors Influence"))),
              fluidRow(
                class = "grid-background",
                box(title = "About the page", status = "info", solidHeader = FALSE, collapsible = TRUE, width = 12,
                    'In this section we have a closer insight into the
                    distribution of the grades from each of the subjects.',
                    br(), br(),
                    'In each of the plots you can choose which data you want to
                    see. In \'Performance and parents education\' plot is is
                    also possible to select one of the squares to see the
                    distribution inside that category.')),
              fluidRow(
                class = "grid-background",
                box(title = "Performance and parents education ", status = "info", solidHeader = FALSE, 
                    collapsible = FALSE, width = 5,
                    
                    radioButtons(inputId = "dataset", 
                                 label = "Choose subject:", 
                                 choices = c("Portuguese" = "data_por", 
                                             "Maths" = "data_math", 
                                             "both combined" = "data_combined"),
                                 selected = "data_por",
                                 inline = TRUE),
                    plotOutput("heatmapPlot",click = "heatmap_click", height = '200px', width = '300px'),
              
                    plotOutput("additionalPlot1", height = '200px', width = '300px'),
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }")
                    
                ),
                box(title = "Performance and family support", status = "info", solidHeader = FALSE, 
                    collapsible = FALSE, width = 6,
                    radioButtons(inputId = "dataset2", 
                                 label = "Choose subject:", 
                                 choices = c("Portuguese" = "data_por", 
                                             "Maths" = "data_math", 
                                             "Both Combined" = "data_combined"),
                                 selected = "data_por",
                                 inline = TRUE),
                    plotOutput("ridgelinePlot", height = '200px'),
                    
                    plotOutput("histogramPlot",height = '200px')
                )
              ) 
      )
    )
  )
)


# Define the server logic
server <- function(input, output) { 
  #Render the plot with information how may students in each group
  
  output$text1 <- renderUI({
   selected_data <- switch(input$dataset0page1,
           "data_por" = data_por,
           "data_math" = data_math,
           "data_combined" = data_combined)
    to_plot = switch(input$dataset0page1,
                     "data_por" = "Portugese ",
                     "data_math" = "Maths ",
                     "data_combined" = "All combined ")
    big_number <- nrow(selected_data)
    #paste(nrow(selected_data),": \nthe number of students in the",to_plot,"class is")
    HTML(paste(
      "<div style='text-align: center;'>",
      "<span style='font-size: 13px;'>The number of records in  ", to_plot, ":</span>",
      "<br>",
      "<span style='font-size: 50px; font-weight: bold;'>", big_number, "</span>",
      "</div>"
    ))
    })
    output$text2 <- renderUI({
      selected_data <- switch(input$dataset1page1,
                              "data_por" = data_por,
                              "data_math" = data_math,
                              "data_combined" = data_combined)
      to_plot = switch(input$dataset1page1,
                       "data_por" = "Portugese ",
                       "data_math" = "Maths ",
                       "data_combined" = "All combined")
      big_number <- round(mean(na.omit(selected_data$G3)),2)
      HTML(paste(
        "<div style='text-align: center;'>",
        "<span style='font-size: 13px;'>The average grade in ", to_plot, ":</span>",
        "<br>",
        "<span style='font-size: 50px; font-weight: bold;'>", big_number, "</span>",
        "</div>"
      ))
      
    })  
    
    output$plot2 <- renderPlot({
      selected_data <- switch(input$dataset1page1,
                              "data_por" = data_por,
                              "data_math" = data_math,
                              "data_combined" = data_combined)
      number_to_plot <- round(mean(na.omit(selected_data$G3)),2)
      remaining_value <- 20 - number_to_plot
      
      
      
      data_to_plot <- data.frame(
        category = c("Mean G3", "Remaining"),
        value = c(remaining_value, number_to_plot)
      )
     
      
      
      
      ggplot(data_to_plot, aes(x = "", y = value, fill = category)) +
        geom_bar(stat = "identity", width = 0.5,color = 'black') +
        scale_fill_manual(values = c("white", "dodgerblue3")) +
        scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20))+
        coord_flip()+
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.grid.major.x = element_line(color = "black", linetype = "solid", size = 0.5),
              legend.position = "none",
              plot.margin = margin(t = 25, r = 15, b = 25, l = 15)) +
        labs(title = "average/highest grade ratio ")
      
    })
    
    output$text3 <- renderUI({
      selected_data <- switch(input$dataset2page1,
                              "data_por" = data_por,
                              "data_math" = data_math,
                              "data_combined" = data_combined)
      to_plot = switch(input$dataset2page1,
                       "data_por" = "Portugese ",
                       "data_math" = "Maths ",
                       "data_combined" = "All combined ")
      big_number <- median(na.omit(selected_data$G3))
      HTML(paste(
        "<div style='text-align: center;'>",
        "<span style='font-size: 13px;'>The median grade in ", to_plot, ":</span>",
        "<br>",
        "<span style='font-size: 50px; font-weight: bold;'>", big_number, "</span>",
        "</div>"
      ))
      
    })
    
    
    library(ggplot2)
    library(ggforce)
    
    
    library(ggplot2)
    
    
    
    library(ggplot2)
    library(ggforce)
    library(UpSetR)
    
    library(shiny)
    library(ggplot2)
    library(UpSetR)
    library(dplyr)
    
    
    output$plot1 <- renderPlot({
      selected_data <- switch(input$dataset0page1,
                              "data_por" = data_por,
                              "data_math" = data_math,
                              "data_combined" = data_combined)
      
      number_por <- nrow(data_por)
      number_maths <- nrow(data_math)
      both_subjects <- nrow(merge(data_por, data_math, by = c("school", "sex", "age", "address",
                                                              "famsize", "Pstatus", "Medu", "Fedu",
                                                              "Mjob", "Fjob", "reason", "nursery", "internet")))
      
      venn_data <- data.frame(
        x = c(0.75, 1.25),
        y = c(1, 1),
        r = c(1, 1),
        group = factor(c("only Maths", "only Portuguese"))
      )
      
      # Data for the text
      text_data <- data.frame(
        x = c(0.01, 2.1, 1),
        y = c(1, 1, 1),
        label = c(number_maths - both_subjects, number_por - both_subjects, both_subjects)
      )
      
      if (input$dataset0page1 == "data_por") {
        text_data <- text_data[c(FALSE, TRUE, TRUE), ]  # Keep only Portuguese and both subjects
      } else if (input$dataset0page1 == "data_math") {
        text_data <- text_data[c(TRUE, FALSE, TRUE), ]}
      
      
      colors <- c("gray", "gray", "gray")
      
      if (input$dataset0page1 == "data_por") {
        colors <- c("gray", "blue", "gray")
      } else if (input$dataset0page1 == "data_math") {
        colors <- c("blue", "gray", "gray")
      } else if (input$dataser0page1 == "data_combined"){
        colors <- c("gray","gray","blue")
      }
      
      ggplot() +
        geom_circle(aes(x0 = x, y0 = y, r = r, fill = group), 
                    data = venn_data, alpha = 0.5, color = "black") +
        geom_text(aes(x = x, y = y, label = label), 
                  data = text_data, size = 6, color = "black") +
        scale_fill_manual(values = colors) +
        theme_void() +
        labs(title = "Students distribution")
        
    })
    
    
    
    
    output$plot2 <- renderPlot({
      selected_data <- switch(input$dataset1page1,
                              "data_por" = data_por,
                              "data_math" = data_math,
                              "data_combined" = data_combined)
      number_to_plot <- round(mean(na.omit(selected_data$G3)),2)
      remaining_value <- 20 - number_to_plot
      
      data_to_plot <- data.frame(
        category = c("Mean G3", "Remaining"),
        value = c(remaining_value, number_to_plot)
      )
      
      ggplot(data_to_plot, aes(x = "", y = value, fill = category)) +
        geom_bar(stat = "identity", width = 0.5, color = 'black') +
        scale_fill_manual(values = c("white", "dodgerblue3")) +
        scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20)) +
        coord_flip() +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.grid.major.x = element_line(color = "black", linetype = "solid", size = 0.4),
              legend.position = "none",
              plot.margin = margin(t = 25, r = 15, b = 25, l = 15)) +
        labs(title = "average/highest grade ratio")
    })
    
    
      
      output$plot3 <- renderPlot({
        selected_data <- switch(input$dataset2page1,
                                "data_por" = data_por,
                                "data_math" = data_math,
                                "data_combined" = data_combined)
        number_to_plot <- median(na.omit(selected_data$G3))
        remaining_value <- 20 - number_to_plot
        
        
        
        data_to_plot <- data.frame(
          category = c("Mean G3", "Remaining"),
          value = c(remaining_value, number_to_plot)
        )
        lines_data <- data.frame(
          y = seq(0, 20, 2))
        
        
        
        
        ggplot(data_to_plot, aes(x = "", y = value, fill = category)) +
          geom_bar(stat = "identity", width = 0.5,color = 'black') +
          
          scale_fill_manual(values = c("white", "dodgerblue4")) +
          scale_y_continuous(breaks = seq(0, 20, 2), limits = c(0, 20))+
          coord_flip()+
          theme_minimal() +
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 8),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                panel.grid.major.x = element_line(color = "black", linetype = "solid", size = 0.4),
                legend.position = "none",
                plot.margin = margin(t = 25, r = 15, b = 25, l = 15)) +
          labs(title = "median/highest grade ratio ")
        
      })
      
      output$plot4 <- renderDT({
        selected_data <- switch(input$dataset3page1,
                                "data_por" = data_por,
                                "data_math" = data_math,
                                "data_combined" = data_combined)
        
        datatable(selected_data, options = list(lengthChange = FALSE, scrollX = TRUE))
        })
    
    
  
  # Render a ggplot heatmap
  output$heatmapPlot <- renderPlot({
    # Prepare the data
    selected_data <- switch(input$dataset,
                            "data_por" = data_por,
                            "data_math" = data_math,
                            "data_combined" = data_combined)
    
    selected_data <- selected_data %>%
      group_by(Medu, Fedu) %>%
      summarize(mean_G3 = ifelse(n() == 1, NA, mean(G3, na.rm = TRUE)),
                n_records = n()) %>%
      ungroup() %>%
      complete(Medu, Fedu, fill = list(mean_G3 = NA))
    
    # Create the heatmap
    ggplot(selected_data, aes(x = Medu, y = Fedu, fill = mean_G3)) +
      geom_tile(color="black") +
      scale_fill_gradient(low = "#fff7fb", high = "#023858", na.value = "red", name = "red: <=1 records\n\n Avg Performance") +
      theme_minimal() +
      labs(title = "School performance given\n parents' education level",
           x = "Mother's level of education",
           y = "Father's level of education",
           fill = "School performance",
           ) +
      theme(plot.title = element_text(face = "bold"),
            legend.title = element_text(margin = margin(t = 20, b = 0)))
  })
  
  # Render additional plots
  output$additionalPlot1 <- renderPlot({
    # # Generate some random data
    # set.seed(123)
    # x <- rnorm(100)
    # y <- rnorm(100)
    # 
    # # Create a scatter plot
    # plot
    clicked_data <- switch(input$dataset,
                           "data_por" = data_por,
                           "data_math" = data_math,
                           "data_combined" = data_combined)
    point = input$heatmap_click
    range_x = 4.82
    range_y = 4.87
    left_x = -0.43
    left_y = - 0.36
    #creating mapping from empirically tested measurments
    if (!is.null(point)) {
      clicked_Medu = floor(abs((point$x-left_x)/(range_x/5)))
      clicked_Fedu = floor(abs((point$y-left_y)/(range_y/5)))
      data_to_plot_clicked <- clicked_data %>%
        filter(Medu == clicked_Medu, Fedu == clicked_Fedu)
    }
    
    
    if (!is.null(data_to_plot_clicked)) {
      ggplot(data_to_plot_clicked, aes(x = "", y = G3, fill = "Records")) +
        geom_violin(alpha = 0.7)+
        geom_sina() +
        labs(title = paste("Grade distribution\nmother edu.", clicked_Medu,
                            " father edu.",clicked_Fedu),
             x = "",
             y = "grade",
             fill = "") +
        scale_fill_manual(values = "white", guide = FALSE) +  # Turn off the fill legend
        guides(fill = guide_legend(title = paste("Number of records:", nrow(data_to_plot_clicked)),
                                   keywidth = 0.5, keyheight = 0.5,
                                   title.position = "top", label.position = "bottom",
                                   label.hjust = 0, label.vjust = 0,
                                   nrow = 1, byrow = TRUE)) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0,face="bold"),
              legend.position = "bottom",  # Move legend to the bottom
              legend.justification = "right",  # Align legend to the right
              legend.box = "horizontal",  # Display legend horizontally
              legend.margin = margin(t = -30, r = 0, b = 0, l = 0),  # Remove legend margin
              legend.key.size = unit(0.5, "lines"),  # Decrease legend key size
              legend.title = element_text(size = 10),  # Decrease legend title size
              legend.text = element_text(size = 8))  # Decrease legend label size      
      
    }
    
    
    
    
    
  })
  
  # Render a ridgeline plot
  output$ridgelinePlot <- renderPlot({
    selected_data <- switch(input$dataset2,
                            "data_por" = data_por,
                            "data_math" = data_math,
                            "data_combined" = data_combined)
    #selected_data$famrel <- as.factor(selected_data$famrel)
    #selected_data$G3 <- as.factor(selected_data$G3)
    # Create ridgeline plot
    
    
    ggplot(selected_data, aes(x = G3, y = as.factor(famrel), fill=famrel)) +
      #geom_density_ridges(alpha=0.6, stat='binline',bins=20)
      geom_density_ridges(alpha=0.6,scale=1)+
      scale_fill_gradient(low = "#fff7fb", high = "#023858")+
      theme_ridges()+
      labs(title = "Ridgeline Plot of school\n performance by family support",
           x = "average school performance",
           y = "family support",
           fill = "family support")
    
  })
  
  
  output$histogramPlot <- renderPlot({
    selected_data_hist <- switch(input$dataset2,
                                 "data_por" = data_por,
                                 "data_math" = data_math,
                                 "data_combined" = data_combined)
    
    summary_data <- selected_data_hist %>%
      group_by(famrel) %>%
      summarize(mean_G3 = mean(G3, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(summary_data, aes(x = as.factor(famrel), y = mean_G3, fill = famrel)) +
      geom_col(color = "black", size = 1) +
      labs(title = "Average of performance by Family Support",
           x = "Family Support (famrel)",
           y = "Average G3",
           fill = "Family Support") +
      scale_fill_gradient(low = "#fff7fb", high = "#023858")+
      theme_minimal()+
      theme(
        plot.title = element_text(face = "bold")
      )
  })
  
}
# Combine UI and server to create the Shiny app
shinyApp(ui, server)

