#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

f1 <- function(vect){
    return(length(which(is.na(vect)==FALSE)))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Study of sequence size extracted from Blast"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # to upload the sequence file
        fileInput(inputId="file1", 
                  label= "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Summary", tableOutput("table")),
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("Comparison", textOutput("comparison"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
      # reading of input file
      data <- reactive({
        inFile <- input$file1
        if (is.null(inFile)){
          df.seq <- read.csv("merge_060519.csv", sep=";")
        }else{   
          df.seq <- read.csv(inFile$datapath, sep=";")
        }
        return(df.seq)
      })
      
      #panel summary
      output$table <- renderTable({
        #determine le nombre de sequence
        
        v.taille <- apply(data(),2, f1)
        v.min <- apply(data(),2, min, na.rm=T)
        v.mean <- apply(data(),2, mean, na.rm=T)
        v.median <- apply(data(),2, median, na.rm=T)
        v.max <- apply(data(),2, max, na.rm=T)
        v.sd <- apply(data(),2, sd, na.rm=T)
        mat.res <- rbind(v.taille,
                         v.min,
                         v.mean,
                         v.median,
                         v.max,
                         v.sd)
        mat.res <- data.frame(c("number of sequence","min","mean","median","max","standard deviation"), mat.res)
        colnames(mat.res) <- c(" ", colnames(data()))
        mat.res 
      })
      
      #panel plot
      output$plot <- renderPlot({
        boxplot(data(), main="distribution of sequence size", las=2)
      })
      
      #panel comparison
      output$comparison<- renderText({
        #preparation of data for bartlett test
        allSize <- NULL
        allType <- NULL
        for(i in 1:ncol(data())){
          ind.ssNA <- which(is.na(data()[,i])==FALSE)
          allSize <- c(allSize, data()[ind.ssNA, i])
          allType <- c(allType, rep(colnames(data())[i], length(ind.ssNA)))
        }
        pval = bartlett.test(allSize, allType)$p.value
        paste("Comparison of variances : Test de Bartlett ", "p-value:", pval, sep=" ")
        
      })

}

# Run the application 
shinyApp(ui = ui, server = server)

