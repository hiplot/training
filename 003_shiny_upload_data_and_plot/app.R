library(shiny)
library(DT)
library(stringr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("Calculate counts and draw plots"),
  sidebarLayout(
    sidebarPanel(
      
      # read in file 
      h3("Input section"),
      fileInput(
        inputId = "inputFile",
        label = "Upload you data",
        multiple = T,
        accept = c(".txt")
      ),
      hr(),
      
      # output table
      h3("Output section"),
      downloadButton(outputId = "outputTableFile",
                     label = "Download table"),
      hr(),
      
      # output plot
      radioButtons(
        inputId = "selectPlot",
        label = "Select plot",
        choices = c("Pie chart" = "pie", "Barplot" = "bar"),
        selected = "pie"
      ),
      downloadButton(outputId = "outputPlotFile",
                     label = "Download plot")
      
    ),
    
    mainPanel(
      h3("Count Table"),
      dataTableOutput(outputId = "outputTable"),
      hr(),
      
      h3("Plot"),
      plotOutput(outputId = "outputPlot")
    )
  )
)

server <- function(input, output, session) {
  
  output$outputTable <- renderDataTable({
    req(input$inputFile,cancelOutput = F)
    
    countTable = as.data.frame(table(read.table(input$inputFile$datapath,
                                                header = F,
                                                sep = "\t")))
    datatable(countTable)
  })
  
  output$outputTableFile <- downloadHandler(
    
    filename = function(){
      str_replace(input$inputFile,".txt$",".countTable.txt")
    },
    content = function(file){
      countTable = as.data.frame(table(read.table(input$inputFile$datapath,
                                                  header = F,
                                                  sep = "\t")))
      colnames(countTable) = c("Items","Count")
      write.table(countTable,file,row.names = F,quote=F,col.names = T,sep="\t")
    },
    contentType = ".txt"
  )
  
  observeEvent(input$selectPlot, {
    output$outputPlot <- renderPlot({
      req(input$inputFile,cancelOutput = F)
      
      countTable = read.table(input$inputFile$datapath,
                              header = F,
                              sep = "\t")
      colnames(countTable) = c("Items")
      
      p <- ggplot(countTable,aes(x=Items, fill=Items))+
        geom_bar(stat="count")+
        theme_bw()
      
      if(input$selectPlot == "pie"){
        p <- ggplot(countTable,aes(x="Items", fill=Items))+
          geom_bar(stat="count",width=0.5, position = "stack")+
          theme_minimal() +
          coord_polar("y", start=0)+
          xlab("")+
          ylab("")
          
      }
      return(p)
    })
    
    output$outputPlotFile <- downloadHandler(
      filename = function(){
        str_replace(input$inputFile,".txt$",".countTable.pdf")
      },
      content = function(file){
        countTable = read.table(input$inputFile$datapath,
                                header = F,
                                sep = "\t")
        colnames(countTable) = c("Items")
        
        p <- ggplot(countTable,aes(x=Items, fill=Items))+
          geom_bar(stat="count")+
          theme_bw()
        
        if(input$selectPlot == "pie"){
          p <- ggplot(countTable,aes(x="Items", fill=Items))+
            geom_bar(stat="count",width=0.5, position = "stack")+
            theme_minimal() +
            coord_polar("y", start=0)+
            xlab("")+
            ylab("")
        }
        ggsave(file,p,device = "pdf",width = 6,height = 6,units = "in")
      },
      contentType = ".pdf"
    )
  })
  
  
}

shinyApp(ui, server)