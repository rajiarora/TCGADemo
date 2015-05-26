#load("/helixdata/pc_tcga.Rda")
fumeric<-function(x) { as.factor(x)}
t<-table(rownames(pc_tcga$x))
t<-names(head(sort(t,dec=TRUE),10))
cancers<-t
# x_cancers<-pc_tcga$x[,1][rownames(pc_tcga$x) %in% cancers]
# y_cancers<-pc_tcga$x[,2][rownames(pc_tcga$x) %in% cancers]
# plot(x_cancers,y_cancers,col=fumeric(names(x_cancers)),pch=19, xlab="PC1", ylab="PC2",main="Principal component analysis of TCGA Data")
# legend("topright",unique(names(x_cancers)),cex=0.5,col=fumeric(unique(names(x_cancers))),pch=16)

server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    selectedLines <- input$checkGroup
    if (!(length(selectedLines) ==0))
    {
      #PCA, but will have to play with it a little, not very good right now
      plotpointsx<-pc_tcga$x[,1][rownames(pc_tcga$x) %in% selectedLines]
      plotpointsy<-pc_tcga$x[,2][rownames(pc_tcga$x) %in% selectedLines]
      colorsgroup=fumeric(names(plotpointsx))
      plot(plotpointsx, plotpointsy,main = "PCA", xlab = "PC1", ylab = "PC2", pch=16,col=colorsgroup)
      legend("topright",selectedLines,col=fumeric(selectedLines),pch=16)
    }
  })
  
}

ui <- shinyUI(  fluidPage(
  
  titlePanel("Interactive PCA Analysis of TCGA Data"),
  
  column(4, wellPanel(
    checkboxGroupInput("checkGroup", label = h3("Tissue types"), 
                       choices = cancers,
                       selected = cancers[1]),
   
    strong("Background"),
    br(),
    p("TBD - Brief writeup about the data")
    )),
  
  column(5,
         "",
         
         # With the conditionalPanel, the condition is a JavaScript
         # expression. In these expressions, input values like
         # input$n are accessed with dots, as in input.n
         conditionalPanel("TRUE",plotOutput("scatterPlot", height = 640, width=640)
         )
  )
    ))

shinyApp(ui = ui, server = server)