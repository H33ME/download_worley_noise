#load the packages
library(shiny)
library(ambient)

#ui for side bar panels
ui.1<-sidebarLayout(
    sidebarPanel(
      numericInput("rows","Rows",value=500),
      numericInput("cols","Columns",value=500)
    ),
    mainPanel(
      #plot the image
      plotOutput("image"),
    )
    
  )

#ui for download button
ui.2<-fluidRow(
  column(width = 12,
         #download the image
         downloadButton("download","Download .png",class = "btn_block"))
)

#join the uis

ui<- fluidPage(
  ui.1,
  ui.2
)

server<- function(input,output,session){
  rows<- reactive({
    input$rows
  })
  colmn<-reactive({
    input$cols
  })
  
  noise.simplx <- reactive({ 
    noise_simplex(c(rows(), colmn()), 
                  pertubation = 'normal', 
                           pertubation_amplitude = 40)
    })
  img<- function(){
    plot(as.raster(normalise(noise.simplx())))
  }
  output$image<- renderPlot({
    img()
  })
   output$download<- downloadHandler(
     filename = function(){
       "worley_noise.png"
     },
     content = function(file){
       png(file)
       img()
       dev.off()
     }
   )
  
}

shinyApp(ui,server)
