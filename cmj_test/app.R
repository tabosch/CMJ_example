

library(DT)
library(ggplot2)
library(dplyr)
library(plyr)
library(readr)

ui <- fluidPage(
   title = "Force Plate Visuals",
   h1('Force Plate Visuals'),
  sidebarLayout(
    sidebarPanel(
      radioButtons("missingrows", "Is your data missing rows at the top of each file?", c("yes", "no"), selected = "no"),
      conditionalPanel("input.missingrows== 'no'",fileInput(
        inputId = "files", 
        label = "Choose CSV File", 
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      )),
      conditionalPanel("input.missingrows == 'yes'", numericInput("mr", "What row does your data start on?", min =1, max = 20, value=8)),
      conditionalPanel("input.missingrows == 'yes'",fileInput(
        inputId = "missing_rowsfiles", 
        label = "Choose CSV File", 
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      )),
      actionButton("runplot", "Plot Trials"),
      sliderInput("timefilter", "Select ranges to filter the time on the plots", min=0, max=6, step = 0.1, value = c(1,4)),
      actionButton("filter", "Filter Data"),
      radioButtons("corr", "Do you want to adjust the time series for any trials?", c("yes", "no"), selected = "no"),
      conditionalPanel("input.corr == 'yes'", selectInput("tc", "Which Trials do you want to adjust?", c("Trial 1",
                                                                                                         "Trial 2",
                                                                                                         "Trial 3",
                                                                                                         "Trial 4",
                                                                                                         "Trial 5",
                                                                                                         "Trial 6"),
                                                          multiple=T)),
      conditionalPanel("input.corr == 'yes'", numericInput("sec", "enter the number of seconds you want to adjust", min=0.1, max = 5, value=0.5)),
      conditionalPanel("input.corr == 'yes'",actionButton("correction", "Correct Time Series")),
      actionButton("limbs", "Show Each Limb Data")
    ),
    mainPanel(
      plotOutput("contents"),
      dataTableOutput("contents2"),
      plotOutput("contents3")
    )
  )
)

server <- function(input, output) {
  
  output$contents <- renderPlot({ 
    
    validate(
      need(input$runplot > 0, "Import Files and Click Plot Trials"
    ))
    
    if(input$filter == 0 && input$correction == 0) {
    #inFile <- input$files
    #jumpfiles <- inFile$datapath
    #data_csv <- ldply(jumpfiles, read_csv)
    #read_csv_filename <- function(jumpfiles){
   #   ret <- read.csv(jumpfiles)
  #    ret$Source <- jumpfiles #EDIT
 #     ret
#    }
      if(!is.null(input$files)) { 
        inFile <- input$files
        jumpfiles <- inFile$datapath }
      else {
        inFile <- input$missing_rowsfiles
        jumpfiles <- inFile$datapath}
      
      #data_csv <- ldply(jumpfiles, read_csv)
      read_csv_filename <- function(jumpfiles){
        if(!is.null(input$files)) {
          ret <- read.csv(jumpfiles)
        }
        else{ret <- read.csv(jumpfiles, skip = input$mr - 1)}
        ret$Source <- jumpfiles #EDIT
        ret
      }
    
    ## combines all .csv files into 1 data frame
    data_csv <- ldply(jumpfiles, read_csv_filename)
    data_csv$trial <- as.numeric(as.factor(data_csv$Source))
    data_csv$trial <- paste("Trial", data_csv$trial, sep = " ")
  ### plot the data
  
    #import.list
    ggplot(data_csv, aes(Time..s., Combined..N., colour=trial))+
      geom_line()+
      theme_bw() }
    else if(input$filter > 0  && input$correction == 0) { 
      #inFile <- input$files
   # jumpfiles <- inFile$datapath
    #data_csv <- ldply(jumpfiles, read_csv)
  #  read_csv_filename <- function(jumpfiles){
      #ret <- read.csv(jumpfiles)
     # ret$Source <- jumpfiles #EDIT
    #  ret
   # }
    
      if(!is.null(input$files)) { 
        inFile <- input$files
        jumpfiles <- inFile$datapath }
      else {
        inFile <- input$missing_rowsfiles
        jumpfiles <- inFile$datapath}
      
      #data_csv <- ldply(jumpfiles, read_csv)
      read_csv_filename <- function(jumpfiles){
        if(!is.null(input$files)) {
          ret <- read.csv(jumpfiles)
        }
        else{ret <- read.csv(jumpfiles, skip = input$mr - 1)}
        ret$Source <- jumpfiles #EDIT
        ret
      }
   
    ## combines all .csv files into 1 data frame
    data_csv <- ldply(jumpfiles, read_csv_filename)
    data_csv$trial <- as.numeric(as.factor(data_csv$Source))
    data_csv$trial <- paste("Trial", data_csv$trial, sep = " ")
    data_csv <- data_csv %>% filter(Time..s. > input$timefilter[1], Time..s. < input$timefilter[2])
    ### plot the data
    
    #import.list
    ggplot(data_csv, aes(Time..s., Combined..N., colour=trial))+
      geom_line()+
      theme_bw() 
      
    }
    
    else if(input$filter > 0 && input$corr == "yes" && input$correction >0) {
      #inFile <- input$files
    #jumpfiles <- inFile$datapath
    #data_csv <- ldply(jumpfiles, read_csv)
  #  read_csv_filename <- function(jumpfiles){
      #ret <- read.csv(jumpfiles)
     # ret$Source <- jumpfiles #EDIT
    #  ret
   # }
    
      if(!is.null(input$files)) { 
        inFile <- input$files
        jumpfiles <- inFile$datapath }
      else {
        inFile <- input$missing_rowsfiles
        jumpfiles <- inFile$datapath}
      
      #data_csv <- ldply(jumpfiles, read_csv)
      read_csv_filename <- function(jumpfiles){
        if(!is.null(input$files)) {
          ret <- read.csv(jumpfiles)
        }
        else{ret <- read.csv(jumpfiles, skip = input$mr - 1)}
        ret$Source <- jumpfiles #EDIT
        ret
      }
   
    `%ni%` <- Negate(`%in%`)
    ## combines all .csv files into 1 data frame
    data_csv <- ldply(jumpfiles, read_csv_filename)
    data_csv$trial <- as.numeric(as.factor(data_csv$Source))
    data_csv$trial <- paste("Trial", data_csv$trial, sep = " ")
    data_csv <- data_csv %>% filter(Time..s. > input$timefilter[1], Time..s. < input$timefilter[2])
    data_csv2 <- data_csv %>% filter(trial %ni% input$tc)
    data_csv3 <- data_csv %>% filter(trial %in% input$tc) %>% mutate(Time..s. = Time..s. - input$sec)
    data_csv4 <- rbind(as.data.frame(data_csv2), as.data.frame(data_csv3))
    ### plot the data
    
    #import.list
    ggplot(data_csv4, aes(Time..s., Combined..N., colour=trial))+
      geom_line()+
      theme_bw() 
    
    }
  })
  library(DT)
  library(readr)
  output$contents2 <- renderDataTable({
    
    validate(
      need(input$runplot > 0, "Import Files and Click Plot Trials"
    ))
    #inFile <- input$files
    #jumpfiles <- inFile$datapath
    #data_csv <- ldply(jumpfiles, read_csv)
    #read_csv_filename <- function(jumpfiles){
    #  ret <- read.csv(jumpfiles)
   #   ret$Source <- jumpfiles #EDIT
  #    ret
 #   }
    
    if(!is.null(input$files)) { 
      inFile <- input$files
      jumpfiles <- inFile$datapath }
    else {
      inFile <- input$missing_rowsfiles
      jumpfiles <- inFile$datapath}
    
    #data_csv <- ldply(jumpfiles, read_csv)
    read_csv_filename <- function(jumpfiles){
      if(!is.null(input$files)) {
        ret <- read.csv(jumpfiles)
      }
      else{ret <- read.csv(jumpfiles, skip = input$mr - 1)}
      ret$Source <- jumpfiles #EDIT
      ret
    }
  
    ## combines all .csv files into 1 data frame
    data_csv <- ldply(jumpfiles, read_csv_filename)
    data_csv$trial <- as.numeric(as.factor(data_csv$Source))
    data_csv2 <- data_csv[,c(1:4,6)]
   datatable(data_csv2)
  })
  
  output$contents3 <- renderPlot({ 
    
    validate(
      need(input$runplot > 0, "Import Files and Click Plot Trials"
      ))
    if(input$filter == 0 && input$limbs == 0) {
    #inFile <- input$files
    #jumpfiles <- inFile$datapath
    #data_csv <- ldply(jumpfiles, read_csv)
    #read_csv_filename <- function(jumpfiles){
     # ret <- read.csv(jumpfiles)
    #  ret$Source <- jumpfiles #EDIT
   #   ret
  #  }
    
      if(!is.null(input$files)) { 
        inFile <- input$files
        jumpfiles <- inFile$datapath }
      else {
        inFile <- input$missing_rowsfiles
        jumpfiles <- inFile$datapath}
      
      #data_csv <- ldply(jumpfiles, read_csv)
      read_csv_filename <- function(jumpfiles){
        if(!is.null(input$files)) {
          ret <- read.csv(jumpfiles)
        }
        else{ret <- read.csv(jumpfiles, skip = input$mr - 1)}
        ret$Source <- jumpfiles #EDIT
        ret
      }
   
    ## combines all .csv files into 1 data frame
    data_csv <- ldply(jumpfiles, read_csv_filename)
    data_csv$trial <- as.numeric(as.factor(data_csv$Source))
    maxline <- max(data_csv$Combined..N.[which(data_csv$Time..s. > 1.5 & data_csv$Time..s < 2.5)])
    
    ### plot the data
    
    #import.list
    data_csv$trial <- paste("Trial", data_csv$trial, sep = " ")
    ggplot(data_csv, aes(Time..s., Combined..N., colour=trial))+
      geom_line()+
      geom_hline(yintercept = maxline, colour="darkgreen",  lty=2)+
      annotate(geom= "text", x=2,y=4000,label="Max force of trials", colour="darkgreen")+
      theme_bw()+
      facet_wrap(~trial)
  }
  else if(input$filter > 0 && input$limbs == 0) { 
    #inFile <- input$files
   # jumpfiles <- inFile$datapath
    #data_csv <- ldply(jumpfiles, read_csv)
  #  read_csv_filename <- function(jumpfiles){
      #ret <- read.csv(jumpfiles)
     # ret$Source <- jumpfiles #EDIT
    #  ret
   # }
    
    if(!is.null(input$files)) { 
      inFile <- input$files
      jumpfiles <- inFile$datapath }
    else {
      inFile <- input$missing_rowsfiles
      jumpfiles <- inFile$datapath}
    
    #data_csv <- ldply(jumpfiles, read_csv)
    read_csv_filename <- function(jumpfiles){
      if(!is.null(input$files)) {
        ret <- read.csv(jumpfiles)
      }
      else{ret <- read.csv(jumpfiles, skip = input$mr - 1)}
      ret$Source <- jumpfiles #EDIT
      ret
    }
    
    ## combines all .csv files into 1 data frame
    data_csv <- ldply(jumpfiles, read_csv_filename)
    data_csv$trial <- as.numeric(as.factor(data_csv$Source))
    data_csv$trial <- paste("Trial", data_csv$trial, sep = " ")
    data_csv <- data_csv %>% filter(Time..s. > input$timefilter[1], Time..s. < input$timefilter[2])
    maxline <- max(data_csv$Combined..N.[which(data_csv$Time..s. > 1.5 & data_csv$Time..s < 2.5)])
    ### plot the data
    
    ggplot(data_csv, aes(Time..s., Combined..N., colour=trial))+
      geom_line()+
      geom_hline(yintercept = maxline, colour="darkgreen", lty=2)+
      annotate(geom= "text", x=2,y=4000,label="Max force of trials", colour="darkgreen")+
      theme_bw()+
      facet_wrap(~trial) }
    
   else if(input$limbs >0) {
      
      #data_csv <- ldply(jumpfiles, read_csv)
      #read_csv_filename <- function(jumpfiles){
        #ret <- read.csv(jumpfiles)
        #ret$Source <- jumpfiles #EDIT
       # ret
      #}
     if(!is.null(input$files)) { 
     inFile <- input$files
     jumpfiles <- inFile$datapath }
     else {
        inFile <- input$missing_rowsfiles
        jumpfiles <- inFile$datapath}
        
        #data_csv <- ldply(jumpfiles, read_csv)
        read_csv_filename <- function(jumpfiles){
          if(!is.null(input$files)) {
            ret <- read.csv(jumpfiles)
          }
          else{ret <- read.csv(jumpfiles, skip = input$mr - 1)}
          ret$Source <- jumpfiles #EDIT
          ret
        }
     
      ## combines all .csv files into 1 data frame
      data_csv <- ldply(jumpfiles, read_csv_filename)
      data_csv$trial <- as.numeric(as.factor(data_csv$Source))
      data_csv$trial <- paste("Trial", data_csv$trial, sep = " ")
      data_csv <- data_csv %>% filter(Time..s. > input$timefilter[1], Time..s. < input$timefilter[2])
      maxline <- max(data_csv$Combined..N.[which(data_csv$Time..s. > 1.5 & data_csv$Time..s < 2.5)])
      ### plot the data
      
      ggplot()+
        geom_line(data=data_csv, aes(Time..s., Combined..N., colour=trial))+
        geom_line(data=data_csv, aes(Time..s., Left..N.), colour="blue", lty=2)+
        geom_line(data=data_csv, aes(Time..s., Right..N.), colour="orange", lty=2)+
        geom_hline(yintercept = maxline, colour="darkgreen", lty=2)+
        annotate(geom= "text", x=2,y=4000,label="Max force of trials", colour="darkgreen")+
        theme_bw()+
        facet_wrap(~trial)
      
    }
  })
  
  #output$contents <- renderTable({
   #req(input$files)
    #upload = list()
    
    #for(nr in 1:length(input$files[, 1])){
     #upload[[nr]] <- read.csv(
      # file = input$files[[nr, 'datapath']]
      #)
    #}
    
    #return(upload)
    
  #}#)
}

shinyApp(ui, server)