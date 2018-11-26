
source('Functions/load_libraries_functions.R')

Fault_name_choices <- gsub(pattern = "_profile\\.RDS$", "", list.files('Profiles/',pattern='profile'))

index_p <- 1

ui <- navbarPage('Save points along Profile',
                 tabPanel('PROFILES', 
                          useShinyalert(),
                          actionButton("Instructions", "Instructions"),
                          titlePanel(textOutput("ProfileName")) ,
                          sidebarPanel(width = 3,
                                       selectInput(inputId = 'FaultName',                                                    
                                                   label = 'Fault Name',        
                                                   choices = Fault_name_choices
                                       ),#end of select fault name
                                       
                                       numericInput(inputId = 'spacing',                                                    
                                                    label = 'Profile Spacing (meters)',        
                                                    value = 100
                                       ),#end of select profile spacing
                                       # Horizontal line ----
                                       tags$hr(),
                                       
                                       uiOutput("profile_number_choices"),
                                       actionButton("save_button", "Save table"),
                                       # Horizontal line ----
                                       p(),
                                       actionButton("start",HTML("Reset Profile</br>number"))
                                       
                          ), #end of sidebarPanel
                          
                          mainPanel(width=9,
                                    plotlyOutput("plot"),
                                    tableOutput("dataTable")
                          ) # end of mainPanel for map 
                 ), # end of tab panel for profiles
                 tabPanel('CREST and BASE', 
                          
                          sidebarPanel(width = 3,
                                       selectInput(inputId = 'FaultName2',
                                                   label = 'Fault Name',
                                                   choices = Fault_name_choices
                                       ),#end of select fault name
                                       selectInput(inputId = 'profile_half_width',
                                                   label = 'Profile halfwidth',
                                                   choices = '200'
                                       ),#end of select fault name
                                       sliderInput(inputId = 'SlopeRange',
                                                   label = 'Slope Range',
                                                   step=0.01,min=0, max=0.1,value=c(0.04,0.06)
                                       ),#end of slope range
                                       numericInput(inputId = 'SlopeStep',
                                                    label = 'Slope Step',
                                                    value=0.01
                                       ),#end of slope step
                                       sliderInput(inputId = 'CurvatureRange',
                                                   label = 'Curvature Range',
                                                   step=0.001,min=0.001, max=0.02,value=c(0.005,0.007)
                                       ),#end of curvature range
                                       numericInput(inputId = 'CurvatureStep',
                                                    label = 'Curvature Step',
                                                    value=0.001
                                       ),#end of curvature step
                                       sliderInput(inputId = 'SteepInRowRange',
                                                   label = 'Steep in row Range',
                                                   min=1, max=20,value=c(2,3)
                                       ),#end of steep in row range
                                       numericInput(inputId = 'SteepInRowStep',
                                                    label = 'Steep in row Step',
                                                    value=1
                                       ),# end of steep in row step
                                       actionButton("GO", "GO")
                          ),  # end of sidebar panel
                          mainPanel(
                            plotOutput("comparison")
                          ) # end of mainPanel for map
                 )  # end of tab panel for crest and base
) #end of navbarPage
save_points <- vector()






server <- function(input, output, session) {
  
  observeEvent(input$Instructions, {
    shinyalert(title = "Instructions",
               html = TRUE,
               text = 
                 HTML(
                   '<p style="text-align:justify">1. Select the fault name and the 
                   spacing of the elevation profiles along the fault.<br />
                   2. Click twice on the profile - for the base and crest of the scarp. <br />
                   3. Select the next profile number and repeat.  <br />
                   4. Then you can download the table with crest and base locations.  <br />
                   5. Then choose a new fault and do the same. 
                   </p>
                   ') 
                 )})
  
  prof_list <- reactive({
    # prof_list <- readRDS(paste0('../Scandinavia/Profiles/',input$FaultName,'_profile.RDS'))
    prof_list <- readRDS(paste0('Profiles/',input$FaultName,'_profile.RDS'))
  })
  ind_check_total <- reactive({seq(1,length(prof_list()),input$spacing)}) 
  profile_choices <- reactive({seq(1,length(ind_check_total()),1)})
  
  output$profile_number_choices <- renderUI(
    radioButtons("profile_number", 
                 HTML("Profile Number <br /> (not clickable)"),
                 profile_choices(),
                 selected = index_p2(),inline = TRUE
    )) # end of radioButton 
  
  inde<-eventReactive(event_data("plotly_click"),{
    ind_check_total()[as.numeric(input$profile_number)]
  })
  
  index_p <- reactiveVal(1)       # rv <- reactiveValues(value = 0)
  index_p2 <- reactiveVal(1)
  
  observeEvent(input$start,{  # was making it reset when fault rechosen...
    newValue <- index_p() - index_p() +1       # rv <- reactiveValues(value = 0)
    newValue2 <- index_p2() - index_p2() +1 
    index_p(newValue) 
    index_p2(newValue2) 
  })
  
  observeEvent(event_data("plotly_click"),{
    newValue <- index_p() + 0.5     # newValue <- rv$value - 1
    index_p(newValue)             # rv$value <- newValue
    if (index_p()%%1 == 0){
      newValue <- index_p()
      index_p2(newValue) 
    }
  })
  
  
  observeEvent(event_data("plotly_click"), {
    if (index_p2() > max(profile_choices())){
      shinyalert("NOW CLICK SAVE TABLE!", "Save table, reset profile number and choose a different fault or profile spacing", type = "success")
    }
  })
  
  
  ## PLOT 
  output$plot <- renderPlotly({
    if(index_p2() <= max(profile_choices())){
      # index_p <- as.numeric(ind_check_total()[as.numeric(input$profile_number)])
      profi <- prof_list()
      d <- highlight_key(profi[[ind_check_total()[floor(index_p2())]]], ~DistTotal)
      plot_ly(data = d, x=~DistTotal, y = ~height,hoverinfo="none",
              type='scatter',mode='markers',
              marker = list(size = 5,
                            color = 'rgba(255, 182, 193, .9)',
                            line = list(color = 'rgba(152, 0, 0, .8)',
                                        width = 2))) %>% 
        highlight(on="plotly_click",off=NULL, persistent=TRUE) %>%
        layout(title=paste0('Dist along fault: ',ind_check_total()[floor(index_p2())],' m' ),
               xaxis = list(
                 title = 'Distance along profile, m'),
               yaxis =  list(
                 title = 'Elevation, m'))
    }
  })
  
  
  
  # Create DATA TABLE
  inde<-eventReactive(event_data("plotly_click"),{
    ind_check_total()[as.numeric(input$profile_number)]
  })
  prof_num <- eventReactive(event_data("plotly_click"),{
    as.numeric(input$profile_number)
  })
  faultname<-eventReactive(event_data("plotly_click"),{
    input$FaultName
  })
  
  output$dataTable <- renderTable({
    d <- data.frame(event_data("plotly_click")[2:4],
                    prof_num=prof_num(),
                    prof_ind=inde(),
                    fault_name=faultname())
    if (is.null(d)) "Click events appear here (double-click to clear)" else
      save_points <<- rbind(d,save_points)
    save_points <- data.frame(save_points)
    save_points <- subset(save_points, fault_name==faultname())
  })
  
  
  # SAVE DATA TABLE
  observeEvent(input$save_button, {
    save_points <- subset(save_points, fault_name==faultname())
    b <- save_points[seq(1,nrow(save_points),2),c(6,4,5,1,2,3)]
    c <- save_points[seq(2,nrow(save_points),2),1:3]
    names(c) <- c('pointNumber2','x2','y2')
    save_points <- cbind(b,c)
    saveRDS(arrange(save_points,prof_ind),paste0('Manual_crba/',
                                                 input$FaultName,'_spacing_',
                                                 input$spacing,
                                                 '.RDS') )
  })
  
  
  
  
  }
shinyApp(ui, server)

#a <- readRDS('Ruokojarvi_a_spacing_250.RDS')

