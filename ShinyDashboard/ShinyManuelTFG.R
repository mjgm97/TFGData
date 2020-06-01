library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(reshape2)
library(png)
library(grid)
library(magick)
library(extrafont)
library(jsonlite)
library(shinythemes)

##library(rsconnect)
##rsconnect::deployApp('path/to/your/app')

datosFunnelPuzzle <- read.csv("datosCSV/pruebaBinarioV2.csv", header = TRUE)
total_puzzles <- 30
datosFunnelUser <- read.csv("datosCSV/funnelOutput.csv", header = TRUE)
datosFunnelPuzzle$level_puzzle <- factor(datosFunnelPuzzle$level_puzzle, levels = c("Basic Puzzles", "Intermediate Puzzles","Advanced Puzzles"))
datosFunnelPuzzle<-datosFunnelPuzzle%>% filter(level_puzzle != "SAND")

dfSequence <- read.csv("datosCSV/sequenceOutput.csv", header = TRUE)
datosDiff <- read.csv("datosCSV/levelsOfDifficultyOutput.csv", header = TRUE)
seqWithinDf <- read.csv("datosCSV/seqWPOutput.csv", header = TRUE)
commonErrorsDf <- read.csv("datosCSV/commonOutput.csv", header = TRUE)
df_melted <- NULL
currentSeq<- NULL

#Controls event click on funnel plot
event<- 0
#Controls student key to change plot
k <- NULL
kBetweenPuzzlesTask <- NULL

#Controls event click on sequenceBP plot
eventBetweenPuzzles<- 0
#Controls student key to change plot
kBetweenPuzzles <- NULL

seqWithinDf <- seqWithinDf %>% mutate(task_id = paste(task_id,  paste("-Attempt", n_attempt), sep = ""))

seqWithinDf$type <- factor(seqWithinDf$type, levels = c("ws-check_solution", "ws-create_shape", "ws-delete_shape", "ws-move_shape", "ws-rotate_shape", "ws-rotate_view", "ws-scale_shape", "ws-snapshot", "ws-start_level"), 
                           labels = c("submit", "create", "delete", "move", "rotate", "rotate_view", "scale", "snapshot", "start_level"))

seqWithinDf$shape_type <- factor(seqWithinDf$shape_type, levels = c("-", "1", "2", "3", "4", "5", "6"), 
                                 labels = c("-", "cube", "pyramid", "prism", "cylinder", "cone", "sphere"))



common_errors <- image_read("images/common_errors.png")
master_sol_img <- image_read("images/master_sol.png")
no_data <- image_read("images/nodata.png")
sphere <- image_read("images/shape_sphere.png")
cone <- image_read("images/shape_cone.png")
cube <- image_read("images/shape_cube.png")
ramp <- image_read("images/shape_prism.png")
pyramid <- image_read("images/shape_pyramid.png")
cylinder <- image_read("images/shape_cylinder.png")

create_sphere <- image_read("images/add_sphere.png")
create_cone <- image_read("images/add_cone.png")
create_cube <- image_read("images/add_cube.png")
create_cylinder <- image_read("images/add_cylinder.png")
create_prism <- image_read("images/add_prism.png")
create_pyramid <- image_read("images/add_pyramid.png")

delete_sphere <- image_read("images/delete_sphere.png")
delete_cone <- image_read("images/delete_cone.png")
delete_cube <- image_read("images/delete_cube.png")
delete_cylinder <- image_read("images/delete_cylinder.png")
delete_prism <- image_read("images/delete_prism.png")
delete_pyramid <- image_read("images/delete_pyramid.png")

scale_sphere <- image_read("images/scale_sphere.png")
scale_cone <- image_read("images/scale_cone.png")
scale_cube <- image_read("images/scale_cube.png")
scale_cylinder <- image_read("images/scale_cylinder.png")
scale_prism <- image_read("images/scale_prism.png")
scale_pyramid <- image_read("images/scale_pyramid.png")

move_sphere <- image_read("images/move_sphere.png")
move_cone <- image_read("images/move_cone.png")
move_cube <- image_read("images/move_cube.png")
move_cylinder <- image_read("images/move_cylinder.png")
move_prism <- image_read("images/move_prism.png")
move_pyramid <- image_read("images/move_pyramid.png")

rotate_sphere <- image_read("images/rotate_sphere.png")
rotate_cone <- image_read("images/rotate_cone.png")
rotate_cube <- image_read("images/rotate_cube.png")
rotate_cylinder <- image_read("images/rotate_cylinder.png")
rotate_prism <- image_read("images/rotate_prism.png")
rotate_pyramid <- image_read("images/rotate_pyramid.png")

submit_correct <- image_read("images/submit_correct.png")
submit_incorrect <- image_read("images/submit_incorrect.png")
snapshot <- image_read("images/snapshot.png")
rotate_view <- image_read("images/rotate_view.png")
pizarra <- image_read("images/pizarra.png")

userSelected <- "86eafa4b05f579291048d9f8e4e715fd"


dashboardSid<-dashboardSidebar(
  sidebarMenu(id = "menu",
              menuItem("Funnel", tabName = "dashboardFunnel", icon = icon("chart-pie")),
              menuItem("Sequence between Puzzles", tabName = "sequenceBP", icon = icon("ellipsis-h")),
              menuItem("Sequence within Puzzles", tabName = "sequenceWP", icon = icon("chalkboard")),
              menuItem("Common Errors", tabName = "common_err", icon = icon("times")),
              menuItem("About us", tabName = "personalInfo", icon = icon("users"))
  )
) 

colTab <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #9370DB;
}"'

body <- dashboardBody(tags$style(colTab),
  tabItems(
    tabItem(tabName = "dashboardFunnel",
            fluidRow(
              tabBox(
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",
                tabPanel("Funnel by Puzzle",
                         box(
                           selectizeInput(inputId = "groupPuzzle", label = "Choose a group",
                                          choices = unique(datosFunnelPuzzle$group), options = list(
                                            placeholder = 'Please select a group',
                                            onInitialize = I('function() { this.setValue(""); }')
                                          ),width = 1000), width = 1000,height = 100, offset = 5),box(plotlyOutput("funnelPuzzle", height = 900, width = 1000), width = 12, height = 950), width = 1000, height = 150,offset = 5),
                
                tabPanel("Funnel by User",
                         box(
                           selectizeInput(inputId = "groupUser", label = "Choose a group",
                                          choices = unique(datosFunnelUser$group), options = list(
                                            placeholder = 'Please select a group',
                                            onInitialize = I('function() { this.setValue(""); }')
                                          ), selected = "8cbfa61cb2b025b16b04a8e470422960",width = 1000),width = 1000,height = 100, offset = 5), box(plotlyOutput("funnelUser", height = 900, width = 1000), width = 12, height = 950),width = 1000,height = 150,offset = 5), height = 150, width = 1000
              )
            )
    ),
    
    tabItem(tabName = "sequenceBP",
            fluidRow(column (box( selectizeInput(
              'groupDiff', "Choose a group - the group will be used to calculate the difficulty metric", unique(datosFunnelUser$group),
              options = list(
                placeholder = 'Please select a group',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )), width = 12, box(selectizeInput(inputId = "user", label = "Choose a user",
                                               choices = unique(dfSequence$user), options = list(
                                                 placeholder = 'Please select a user',
                                                 onInitialize = I('function() { this.setValue(""); }')
                                               )))), column (box(plotlyOutput("sequenceBetweenPuzzles", height = 500, width = 1100), width = 12, height = 550), width = 12))),
    tabItem(tabName = "sequenceWP",
            fluidRow(column (box(selectizeInput(inputId = "sequenceWPGroup", label = "Choose a group",
                                                choices = unique(seqWithinDf$group), options = list(
                                                  placeholder = 'Please select a group',
                                                  onInitialize = I('function() { this.setValue(""); }')
                                                ))), box(selectizeInput(inputId = "sequenceWPUser", label = "Choose a user",
                                                                        choices = unique(unique(seqWithinDf$user)), options = list(
                                                                          placeholder = 'Please select a user',
                                                                          onInitialize = I('function() { this.setValue(""); }')
                                                                        ))
                                                ), width = 12)), fluidRow(column(width = 12,
                                                                                 box(selectizeInput(inputId = "sequenceWPPuzzle", label = "Choose a puzzle",
                                                                                                    choices = unique(unique(seqWithinDf$task_id)), options = list(
                                                                                                      placeholder = 'Please select a puzzle',
                                                                                                      onInitialize = I('function() { this.setValue(""); }')
                                                                                                    )), width = 12)), width = 12),  fluidRow(column (plotOutput("sequenceWP", height = 800, width = 700), width = 12, align = "center")
                                                                                                    )
    )
    , tabItem(tabName = "common_err",
              fluidRow(column (box(selectizeInput(inputId = "common_errGroup", label = "Choose a group",
                                                  choices = unique(commonErrorsDf$group_id), options = list(
                                                    placeholder = 'Please select a group',
                                                    onInitialize = I('function() { this.setValue(""); }')
                                                  ))), width = 12, box(
                                                    selectizeInput(inputId = "common_errPuzzle", label = "Choose a puzzle",
                                                                   choices = unique(unique(commonErrorsDf$task_id)), options = list(
                                                                     placeholder = 'Please select a puzzle',
                                                                     onInitialize = I('function() { this.setValue(""); }')
                                                                   )))),  fluidRow(column (align = "center", plotOutput("common_errPlot", height = 800, width = 700), width = 12))
              )
    ), 
    tabItem(tabName = "personalInfo", titlePanel(h1(strong("Sobre nosotros"), align = "center", style = "font-family: 'times'; font-si16pt")),titlePanel(
      h4("Hola, mi nombre es Manuel Jesús Gómez Moratilla, y actualmente me encuentro finalizando mis estudios de grado en Ingeniería Informática en la Universidad de Murcia
        .\n Este dashboard ha sido creado como parte de mi Trabajo Fin de Grado, consistente en técnicas de data science para la evaluación de competencias en juegos. Para ello se ha usado información recopilada en un conjunto de institutos estadounidenses por el Playful Journey Lab de MIT, haciendo uso del juego Shadowspect.", align = "center")), titlePanel(h4("Este trabajo ha sido tutorizado y supervisado por Jose Antonio Ruipérez Valiente y Gregorio Martínez Pérez",align="center")), img(src = "shadowLogo.png", height = 80, width = 600, style="display: block; margin-left: auto; margin-right: auto;"), img(src = "yo2.jpg", height = 300, width = 200, style="display: block; margin-left: auto; margin-right: auto;"))
  )
)




ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Shadowspect Dashboard", titleWidth = 500),
  dashboardSid,
  body
  
)



server <- function(session,input, output) {
  
  observe({
    selectClass = datosFunnelUser %>% filter(group == input$groupDiff) %>% select(user)
    updateSelectInput(session, "user","Select a user",choices = selectClass, selected = k)
    
    if (nchar(input$groupUser) < 1) {k <<- NULL}
    else {
      e <<- event_data(
        event = "plotly_click",
        source = "F",
        session = session
      )
      st <- sort(unique(df_melted$user))[(e$curveNumber + 1)]
      df_ch <- df_melted[df_melted$user==st,]
      key <- unique(df_ch$userN)
      
      new_value <- ifelse(is.null(e),"0",(e$curveNumber+1)) # 0 if no selection
      if(event!=new_value) {
        event <<- new_value 
        if(event !=0) {
          updateTabItems(session, "menu",
                         selected = "sequenceBP")
          groupUpdate = unique(datosFunnelUser[datosFunnelUser$user==key,]$group)
          updateSelectizeInput(session, "groupDiff", selected = groupUpdate)
          selectClass = datosFunnelUser %>% filter(group == groupUpdate) %>% select(user)
          k <<- key
          updateSelectInput(session, "user","Select a user",choices = selectClass, selected = key)
          #updateSelectInput(session, "user", selected = key)
        }
      }
      
    }
  })
  
  observe({
    selectPuzzle = seqWithinDf %>% filter(group_id == input$sequenceWPGroup)  %>% filter (user == input$sequenceWPUser) %>% select(task_id)
    updateSelectInput(session, "sequenceWPPuzzle","Select a puzzle",choices = selectPuzzle, selected = kBetweenPuzzlesTask)
  })
  
  observe({
    selectClassSequence = seqWithinDf %>% filter(group_id == input$sequenceWPGroup) %>% select(user)
    updateSelectInput(session, "sequenceWPUser","Select a user",choices = selectClassSequence, selected = kBetweenPuzzles)
    #kBetweenPuzzles <<- NULL
    #kBetweenPuzzlesTask <<- NULL
    
    if (nchar(input$user) < 1) {}
    else {
      eS <<- event_data(
        event = "plotly_click",
        source = "S",
        session = session
      )
      new_value <- ifelse(is.null(eS),"0",(eS$pointNumber+1)) # 0 if no selection
      if(eventBetweenPuzzles!=new_value) {
        eventBetweenPuzzles <<- new_value
        if(eventBetweenPuzzles !=0) {
          gr = input$groupDiff
          us = input$user
          nAttempt = eS$x
          updateTabItems(session, "menu",
                         selected = "sequenceWP")
          updateSelectizeInput(session, "sequenceWPGroup", selected = gr)
          
          namePuzzle = unique(currentSeq[currentSeq$sequence == nAttempt,]$task_id)
          at = paste("-Attempt", nAttempt, sep = " ")
          pD <- paste(namePuzzle, at, sep="")
          selectClassSequence = seqWithinDf %>% filter(group_id == input$sequenceWPGroup) %>% select(user)
          selectPuzzle = seqWithinDf %>% filter(group_id == input$sequenceWPGroup)  %>% filter (user == input$sequenceWPUser) %>% select(task_id)
          kBetweenPuzzles <<- us
          kBetweenPuzzlesTask <<- pD
          updateSelectInput(session, "sequenceWPUser",choices = selectClassSequence, selected = us)
          updateSelectInput(session, "sequenceWPPuzzle", choices = selectPuzzle, selected = pD)
        }
      }
      
    }
  })
  
  observe({
    selectTaskId= commonErrorsDf %>% filter(group_id == input$common_errGroup)  %>% select(task_id)
    updateSelectInput(session, "common_errPuzzle","Select a puzzle",choices = selectTaskId)
  })
  
  
  output$funnelUser <- renderPlotly ({
    
    if (nchar(input$groupUser) < 1) {}
    else {
      df1 <- datosFunnelUser  %>% mutate(started = started/total_puzzles, create_shape = create_shape/total_puzzles, submitted = submitted/total_puzzles, completed = completed/total_puzzles) %>% filter(group == input$groupUser) %>% select(-task_id, -group)
      df1 <- df1 %>% mutate(userN = user)
      df_melted <<- df1 %>% melt(id.vars = c("user","userN"), measure.vars = c("started", "create_shape", "submitted", "completed"), variable.name = "funnel_state") %>% mutate(user = paste("Student",  as.numeric(user)))
      df_melted$funnel_state <<- factor(df_melted$funnel_state, levels = c("started","create_shape","submitted", "completed"), labels = c("Started","Shape Created","Submitted", "Completed"))
      df_melted <<- df_melted %>% mutate(percentage = round(value * 100),2)
      ax <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      cont = 0
      nr = as.integer(length(unique(df_melted$user))/6)
      p.list = lapply(sort(unique(df_melted$user)), function(i) {
        df_use <- df_melted[df_melted$user==i,]
        funnel <- plot_ly(source = "F") 
        funnel <- funnel %>%
          add_trace(df_use,
                    type = "funnel",
                    y =  df_use$funnel_state,
                    x =  df_use$percentage, texttemplate="%{x}%", hoverinfo = "y+percent initial+percent previous",
                    marker = list(color = c("steelblue", "yellow", "red", "lime"),
                                  line = list(width = c(1,1,1,1), color = c("steelblue", "yellow", "red", "lime"))),
                    connector = list(line = list(color = "royalblue", dash = "dot", width = 3)), name = unique(df_use$user))
        funnel <- funnel %>% add_annotations(
          text = ~unique(df_use$user),
          x = 0.5,
          y = 3.5,
          yref = "y",
          xref = "x",
          xanchor = "middle",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 15)
        ) 
        
        if (cont%%as.integer(((length(unique(df_melted$user))/nr) + 1)) != 0) {
          funnel <- funnel %>% layout(yaxis = ax)
        }
        cont <<- cont + 1
        
        funnel
        
      })
      subplot(p.list, nrows = nr) %>% layout(showlegend = FALSE)
    }
    
  })
  
  output$funnelPuzzle <- renderPlotly ({
    
    if (nchar(input$groupPuzzle) < 1) {}
    else {
      
      datosFunnelPuzzle <- datosFunnelPuzzle %>% filter(group==input$groupPuzzle)
      total_students <- length(unique(datosFunnelPuzzle$user))
      
      datosFunnelPuzzle <- datosFunnelPuzzle %>% group_by(task_id,level_puzzle) %>% summarise(started = sum(started), create_shape = sum(create_shape), submitted = sum(submitted), completed = sum(completed)) %>% 
        mutate(started = started/total_students, create_shape = create_shape/total_students, submitted = submitted/total_students, completed = completed/total_students) # %>% filter(level_puzzle==input$level_puzzle)
      datos_melted <- datosFunnelPuzzle %>% melt(id.vars = c("task_id","level_puzzle"), measure.vars = c("started", "create_shape", "submitted", "completed"), value.name = "percentage", variable.name = "funnel_state")
      datos_melted$funnel_state <- factor(datos_melted$funnel_state, levels = c("started","create_shape","submitted", "completed"), labels = c("Started","Shape Created","Submitted", "Completed"))
      datos_melted <- datos_melted %>% mutate(percentage = round(percentage * 100),2)
      ax <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      cont1 = 0
      if (length(unique(datos_melted$task_id)) == 30) {
        nr2 = 5
      } else {
        nr2 = 4
      }
      p.list = lapply(sort(unique(datos_melted$task_id)), function(i) {
        df_use <- datos_melted[datos_melted$task_id==i,]
        funnel <- plot_ly() 
        funnel <- funnel %>%
          add_trace(df_use,
                    type = "funnel",
                    y =  df_use$funnel_state,
                    x =  df_use$percentage, texttemplate="%{x}%", hoverinfo = "y+percent initial+percent previous",
                    marker = list(color = c("steelblue", "yellow", "red", "lime"),
                                  line = list(width = c(1,1,1,1), color = c("steelblue", "yellow", "red", "lime"))),
                    connector = list(line = list(color = "royalblue", dash = "dot", width = 3)), name = unique(df_use$task_id))
        funnel <- funnel %>% add_annotations(
          text = ~unique(df_use$task_id),
          x = 0.5,
          y = 3.5,
          yref = "y",
          xref = "x",
          xanchor = "middle",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 9)
        ) 
        if (nr2 == 5) {
          md = 6
        }else {
          md = 7
        }
        if (cont1%%md!= 0) {
          funnel <- funnel %>% layout(yaxis = ax)
        }
        cont1 <<- cont1 + 1
        
        funnel
        
      })
      
      subplot(p.list, nrows = nr2) %>% layout(showlegend = FALSE)
    }
  })

  output$sequenceBetweenPuzzles <- renderPlotly ({
    if (is.null(input$user)) {}
    else {
      if(nchar(input$user)>1)
      {
        
        currentSeq <<- dfSequence %>% filter(user==input$user) %>% select(-session_id) %>% mutate(user = paste("Student",  as.numeric(user)))
        
        currentSeq$funnel <<- factor(currentSeq$funnel, levels = c("completed", "submitted", "shape_created", "started"), 
                                     labels = c("Completed", "Submitted", "Shape created", "Started"))
        
        dfDifficulty1 <- datosDiff %>% filter(group == input$groupDiff) 
        datosDiff <- dfDifficulty1 %>% select(-order, -completed_time, -actions_completed, -p_incorrect, -p_abandoned, -group, -X)
        
        currentSeq <<- merge(currentSeq, datosDiff, by.x = "task_id", by.y = "task_id")
        
        dotSize = 4
        long = length(currentSeq$sequence)
        if (long < 70 && long >=55) {
          dotSize = 3
        }else if (long < 40 && long >=30) {
          dotSize = 5
        } else if (long < 30  && long >=20) {
          dotSize = 6
        } else if (long < 20  && long >=10) {
          dotSize = 7
        } else if (long < 10) {
          dotSize = 8
        }
        
        coloresVis <- c("Completed" = "green", "Submitted" = "red", "Shape created" = "yellow", "Started" = "steelblue4")
        diff <- ggplot(currentSeq, aes(x = sequence, y = norm_all_measures,  label = task_id, label2 = funnel), fill = task_id) + geom_point(aes(colour = factor(funnel)), size = dotSize) + scale_color_manual(values=coloresVis) +
          scale_y_continuous(limit = c(-0.5,1.5)) + scale_x_continuous(minor_breaks = seq(1, length(currentSeq$sequence) + 5, 1)) + labs(title ="Sequence within Puzzles", x = "Sequence", y = "Difficulty") +
          theme_minimal() + theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5)) + labs(color='Funnel') 
        
        ggplotly(diff, tooltip = c("x", "label","label2"), source = "S")
      }   
    }
  })
  
  output$sequenceWP <- renderPlot ({
    if (nchar(input$sequenceWPPuzzle) < 1) {}
    else {
      currentPuzzle <<- seqWithinDf %>% filter(group_id == input$sequenceWPGroup) %>% filter(user == input$sequenceWPUser) %>% filter(task_id == input$sequenceWPPuzzle)
      
      #Vemos cuantas filas tiene para poder escalar en consecuencia
      n = 0
      filas = 0
      for (row in 1:nrow(currentPuzzle)) {
        n = n + 1
        if (n == 10 || currentPuzzle$type[row] == "submit") {
          filas = filas + 1
          n = 0
        }
      }
      
      if (length(unique(currentPuzzle$type)) == 1 && unique(currentPuzzle$type) == 'start_level') {
        plot(1:3, type='n', axes = FALSE, xlab = "", ylab = "", main = currentPuzzle$task_id[1], font.main=4, cex.main=1)
        rasterImage(pizarra, 0.5, 0, 3.3, 3.5)
        rasterImage(no_data, 1.5, 2.9, 2.5, 2.70)
      } else {
        
        if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
          plot(1:3, type='n', axes = FALSE, xlab = "", ylab = "", main = currentPuzzle$task_id[1], font.main=4, cex.main=1)
          if (filas > 9) {
            offsetX = 0.1
            offsetY = 0.075
            xI = 1
            yI = 2.9
            limitColumn = 20
          } else if (filas >= 3) { 
            offsetX = 0.2
            offsetY = 0.15
            xI = 1
            yI = 2.9
            limitColumn = 10
          } else {
            offsetX = 0.4
            offsetY = 0.3
            xI = 1
            yI = 2.9
            limitColumn = 5
          }
          contador = 0
          rasterImage(pizarra, 0.5, 0, 3.3, 3.5)
          
          for (row in 1:nrow(currentPuzzle)) {
            if (contador == limitColumn) {
              yI = yI - offsetY
              xI = 1
              contador = 0
            }
            
            if (currentPuzzle$type[row] == "start_level") {} 
            else {
              if (currentPuzzle$type[row] == "rotate_view") {
                img_use <- rotate_view
              } else if (currentPuzzle$type[row] == "submit") {
                if (currentPuzzle$correct[row] == "True") {
                  
                  img_use <- submit_correct
                } else {
                  img_use <- submit_incorrect
                }
                
              } else if (currentPuzzle$type[row] == "snapshot") {
                
                img_use <- snapshot
                
              } else {
                stringAction <- currentPuzzle$type[row]
                figure <- currentPuzzle$shape_type[row]
                nameString <- paste(stringAction, figure, sep = "_")
                img_use <- eval(as.symbol(nameString))
                img_use <- image_annotate(img_use, currentPuzzle$shape_id[row], size = 175, gravity = "south", color = "white")
              }
              
              if (currentPuzzle$n_times[row] > 1){
                if (currentPuzzle$type[row] == "rotate_view" || currentPuzzle$type[row] == "submit" || currentPuzzle$type[row] == "snapshot"){ 
                  imgAnnotated <- image_annotate(img_use, paste( "x", currentPuzzle$n_times[row], sep= ""), size = 85, gravity = "northeast", color = "white")
                } else {
                  imgAnnotated <- image_annotate(img_use, paste( "x", currentPuzzle$n_times[row], sep= ""), size = 200, gravity = "northeast", color = "white")
                }
              } else {
                imgAnnotated <- img_use
              }
              
              #Para publicarlo hacerle el flip
              #imgAnnotated <- image_flip(imgAnnotated)
              rasterImage(imgAnnotated, xI, yI, (xI+offsetX), yI - offsetY)
              
              
              
              xI = xI + offsetX
              contador = contador + 1
              
              if (currentPuzzle$type[row] == "submit") {
                if  (currentPuzzle$correct[row] == "False") {
                  xI = 1
                  yI = yI - (offsetY + 0.05)
                  contador = 0
                }
              }
            }
          }
        }
      }
    }
    
  })
  
  output$common_errPlot <- renderPlot({
    if (nchar(input$common_errPuzzle) < 1) {}
    else {
      currentGroupPuzzle <- commonErrorsDf %>% filter(group_id == input$common_errGroup) %>% filter(task_id == input$common_errPuzzle) 
      
      if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
        plot(1:3, type='n', axes = FALSE, xlab = "", ylab = "", main = currentGroupPuzzle$task_id, sub = currentGroupPuzzle$group_id, font.main=4, cex.main=1)
        
        contador = 0
        rasterImage(pizarra, 0.5, 0, 3.3, 3.5)
        rasterImage(master_sol_img, 1.5, 2.9, 2.5, 2.70)
        currentGroupPuzzle$master_solution <- gsub("'", "\"", currentGroupPuzzle$master_solution)
        # parse the json
        mas_sol <- fromJSON(currentGroupPuzzle$master_solution)
        offsetX = 0.2
        offsetY = 0.15
        xI = 1
        yI = 2.65
        for(i in names(mas_sol)) {
          if (contador == 10) {
            yI = yI - offsetY
            xI = 1
          }
          shape <- mas_sol[[i]]
          imgsh <- eval(as.symbol(shape))
          #imgsh <- image_flip(imgsh)
          rasterImage(imgsh, xI, yI, (xI+offsetX), yI - offsetY, interpolate=FALSE)
          xI = xI + offsetX
          contador = contador + 1
        }
        rasterImage(common_errors, 1.5, yI - (offsetY*1.5), 2.5, (yI - (offsetY*3.1)))
        
        currentGroupPuzzle$common_Errors <- gsub("'", "\"", currentGroupPuzzle$common_Errors)
        # parse the json
        commonE <- fromJSON(currentGroupPuzzle$common_Errors)
        offsetX = 0.4
        offsetY = 0.3
        xI = 1
        yI = yI - (offsetY*1.75)
        contador = 0
        for(i in names(commonE)) {
          mov <- i
          if (mov == "moved_shapes") {
            mov <- "move"
          } else if (mov == "created_shapes") {
            mov <- "create"
          }  else if (mov == "deleted_shapes") {
            mov <- "delete"
          }  else if (mov == "scaled_shapes") {
            mov <- "scale"
          }  else if (mov == "rotated_shapes") {
            mov <- "rotate"
          }
          
          for (j in names(commonE[[i]])) {
            if (contador == 5) {
              yI = yI - offsetY
              xI = 1
            }
            sh <- j
            if (sh == "ramp") {
              sh = "prism"
            }
            comp_name <- paste(mov, sh, sep = "_")
            img_used <- eval(as.symbol(comp_name))
            img_used <- image_annotate(img_used, paste(commonE[[i]][[j]], "%", sep = ""), size = 150, gravity = "south", color = "white")
            #img_used <- image_flip(img_used)
            rasterImage(img_used, xI, yI, (xI+offsetX), yI - offsetY, interpolate=FALSE)
            xI = xI + offsetX
            contador = contador + 1
          }
        }
      }
      
    }
  })
  
  
  
}

shinyApp(ui, server)


