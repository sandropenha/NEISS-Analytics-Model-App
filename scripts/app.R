## packages
pacman::p_load(tidyverse,data.table,shiny,shinydashboard,plotly,shinythemes,shinycssloaders)


## data
forecast <- fread("data/processed/forecast.csv") |> 
  mutate(year = year(date)) |>
  filter(year != 2018) |>
  select(-year)
df <- fread("data/processed/neiss_dataset.csv") |>
  mutate(date = as.Date(Treatment_Date,format="%m/%d/%Y"),
         year = year(date)) |> 
  filter(year != 2018)




real19 <- df |>
  dplyr::group_by(year) |> 
  dplyr::summarise(n=n()) |>
  filter(year == 2019) |> 
  select(n) |> 
  mutate(n=as.character(n)) |> 
  mutate(n = stringr::str_c(stringr::str_sub(n,start=1,end = 3),
                            ".",
                            stringr::str_sub(n,start = 4,end=6))) |> 
  as.character()


real20 <- df |>
  dplyr::group_by(year) |> 
  dplyr::summarise(n=n()) |>
  filter(year == 2020) |> 
  select(n) |> 
  mutate(n=as.character(n)) |> 
  mutate(n = stringr::str_c(stringr::str_sub(n,start=1,end = 3),
                            ".",
                            stringr::str_sub(n,start = 4,end=6))) |> 
  as.character()


real21 <- df |>
  dplyr::group_by(year) |> 
  dplyr::summarise(n=n()) |>
  filter(year == 2021) |> 
  select(n) |> 
  mutate(n=as.character(n)) |> 
  mutate(n = stringr::str_c(stringr::str_sub(n,start=1,end = 3),
                            ".",
                            stringr::str_sub(n,start = 4,end=6))) |> 
  as.character()


pred22 <- forecast |> 
  mutate(year=year(date)) |> 
  filter(year == 2022) |> 
  group_by(year) |> 
  dplyr::summarise(n=sum(forecast)) |> 
  select(n) |> 
  mutate(n=as.character(n)) |> 
  mutate(n = stringr::str_c(stringr::str_sub(n,start=1,end=3),
                            ".",
                            stringr::str_sub(n,start=4,end=6))) |> 
  as.character()





df <- df |> mutate(Weight = str_replace(Weight,pattern=",",replacement="\\.")) |> 
  mutate(Weight =as.numeric(Weight))

df <- df |> filter(Weight>70)


population <- fread("data/raw/population.tsv") |> dplyr::rename(Age = age,Sex = sex)

df <- df |> 
  left_join(population |> mutate_if(is.character,str_to_title),by = c("Sex","Age"))

df <- df |> select(-V1,-Race,-Alcohol,-Drug,-Narrative,-Weight,-Disposition,-Treatment_Date)

df <- df |> arrange(Product_1) |> na.omit()

years_select <- df |> select(year) |> unique() |> as.list()






## ui
ui <- dashboardPage(
  dashboardHeader(title = "National Eletronic Injury Surveillance System",
                  titleWidth = 450),
  dashboardSidebar(collapsed = FALSE,
                   disable = TRUE),
  dashboardBody(
    tabsetPanel(id="tabs",
                tabPanel(title = "Analytics",
                         value = "page1",
                         fluidRow(
                           column(4,offset = 0,
                                  selectInput("product","Injuries",
                                              choices = df$Product_1 |> unique(),
                                              width = "800px",
                                              selected = "Floors Or Flooring Materials")),
                           column(1,offset = 0,
                                  selectInput("year2","Period",choices = years_select))
                         ),
                         fluidRow(
                           box(title = "",
                               status="primary",
                               solidHeader = FALSE,
                               collapsible = FALSE,width = 20,
                               shinycssloaders::withSpinner(
                                 plotlyOutput("injuries"),
                                 type = 6,
                                 color = "lightblue",
                                 color.background = "none",
                                 size=1)),
                           fluidRow(
                             column(width = 4,
                                    shinycssloaders::withSpinner(
                                      plotlyOutput("injuriesDiagnosis"),
                                      type = 6,color = "lightblue",
                                      color.background = "none",
                                      size = 1)),
                             column(width = 4,
                                    shinycssloaders::withSpinner(
                                      plotlyOutput("injuriesLocation"),
                                      type = 6,color="lightblue",
                                      color.background = "none",
                                      size=)),
                             column(width = 4,
                                    shinycssloaders::withSpinner(
                                      plotlyOutput("InjuriesBodyPart"),
                                      type = 6,color="lightblue",
                                      color.background = "none",
                                      size=))
                           ),
                           
                         )
                ),
                tabPanel(title = "Forecast",
                         value = "page2",
                         fluidRow(
                           valueBoxOutput("real2019",width = 3),
                           valueBoxOutput("real2020",width = 3),
                           valueBoxOutput("real2021",width=3),
                           valueBoxOutput("pred22",width=3)
                         ),
                         fluidRow(
                           plotlyOutput("forecastplot")
                         )))
    
  )
  
)


## server
server <- function(input,output){
  output$forecastplot <- renderPlotly({
    ggplotly(
      forecast |>mutate(forecast=as.numeric(forecast)) |> 
        ggplot(aes(x=date,y = round(forecast)))+
        geom_line(aes(color = type))+
        geom_point(aes(color = type))+
        theme_minimal()+
        labs(title = "Injuries, Time-Series",
             x="",
             y="")
    )
  })
  
  output$injuries <- renderPlotly({
    ggplotly(df |> 
               filter(Product_1 == input$product) |> 
               filter(year == input$year2) |> 
               group_by(Sex,Age) |>
               dplyr::summarise(n=n()) |> 
               left_join(population |> mutate_if(is.character,str_to_title),
                         by = c("Sex","Age")) |> 
               mutate(rate = (n/population)*10000) |> 
               na.omit() |> 
               ggplot(aes(x=Age,y=rate))+
               geom_line(aes(color = Sex))+
               geom_point(aes(color=Sex))+
               theme_minimal()+
               labs(title = "Injuries (rate 10k population and statistics weight > 70)",
                    x="",y=""))
    
  })
  
  output$injuriesDiagnosis <- renderPlotly({
    ggplotly(
      df |> 
        filter(year == input$year2) |> 
        filter(Product_1 == input$product) |> 
        group_by(Diagnosis) |> 
        dplyr::summarise(n=n()) |> 
        arrange(desc(n)) |> 
        head(20) |> 
        ggplot(aes(x=n,y=reorder(Diagnosis,n)))+
        geom_col(color = "steelblue", fill = "steelblue", alpha = c(0.3))+
        theme_minimal()+
        labs(x="",
             y="",
             title="Injuries by Diagnosis")
    )
  })
  
  output$injuriesLocation <- renderPlotly({
    ggplotly(
      df |> 
        
        filter(year == input$year2) |> 
        filter(Product_1 == input$product) |> 
        group_by(Location) |> 
        dplyr::summarise(n=n()) |> 
        arrange(desc(n)) |> head(20) |> 
        ggplot(aes(x=n,y=reorder(Location,n)))+
        geom_col(color = "steelblue",fill="steelblue",alpha = c(0.3))+
        theme_minimal()+
        labs(x="",y="",title="Injuries by Location")
    )
  })
  output$InjuriesBodyPart <- renderPlotly({
    ggplotly(
      df |>  
        filter(year == input$year2) |> 
        filter(Product_1 == input$product) |> 
        group_by(Body_Part) |> 
        dplyr::summarise(n=n()) |> 
        arrange(desc(n)) |> head(20) |> 
        ggplot(aes(x=n,y=reorder(Body_Part,n)))+
        geom_col(color = "steelblue",fill = "steelblue", alpha = c(0.3))+
        theme_minimal()+
        labs(x="",y="",title="Injuries by Body Part")
    )
    
    
  })
  
  output$real2018 <- renderValueBox({
    valueBox(real18,
             "Real 2018",color = "lightblue",
             icon=icon("chart-line",lib = "font-awesome"))
  })
  
  output$real2019 <- renderValueBox({
    valueBox(real19,
             "Real 2019",color = "light-blue",
             icon=icon("chart-line",lib = "font-awesome"))
  })
  
  output$real2020 <- renderValueBox({
    valueBox(real20,
             "Real 2020",color = "light-blue",
             icon=icon("chart-line",lib = "font-awesome"))
    
  })
  
  output$real2021 <- renderValueBox({
    valueBox(real21,
             "Real 2021",color = "light-blue",
             icon=icon("chart-line",lib = "font-awesome"))
    
  })
  
  output$pred22 <- renderValueBox({
    valueBox(pred22,
             "Pred 2022",color = "red",
             icon=icon("chart-line",lib = "font-awesome"))
    
  })
  
}



shinyApp(ui,server)
