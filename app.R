#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(RMariaDB)
library(shinyWidgets)
library(odbc)
library(DBI)
library(ggplot2)




Sys.setlocale(category = "LC_ALL", locale = "Russian") ##here
# Define UI for application that draws a histogram


con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "vm-bi", 
                      Database = "SP_Common", Trusted_Connection = "True")

rs2 <-  dbSendQuery(con, "SELECT distinct FIO FROM Users_powerbi where (department = 'Техническая поддержка' or department = 'Эксплуатация') and dateend is null and type <> 'вакансия'")
responsible <- dbFetch(rs2, n = -1)

dbcon <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "vm-bi", 
                      Database = "SP_Trades", Trusted_Connection = "True")
rs3 <-  DBI::dbSendQuery(dbcon, "set nocount on\nEXEC [rpt].[SP_megaplan_3ltp_new_Test]
        @reportType = 'stat'")

d2 <- dbFetch(rs3,n = -1)


sidebar <- dashboardSidebar(
    ###selectInput ("project","Проект",choices = c("223 фз","44 фз","Витрина,МИК,ЭДО,Факторинг","Имущество","615 пп","Проект не проставлен"),multiple=TRUE )

    
    pickerInput(
        inputId = "project",
        label = "Проект", 
        choices = c("223 фз","44 фз","Витрина,МИК,ЭДО,Факторинг","Имущество","615 пп","Проект не проставлен"),
        options = list(
            `actions-box` = TRUE), 
        multiple = TRUE,
        selected = c("223 фз","44 фз","Витрина,МИК,ЭДО,Факторинг","Имущество","615 пп","Проект не проставлен")
    ),
    pickerInput(
        inputId = "priority",
        label = "Приоритет", 
        choices = c("Низкий приоритет","Высокий приоритет","Средний приоритет","Чрезвычайный приоритет"),
        options = list(
            `actions-box` = TRUE), 
        multiple = TRUE,
        selected = c("Низкий приоритет","Высокий приоритет","Средний приоритет","Чрезвычайный приоритет")
    ),
    
    pickerInput(
        inputId = "responsible",
        label = "исполнитель", 
        choices = sort(unique(responsible$FIO)),
        options = list(
            `actions-box` = TRUE), 
        multiple = TRUE,
        selected = unique(responsible$FIO)
    
    )
)

body <- dashboardBody(
    
  
     fluidRow(
         
         
         
         
         box(
    width = 6,     
    plotOutput(outputId = "project")
             )
            ,
    
        box(
    width = 6,
    plotOutput(outputId = "priority")
           )
             )   ,
      
    
    
    
    fluidRow(
        box(
    width = 6,
    plotOutput(outputId = "responsible")
        )
    ,
    
     box(
            width = 6,
            plotOutput(outputId = "Overdueresponsible")
        )
    ),
    
    fluidRow(
        box(
            width = 6,
            plotOutput(outputId = "Createproject")
        )
    ,
    
        box(
            width = 6,
            plotOutput(outputId = "Createpriority")
        )
    )       
         
)

ui <- dashboardPage(header = dashboardHeader(),
                          sidebar = sidebar,
                          body = body
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {

    OperateToday <- reactive ({
               d2 %>% dplyr::filter(as.Date(day)==Sys.Date()) %>% filter(priority %in% input$priority & responsible_name %in% input$responsible) %>% group_by(project) %>% 
        summarise(open = sum(открыто),overdue = sum(закрыто),created = sum(поставлено)) 
                           })
    
    OperateToday_resp <-reactive({
         d2 %>% dplyr::filter(as.Date(day)==Sys.Date()) %>% filter(priority %in% input$priority & project %in% input$project)  %>% group_by(responsible_name) %>% 
        summarise(open = sum(открыто),overdue = sum(закрыто),created = sum(поставлено))
                         })
    
    OperateToday_prior <- reactive({
        d2 %>% dplyr::filter(as.Date(day)==Sys.Date()) %>% filter(responsible_name %in% input$responsible & project %in% input$project) %>% group_by(priority) %>% 
        summarise(open = sum(открыто),overdue = sum(закрыто),created = sum(поставлено)) 
                                 })
    
    
    output$project <- renderPlot({
        
    OpenToday_Plot <- OperateToday() %>% arrange(open) %>% mutate(project=factor(project,levels=project))    
        
    ggplot2::ggplot(data = OpenToday_Plot, aes(x = open, y = project)) + geom_bar(stat = "identity",fill = "dodgerblue3") + 
        geom_text(aes(label = open)) + ggtitle("Открыто на сегодня") + xlab("") + theme_bw() + 
        theme(
            panel.background = element_rect(fill = "grey80",
                                            colour = "lightblue"))  
        
    })
    
    
    
    output$responsible <- renderPlot({
        
        OpenToday_Plot <- OperateToday_resp() %>% arrange(open) %>% mutate(responsible_name=factor(responsible_name,levels=responsible_name))    
        
        ggplot2::ggplot(data = OpenToday_Plot, aes(x = open, y = responsible_name)) + geom_bar(stat = "identity",fill = "dodgerblue3") + 
            geom_text(aes(label = open)) + ggtitle("Открыто на сегодня по исполнителю") + xlab("") + theme_bw() + 
            theme(
                panel.background = element_rect(fill = "grey80",
                                                colour = "lightblue"))  
        
    })
    
    
    output$priority <- renderPlot({
        
        OpenToday_Plot <- OperateToday_prior() %>% arrange(open) %>% mutate(priority=factor(priority,levels=priority))    
        
        ggplot2::ggplot(data = OpenToday_Plot, aes(x = open, y = priority)) + geom_bar(stat = "identity",fill = "dodgerblue3") + 
            geom_text(aes(label = open)) + ggtitle("Открыто на сегодня по приоритету") + xlab("") + theme_bw() + 
            theme(
                panel.background = element_rect(fill = "grey80",
                                                colour = "lightblue"))  
        
                                })
    
    
    output$Overdueresponsible <- renderPlot({
        
        OpenToday_Plot <- OperateToday_resp() %>% arrange(overdue) %>% mutate(responsible_name=factor(responsible_name,levels=responsible_name))    
        
        ggplot2::ggplot(data = OpenToday_Plot, aes(x = overdue, y = responsible_name)) + geom_bar(stat = "identity",fill = "dodgerblue3") + 
            geom_text(aes(label = overdue)) + ggtitle("Закрыто сегодня по исполнителю") + xlab("") + theme_bw() + 
            theme(
                panel.background = element_rect(fill = "grey80",
                                                colour = "lightblue"))  
        
    })
    
    
    
    output$Createproject <- renderPlot({
        
        OpenToday_Plot <- OperateToday() %>% arrange(created) %>% mutate(project=factor(project,levels=project))    
        
        ggplot2::ggplot(data = OpenToday_Plot, aes(x = created, y = project)) + geom_bar(stat = "identity",fill = "dodgerblue3") + 
            geom_text(aes(label = created)) + ggtitle("Поставлено сегодня по проекту") + xlab("") + theme_bw() + 
            theme(
                panel.background = element_rect(fill = "grey80",
                                                colour = "lightblue"))  
        
    })
    
    
    output$Createpriority <- renderPlot({
        
        OpenToday_Plot <- OperateToday_prior()  %>% filter(created>0)    
        
        ggplot2::ggplot(data = OpenToday_Plot, aes(x="", y=created, fill=priority)) +
            geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0) + theme_void() + geom_text(aes(y = created, label = created), color = "white", size=6,position = position_stack(vjust = 0.5))+theme(
                panel.background = element_rect(fill = "grey80",
                                                colour = "lightblue")) + coord_polar("y", start=0) +
            scale_fill_brewer(palette="Set1") + ggtitle("Поставлено сегодня по приоритету")  
        
    })     
        
        
        
 
}

# Run the application 
shinyApp(ui = ui, server = server)
