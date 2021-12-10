library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(readr)
library(purrr)
library(RPostgres)
library(ggplot2)
library(ggmap)

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "d41lsl8qgestjf", 
                 host = "ec2-3-229-43-149.compute-1.amazonaws.com", 
                 port = 5432, 
                 user = "uqtxfaqjjcxggw", 
                 password = "916d311356954de6a99118d13578bb9d1b47bdc86cb8360a60b9606293bd882d")

df_out = dbGetQuery(con, "SELECT * FROM airbnb")

types <- unique(df_out$room_type)
groups <- unique(df_out$neighbourhood_group)
host_id <- unique(df_out$host_id)

df_out %>%
    group_by(host_id) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(5) -> top_hostid

df_host = filter(df_out, host_id %in% top_hostid$host_id)

df_host%>%
    group_by(host_id, neighbourhood_group) %>%
    summarise(n = n()) -> df_host_agg

df_out %>%
    group_by(neighbourhood_group, room_type)%>%
    summarise(m = mean(price)) -> group_type

df_out %>%
    group_by(neighbourhood_group, neighbourhood, room_type)%>%
    summarise(mean_price = mean(price)) -> df_ng


mykey = "AIzaSyCRgNUY6U40KR4MHy0RHKsUxsSRLkE_0i0"
register_google(key = mykey)
myLocation <- "Nueva York"
myMap <- get_map(location = myLocation, zoom = 10)


ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Alojamientos en Airbnb"),
    navbarPage("", id="main",
               tabPanel("Host", 
                        fluidRow(
                            selectizeInput(
                                inputId = "hosts", 
                                label = "Seleccione un anfitrión", 
                                choices = unique(df_host_agg$host_id), 
                                selected = "219517861",
                                multiple = TRUE
                            ),
                            plotlyOutput(outputId = "thost"),
                            plotlyOutput(outputId = "t2host"))
                        ),
               tabPanel("GrupoVecindario", 
                        fluidRow(
                            selectizeInput(
                                inputId = "types", 
                                label = "Seleccione un tipo de alojamiento",
                                choices = types, 
                                selected = "Shared Room",
                                multiple = TRUE
                            ),
                            selectizeInput(
                                inputId = "group", 
                                label = "Seleccione un tipo de grupo de vecindario", 
                                choices = groups, 
                                selected = "Brooklyn",
                                multiple = FALSE
                            ),
                            plotlyOutput(outputId = "t2group"),
                            plotlyOutput(outputId = "tgroup")
                            )
               ),
               tabPanel("Participación", 
                        fluidRow(
                            plotlyOutput("pie"), uiOutput("back")
                        )
               ),
               tabPanel("Mapas", 
                        fluidRow(
                            plotlyOutput("maps"),
                            plotlyOutput("maps2")
                        )
               )
               )
)

server <- function(input, output, ...) {
    output$thost <- renderPlotly({
        plot_ly(top_hostid, x = ~factor(host_id), y = ~n, source="host1") %>%
            filter(host_id %in% input$hosts) %>%
            add_trace(type = 'bar', position="dodge")%>%
            layout(title = "Top 5 de los anfitriones con más alojamientos", xaxis = list(title = 'Host'), yaxis = list(title = 'Cant . Alojamientos'), showlegend = FALSE)
    })
    
    output$t2host <- renderPlotly({
        # if there is no click data, render nothing!
        clickData <- event_data("plotly_click", source = "host1")
        if (is.null(clickData)) return(NULL)
        
        vars <- c(clickData[["x"]])
        d <- filter(df_host_agg, host_id %in% vars)
        
        plot_ly(d, x = ~factor(host_id), y = ~n, color=~factor(neighbourhood_group)) %>%
            group_by(neighbourhood_group) %>%
            add_trace(type = 'bar')%>%
            layout(title = "\nTop 5 de los anfitriones con más alojamientos distribuidos por grupo vecindario", xaxis = list(title = 'Host'), yaxis = list(title = 'Cant . Alojamientos'), 
                   showlegend = TRUE) 
        
    })
    
    output$tgroup <- renderPlotly({
        plot_ly(df_ng, x = ~neighbourhood, y = ~mean_price, color=~factor(room_type)) %>%
            filter(neighbourhood_group %in% input$group) %>%
            filter(room_type %in% input$types) %>%
            group_by(room_type) %>%
            add_trace(type = 'bar')%>%
            layout(title = "Precio Promedio por vecindario", xaxis = list(title = 'Vecindario'), yaxis = list(title = 'Precio Medio'), showlegend = TRUE)
    })
    output$t2group <- renderPlotly({
        req(input$types)
        p <- ggplot(data = filter(group_type, room_type %in% input$types)) + 
            geom_bar(stat="identity", aes(x=room_type, y=m, fill=room_type))+
            facet_wrap(~neighbourhood_group)+
            xlab("Grupo Vecindario") + ylab("Precio Medio") +
            ggtitle("\nPrecio promedio por grupo de vecindario y tipo de alojamiento")+
            theme(axis.text.x=element_blank(), legend.title = element_blank())
        ggplotly(p)
    })
    
    output$maps <- renderPlotly({
        ggmap(myMap) + 
            geom_point(data=df_out, aes(x = longitude, y = latitude, colour= neighbourhood_group), source="maps")+
            ggtitle("Distribución de alojamientos por grupo de vecindario")-> g
            ggplotly(g)
    })

    output$maps2 <- renderPlotly({
        ggmap(myMap) + 
        geom_point(data=df_out, aes(x = longitude, y = latitude, colour= price))+
        scale_color_gradientn(colours = rainbow(5))+
        facet_wrap(~neighbourhood_group)+
        ggtitle("Precios por grupo de vecindario")-> g2
        ggplotly(g2,height = 600, width=1024)
    })
    
    current_neighbourhood_group <- reactiveVal()
    prices_data <- reactive({
        if (!length(current_neighbourhood_group())) {
            return(count(df_out, neighbourhood_group, wt = round(mean(price),2)))
        }
        df_out %>%
            filter(neighbourhood_group %in% current_neighbourhood_group()) %>%
            count(room_type, wt =  round(mean(price),2))
    })
    
    # Note that pie charts don't currently attach the label/value 
    # with the click data, but we can include as `customdata`
    output$pie <- renderPlotly({
        d <- setNames(prices_data(), c("labels", "values"))
        plot_ly(d) %>%
            add_pie(
                labels = ~labels, 
                values = ~values, 
                customdata = ~labels
            ) %>%
            layout(title = list(text=current_neighbourhood_group() %||% "Precio Promedio"))
    })
    
    # update the current neighbourhood_group when appropriate
    observe({
        cd <- event_data("plotly_click")$customdata[[1]]
        if (isTRUE(cd %in% groups)) current_neighbourhood_group(cd)
    })
    
    # populate back button if neighbourhood_group is chosen
    output$back <- renderUI({
        if (length(current_neighbourhood_group())) 
            actionButton("clear", "Back", icon("chevron-left"))
    })
    
    # clear the chosen neighbourhood_group on back button press
    observeEvent(input$clear, current_neighbourhood_group(NULL))
    
}


# Run the application 
shinyApp(ui = ui, server = server)
