library(jsonlite)
library(readr)
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)
library(leafpop)
library(purrr)
library(tidyr)
library(plotly)

#dataindo

#dataglobal
a="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
b="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
c="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

data_1=read_csv(a, col_names = TRUE)
data_2=read_csv(b, col_names = TRUE)
data_3=read_csv(c, col_names = TRUE)


header <- dashboardHeader(title = "COVID-19 DASHBOARD",dropdownMenu(type="notifications",icon = icon("question-circle"),badgeStatus = NULL,headerText = "contact info:",notificationItem("github", icon = icon("file"),href = "https://github.com/riskyfrasetio"),notificationItem("Linkedin", icon = icon("file"),href = "https://www.linkedin.com/in/risky-frasetio-wahyu-pratama-207359137/")))  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Indonesia Summary", tabName = "ID", icon = icon("ID")),
    menuItem("Case Distribution", tabName = "CC", icon = icon("CC")),
    menuItem("Death rate vs Recovered rate", tabName = "AA", icon = icon("AA")),
    menuItem("Global Summary", tabName = "GG", icon = icon("GG")),
    menuItem("Current Cases", tabName = "OO", icon = icon("OO"))
  )
)

body <- dashboardBody(tabItems(tabItem(tabName = "ID", 
                                      fluidRow(
                                        valueBoxOutput("Kasus.positif")
                                        ,valueBoxOutput("Kasus.meninggal")
                                        ,valueBoxOutput("Kasus.sembuh")
                                        ,valueBoxOutput("Kasus.aktif")
                                        ,valueBoxOutput("death.rate")
                                        ,valueBoxOutput("recovery.rate")
                                      ),
                                      fluidRow(
                                        box(
                                          title = "Peta Sebaran Kasus"
                                          ,status = "primary"
                                          ,solidHeader = TRUE
                                          ,width=15
                                          ,collapsible = TRUE 
                                          ,leafletOutput("sebaran", height = "500px")
                                        )
                                      )),
                               tabItem(tabName = "CC",
                                       fluidPage(
                                         titlePanel("Distribution"),
                                         sidebarPanel(selectInput(inputId ="country",
                                                      label="Pilih negara:",
                                                      choices = unique(data_1$`Country/Region`)
                                                      )),
                                         mainPanel(
                                           plotlyOutput("d")  
                                         )
                                       )),
                               tabItem(tabName = "AA",
                                       fluidPage(
                                         titlePanel("Death rate vs recovered rate"),
                                         sidebarPanel(selectInput(inputId ="country1",
                                                      label="Pilih negara:",
                                                      choices = unique(data_1$`Country/Region`),
                                                      multiple = TRUE,width=100)
                                         ),
                                         mainPanel(fluidRow(splitLayout(
                                           cellWidths = c("50%","50%"),plotlyOutput("oo1")
                                           ,plotlyOutput("oo2")  
                                         )))
                                       )),
                               tabItem(tabName = "GG", 
                                       fluidRow(
                                         valueBoxOutput("Kasus.positif.world",width = 2)
                                         ,valueBoxOutput("Kasus.meninggal.world",width = 2)
                                         ,valueBoxOutput("Kasus.sembuh.world",width = 2)
                                         ,valueBoxOutput("Kasus.aktif.world",width = 2)
                                         ,valueBoxOutput("death.rate.world",width = 2)
                                         ,valueBoxOutput("recovery.rate.world",width = 2)
                                         ,valueBoxOutput("number.country",width = 2)
                                         ,valueBoxOutput("recover.country",width = 2)
                                       ),
                                       fluidRow(id = "first",
                                         box(
                                           title = "Peta Sebaran Kasus Aktif by Country"
                                           ,status = "primary"
                                           ,solidHeader = TRUE
                                           ,width=15
                                           ,collapsible = TRUE 
                                           ,leafletOutput("world", height = "500px")
                                         )
                                       )
                                       ),
tabItem(tabName = "OO",fluidRow(splitLayout(
    cellWidths = c("50%","50%"),plotlyOutput("o1")
    ,plotlyOutput("o2",height="500px")  
  )
)))) 


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'my dashboard', header, sidebar, body, skin='blue')



server <- function(input, output, session) {
  url="https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/COVID19_Indonesia_per_Provinsi/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
  
  data_=fromJSON(url, flatten = TRUE)
  data_provinsi=data_$features
  data_provinsi=data_provinsi %>% mutate(Long=geometry.y,Lat=geometry.x) %>% select(-c(geometry.x,geometry.y))
  
  data_provinsi_case= data_provinsi %>% select(Provinsi=attributes.Provinsi,Long,Lat,jumlah=attributes.Kasus_Posi) %>% mutate(type=rep("positif",35))
  
  data_provinsi_death= data_provinsi %>% select(Provinsi=attributes.Provinsi,Long,Lat,jumlah=attributes.Kasus_Meni) %>% mutate(type=rep("meninggal",35))
  
  data_provinsi_rec= data_provinsi %>% select(Provinsi=attributes.Provinsi,Long,Lat,jumlah=attributes.Kasus_Semb) %>% mutate(type=rep("sembuh",35))
  
  data_prov=rbind(data_provinsi_case,data_provinsi_death,data_provinsi_rec)
  
  url_global="https://services5.arcgis.com/VS6HdKS0VfIhv8Ct/arcgis/rest/services/Statistik_Perkembangan_COVID19_Indonesia/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
  
  data_time=fromJSON(url_global, flatten = TRUE)
  data_time=data_time$features
  
  # PETA INDO
  
  data_plot <- data_prov %>% filter(jumlah>0 & Provinsi!='Indonesia') %>%
    mutate(log_cases = 2 * log(jumlah))
  cv_data_plot_asean.split <- data_plot %>% split(data_plot$type)
  
  pal <- colorFactor(c("orange", "red", "green"), domain = c("positif","meninggal","sembuh"))
  map_object <- leaflet() %>% setView(lat = -0.98981, lng  = 113.91587, zoom = 5) %>% addProviderTiles(providers[[1]])
  names(cv_data_plot_asean.split) %>%
    purrr::walk(function(df){
      map_object <<- map_object %>%
        addCircleMarkers(
          data = cv_data_plot_asean.split[[df]],
          lng = ~Lat, lat = ~Long,
          # label=~as.character(cases),
          color = ~pal(type),
          stroke = FALSE,
          fillOpacity = 0.9,
          radius = ~log_cases,
          popup = leafpop::popupTable(cv_data_plot_asean.split[[df]],
                                      feature.id = FALSE,
                                      row.numbers = FALSE,
                                      zcol = c("type","jumlah","Provinsi")
          ),
          group = df,
          # clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
          labelOptions = labelOptions(
            noHide = F,
            direction = "auto"
          )
        )
    })
  
output$sebaran=renderLeaflet({ map_object %>%
    addLayersControl(
      overlayGroups = names(cv_data_plot_asean.split),
      options = layersControlOptions(collapsed = FALSE)
    )
}
)

kasus_positif=as.numeric(data_prov %>% filter(type=='positif') %>% summarise(jumlah=sum(jumlah)))
kasus_meninggal=as.numeric(data_prov %>% filter(type=='meninggal') %>% summarise(jumlah=sum(jumlah)))
kasus_sembuh=as.numeric(data_prov %>% filter(type=='sembuh') %>% summarise(jumlah=sum(jumlah)))
kasus_aktif=kasus_positif-kasus_meninggal-kasus_sembuh
d_rate=round(kasus_meninggal*100/kasus_positif,2)
r_rate=round(kasus_sembuh*100/kasus_positif,2)
output$Kasus.positif <- renderValueBox({
  valueBox(
    formatC(kasus_positif, format="d", big.mark=',')
    ,"Kasus Terkonfirmasi"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "yellow")
})
output$Kasus.meninggal <- renderValueBox({
  valueBox(
    formatC(kasus_meninggal, format="d", big.mark=',')
    ,"Kasus Meninggal"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "red")
})

output$Kasus.sembuh <- renderValueBox({
  valueBox(
    formatC(kasus_sembuh, format="d", big.mark=',')
    ,"Kasus Sembuh"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "green")
})

output$Kasus.aktif <- renderValueBox({
  valueBox(
    formatC(kasus_aktif, format="d", big.mark=',')
    ,"Kasus Aktif"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "orange")
})

output$death.rate <- renderValueBox({
  valueBox(
    formatC(paste(d_rate,'%'), digits=2, format="f", big.mark=',')
    ,"Persen Kematian"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "red")
})

output$recovery.rate <- renderValueBox({
  valueBox(
    formatC(paste(r_rate,'%'), digits=2, format="f", big.mark=',')
    ,"Persen Sembuh"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "green")
})

data_confirmed=data_1 %>% gather(Tanggal,confirmed,-c(`Country/Region`,Lat,Long,`Province/State`))
data_recovered=data_3 %>% gather(Tanggal,confirmed,-c(`Country/Region`,Lat,Long,`Province/State`))
data_death=data_2 %>% gather(Tanggal,confirmed,-c(`Country/Region`,Lat,Long,`Province/State`))

#False Long Lat
data_confirmed_country=data_confirmed %>% group_by(`Country/Region`,Tanggal) %>% summarise(Lat=mean(Lat),Long=mean(Long),confirmed=sum(confirmed)) %>% mutate(Tanggal=as.Date(Tanggal,format="%m/%d/%y"))

data_recovered_country=data_recovered %>% group_by(`Country/Region`,Tanggal) %>% summarise(Lat=mean(Lat),Long=mean(Long),confirmed=sum(confirmed)) %>% mutate(Tanggal=as.Date(Tanggal,format="%m/%d/%y"))

data_death_country=data_death %>% group_by(`Country/Region`,Tanggal) %>% summarise(Lat=mean(Lat),Long=mean(Long),confirmed=sum(confirmed)) %>% mutate(Tanggal=as.Date(Tanggal,format="%m/%d/%y"))

#countryInput <- reactive({
#  switch(input$country,
#         "Italy" = Italy,
#         "Indonesia" = Indonesia,
#         "China" = China ,
#         "USA"=US,
#         "Spain"=Spain)
#})


data_case= data_confirmed_country %>% left_join(data_death_country,by=c("Country/Region","Tanggal")) %>% mutate(Lat=Lat.x,Long=Long.x,confirmed=confirmed.x,death=confirmed.y) %>% select(-c(Lat.y,Long.y,Lat.x,Long.x,confirmed.x,confirmed.y))
data_case=data_case %>% left_join(data_recovered_country,by=c("Country/Region","Tanggal")) %>% mutate(Lat=Lat.x,Long=Long.x,confirmed=confirmed.x,recover=confirmed.y) %>% select(-c(Lat.y,Long.y,Lat.x,Long.x,confirmed.x,confirmed.y)) %>% arrange(Tanggal) %>% mutate(active_case=confirmed-death-recover)
  
output$d=renderPlotly({
  plot_ly(data_case %>% filter(`Country/Region`==input$country & confirmed>0) %>% arrange(Tanggal) %>% as.data.frame(),x=~Tanggal,y=~active_case,type="bar",name = 'active cases') %>% add_trace(y =~death , name = 'death cases') %>% add_trace(y =~recover , name = 'recovered cases')  %>% layout(yaxis = list(title = 'confirmed cases'), barmode = 'stack')
})

rate=data_case %>% group_by(`Country/Region`,Tanggal) %>% filter(confirmed>0) %>% mutate(death_=death*100/confirmed,recover_=recover*100/confirmed) %>% mutate(number=1) 
rate=rate %>% group_by(`Country/Region`) %>% mutate(day=cumsum(number))

output$oo1=renderPlotly({
plot_ly(rate, x = ~day, y = ~death_,color=~`Country/Region`) %>%
  filter(`Country/Region` %in% input$country1) %>%
  group_by(`Country/Region`) %>%
  add_lines() %>% layout(title='Death Rate by Country',xaxis=list(title='Days'),yaxis=list(title='Death (%)'))
})

output$oo2=renderPlotly({
plot_ly(rate, x = ~day, y = ~recover_,color=~`Country/Region`) %>%
  filter(`Country/Region` %in% input$country1) %>%
  group_by(`Country/Region`) %>%
  add_lines() %>% layout(title='Recovered Rate by Country',xaxis=list(title='Days'),yaxis=list(title='Recovered (%)'))
})

kasus_world=rate %>% group_by(`Country/Region`) %>% summarise(total=max(confirmed)) %>% summarise(sum(total)) %>% as.numeric()
death_world=rate %>% group_by(`Country/Region`) %>% summarise(total=max(death)) %>% summarise(sum(total)) %>% as.numeric()
recover_world=rate %>% group_by(`Country/Region`) %>% summarise(total=max(recover)) %>% summarise(sum(total)) %>% as.numeric()
active_world=kasus_world-death_world-recover_world
death_rate_world=round(death_world*100/kasus_world,2)
recover_rate_world=round(recover_world*100/kasus_world,2)

output$Kasus.positif.world <- renderValueBox({
  valueBox(
    formatC(kasus_world, format="d", big.mark=',')
    ,"Kasus Terkonfirmasi"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "yellow")
})
output$Kasus.meninggal.world <- renderValueBox({
  valueBox(
    formatC(death_world, format="d", big.mark=',')
    ,"Kasus Meninggal"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "red")
})

output$Kasus.sembuh.world <- renderValueBox({
  valueBox(
    formatC(recover_world, format="d", big.mark=',')
    ,"Kasus Sembuh"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "green")
})

output$Kasus.aktif.world <- renderValueBox({
  valueBox(
    formatC(active_world, format="d", big.mark=',')
    ,"Kasus Aktif"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "orange")
})

output$death.rate.world <- renderValueBox({
  valueBox(
    formatC(paste(death_rate_world,'%'), digits=2, format="f", big.mark=',')
    ,"Persen Kematian"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "red")
})

output$recovery.rate.world <- renderValueBox({
  valueBox(
    formatC(paste(recover_rate_world,'%'), digits=2, format="f", big.mark=',')
    ,"Persen Sembuh"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "green")
})

number_country=length(unique(rate$`Country/Region`))

output$number.country <- renderValueBox({
  valueBox(
    formatC(number_country, digits=2, format="f", big.mark=',')
    ,"Country Terjangkit"
    ,icon = icon("stats",lib='glyphicon')
    ,color = "yellow")
})

true_long_lat=data_confirmed %>% group_by(`Country/Region`) %>% filter(row_number()==n()) %>% select(`Country/Region`,Lat,Long)

rate=rate %>% select(-c(Lat,Long)) %>% left_join(true_long_lat,by=c("Country/Region"))

data_world=rate %>% group_by(`Country/Region`,Lat,Long) %>% summarise(recovered=max(recover),confirmed=max(confirmed),death=max(death)) %>% mutate(active=confirmed-recovered-death)
data_world_long=data_world %>% gather(type,jumlah,recovered:active)


data_world_long <- data_world_long %>% filter(jumlah>0) %>%
  mutate(cases = 2 * log(jumlah))
cv_data_plot_world.split <- data_world_long %>% split(data_world_long$type)

pale <- colorFactor(c("orange", "red", "green","yellow"), domain = c("confirmed","death","recovered","active"))
map_object2 <- leaflet() %>% setView(lat = -0.98981, lng  = 113.91587, zoom = 3) %>% addProviderTiles(providers$Stamen.Toner)
names(cv_data_plot_world.split) %>%
  purrr::walk(function(da){
    map_object2 <<- map_object2 %>%
      addCircleMarkers(
        data = cv_data_plot_world.split[[da]],
        lng = ~Long, lat = ~Lat,
        # label=~as.character(cases),
        color = ~pale(type),
        stroke = FALSE,
        fillOpacity = 0.9,
        radius = ~cases,
        popup = leafpop::popupTable(cv_data_plot_world.split[[da]],
                                    feature.id = FALSE,
                                    row.numbers = FALSE,
                                    zcol = c("type","jumlah","Country/Region")
        ),
        group = da,
        # clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
        labelOptions = labelOptions(
          noHide = F,
          direction = "auto"
        )
      )
  })

output$world=renderLeaflet({ map_object2 %>%
    addLayersControl(
      overlayGroups = names(cv_data_plot_world.split),
      options = layersControlOptions(collapsed = FALSE)
    )
}

)
data_world$`Country/Region` <- factor(data_world$`Country/Region`, levels = unique(data_world$`Country/Region`)[order(data_world$active, decreasing = TRUE)])

output$o1=renderPlotly({
plot_ly(data_world %>% filter(confirmed>0) %>% arrange(-active) %>% head(20),x=~`Country/Region`,y=~active,type="bar",name = 'active cases')  %>% layout(title="Highest Current Active Cases",yaxis = list(title = 'cases'))
})

output$o2=renderPlotly({
  plot_ly(data_world %>% filter(confirmed>0) %>% arrange(active) %>% head(20),x=~`Country/Region`,y=~active,type="bar",name = 'active cases')  %>% layout(title="Lowest Current Active Cases",yaxis = list(title = 'Cases'))
})

}

shinyApp(ui = ui, server = server)


