library(shiny)
library(leaflet)
library(DBI)
library(RSQLite)

dbu = dbConnect(SQLite(), dbname="/home/dentalplan/Programming/estatemap/ldd_essential/ldd_wlatlon.sqlite")  
q <- dbSendQuery(dbu, "SELECT name FROM ld_boroughs ORDER BY name")
searchfieldin <- c("Borough Ref", "Postcode", "Address", "Description")
searchfieldout <- c("borough_ref", "postcode", "prim_add_obj_name || ' ' || street", "descr")
searchfields <- setNames(as.list(searchfieldin), searchfieldout)

while (!dbHasCompleted(q)){
  bor <- dbFetch(q, n = -1)
}
bor <- rbind(data.frame(name = "All boroughs"), bor)
#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()


#This bit controls the UI - I'm adding a load of widgets to the sidebar, and telling it where to put the main map
ui <- fluidPage(
  titlePanel("London Data Map"),
  sidebarPanel(
    tabsetPanel(type = "tabs",
                tabPanel("LDDB", 
                         checkboxInput("ldd_active"," Active", value = TRUE),
                         selectInput ("status","Status:",list("All valid","All invalid")),
                         #selectInput ("borough","Borough:",list("Newham","Lambeth","Tower Hamlets")),
                         selectInput ("ldd_borough","Borough:", bor),
                         dateRangeInput("dates", "Period:", start = "2006-01-01", end = "2019-12-31", min = "2006-01-01", format = "dd/mm/yyyy"),
                         sliderInput("shi",
                                     "Social housing impact (min housing unit change):",
                                     min = 1,
                                     max = 500,
                                     value = 1),
                         
                         selectInput ("searchfield","Search in:",list("Borough Ref", "Postcode", "Address", "Description")),
                         textInput("searchtext", "Search")
                ),
                tabPanel("IMD",
                         checkboxInput("IMD_active"," Active"))
    )
  ),
  mainPanel(
    leafletOutput("mymap"),
    #textOutput("infopanel"),
    "oh",
    p()
  )
)

server <- function(input, output, session) {
  db = dbConnect(SQLite(), dbname="/home/dentalplan/Programming/estatemap/ldd_essential/ldd_wlatlon.sqlite")  
  tbl <- "results_all_nobranch_S_net"
  
  l <- renderLeaflet({
    if (input$ldd_active == TRUE){
      w1 <- paste("WHERE (sum_prop_S - sum_exist_S) <= -", toString(input$shi), sep="")  
      if (input$ldd_borough == "All boroughs"){
        w2 <- ""
      }else{
        w2 <- paste("AND borough = '",input$ldd_borough,"'",sep="")
      }
      w3 <- paste("AND permission_date > '", toString(input$dates[1]), "'",  sep="")
      w4 <- paste("AND permission_date < '", toString(input$dates[2]),"'", sep="") 
      where <- paste(w1, w2, w3)
      q <- dbSendQuery(db, paste("SELECT branches, permission_id, lat, lon, status_rc, permission_date, permission_year, descr, prim_add_obj_name, street, post_code, borough, borough_ref, sum_exist_s, sum_exist_la, sum_prop_s, sum_prop_la, sum_exist_m, sum_exist_llr, sum_exist_a, sum_exist_i, sum_exist_sh, sum_exist_prs, sum_exist_dmr, sum_prop_llr, sum_prop_a, sum_prop_i, sum_prop_sh, sum_prop_prs, sum_prop_dmr, sum_prop_m FROM ", tbl, where))
      while (!dbHasCompleted(q)){
        mark.data <- dbFetch(q, n = -1) #n-1 needed not to stop at 500!
        popuptext <- paste("<strong>Address:</strong>", mark.data$prim_add_obj_name, mark.data$street, "<br/>", mark.data$post_code,"<br/><strong>Borough:</strong>", mark.data$borough, "<br/><strong>Permission granted:</strong>", mark.data$permission_date, "<br/><strong>Borough ref:</strong>", mark.data$borough_ref, "<br/><Strong>Social housing impact:</strong>", mark.data$sum_prop_s - mark.data$sum_exist_s, "<br/><Strong>Status:</strong>", mark.data$status_rc)
        markers <- data.frame(mark.data$lon, mark.data$lat, 1, as.character(popuptext), as.character(mark.data$borough_ref))
      }      
      colnames(markers) <- c("lng", "lat", "radius", "popup", "label")
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap.HOT,
                         options = providerTileOptions(noWrap = FALSE)
        ) %>%
        addCircleMarkers(lng = markers$lng, lat=markers$lat, radius=markers$radius, popup=markers$popup, label=markers$label, data = markers)
    }
  })
  output$mymap <- l
  #output$mymap <- l
  # click<-l$marker_click
  # output$infopanel <-renderUI(
  #   toString(click$lat)
  # )
  #dbDisconnect(db)
}

shinyApp(ui, server)