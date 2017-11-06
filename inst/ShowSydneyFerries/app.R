
library(shiny)

library(leaflet)
library(tRainspotting)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                passwordInput("apikey", "NSW Transport API key",
                           value = ""),
                sliderInput("sampleTrack", label="Track length", min=0, max=100, value=10)
                )
)

server <- function(input, output, session) {

  autoInvalidate <- reactiveTimer(20000)

  ferries <- NULL
  queryNum <- 0

  observe({

    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()
    if (input$apikey != "") {
      tRainspotting::register_nsw(input$apikey)
      tryCatch({
        cat("Update\n")
        ferries_now <- nswVehicles("ferries")
        ferry_locations_now <- cleanFerries(getPosition(ferries_now))
        ferry_locations_now$QueryNum <- queryNum
        ## append to global ferry frame, then filter
        ferry_locations <- rbind(ferries, ferry_locations_now)
        ferries <<- ferry_locations
        m <- leafletProxy("map")
        m <- clearShapes(m)
        m <- clearMarkers(m)
        m <- addCircles(m, data=ferry_locations_now, lng=~longitude, lat=~latitude, radius=1)
        m <- addLabelOnlyMarkers(m, data=ferry_locations_now,
                                 lng=~longitude, lat=~latitude,
                                 label=~label,
                                 labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
        ferry_locations_filt <- subset(ferry_locations, QueryNum > queryNum - input$sampleTrack)
        ships <- unique(ferry_locations_filt$id)

        for (s in ships) {
          dd <- subset(ferry_locations_filt, id==s)
          dd <- dd[order(dd$QueryNum), ]
          m <- addPolylines(m, data=dd, lng=~longitude, lat=~latitude)
        }
        queryNum <<- queryNum + 1
        m
        }, error=function(e){
          print(e)
          cat("Check API key\n")
          }
        )
    }
  })


  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    m <- addProviderTiles(leaflet(), providers$Stamen.TonerLite,
                          options = providerTileOptions(noWrap = TRUE))
    setView(m, lng=151.213896, lat=-33.854211, zoom=12)
  })


}

shinyApp(ui, server)
