#Name: Pradnya Alchetti
#Student Id - 29595916
#Visualisation Project

# server.R
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(geosphere)
library(plotly)
library(chorddiag)
library(ggplot2)

#Read formatted csv for airports data
data <- read.csv("data_with_airports.csv", sep = '\t')
#Read sample taxi data with zones 
data_hail <- read.csv("hailForStatsWithZone_Sample_2.csv")
#Read taxi zones csv created from the shape file
taxi_zones <- read.csv("taxi_zones.csv")

#add latitude and longitude of the airports to the airport data
for(i in 1:nrow(data)){
  if(data[i,'airport']=='LGA'){
    data[i,'pick_longitude']= -73.87194
    data[i,'pick_latitude']= 40.77472
  }else if(data[i,'airport']=='JFK'){
    data[i,'pick_longitude']= -73.78222
    data[i,'pick_latitude']= 40.64417
  }else if(data[i,'airport']=='EWR'){
    data[i,'pick_longitude']= -74.175
    data[i,'pick_latitude']= 40.68972
  }
}


#server function to plot various input variables
function(input, output, session) {

  #create airplane icon for further use as markers
  airPlaneIcon <- makeIcon(
    iconUrl = "http://getdrawings.com/free-icon/plane-icon-png-75.png",
    iconWidth = 25, iconHeight = 25
  )
  
  #Render dialog box for Airport Insights or Fare Estimator
  observe({
    if(input$nav == "NYC AirPort Insights"){
      showModal(modalDialog(
        title = "Airport Insights For Business User",
        "This section of the Application is only for Business User. If you are customer please visit the Fare estimator tab.
    This section provides Taxi trip details for three airports in NYC for the period Oct 2016-Dec2016.
    It allows the business user to check the trip details such as number of trips, tipping trends changing monthly."
      ))
      
    }else if(input$nav == "NYC Fare Estimator"){
      showModal(modalDialog(
        title = "Fare Estimator For the Customer",
        "This section of the Application is only for Cutomers. The customer can choose the borough, pickup and dropoff locations.
    This fare estimator will provide the average cost and the trip duration to reach from one zone to the other."
      ))
    }
    
  })
  
  
  #Render base leaflet with the map focussing on New York
  output$siteMap <- renderLeaflet({ # create leaflet map
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }") %>% 
      addTiles() %>% 
      addProviderTiles('CartoDB.Positron')%>%
      setView(-74.00, 40.71, zoom = 13)
  })
  
  #Generate the intial data for Airport trips dependending upon selected input
  plot_data <- reactive({
    if(length(input$airport)==3){
      
      return(data[data['pickup_month']==input$month,])
      
    } else if(length(input$airport)==2) {
      
      return(data[((data['airport']==input$airport[1]) | (data['airport']==input$airport[2]))&(data['pickup_month']==input$month),])
      
    } else if(length(input$airport)==1){
      return(data[(data['airport']==input$airport[1])&(data['pickup_month']==input$month),])
    }
  })
  
  #Render donut chart for payment type
  output$payment <- renderPlotly({
    if(length(input$airport) > 0){
      
        # group the data by payment type
        pay_data <- plot_data() %>%
        group_by(Payment_type)%>%
        summarize(count = n())
    
      payment_data <- data.frame(matrix(ncol = 2, nrow = 2))
      colnames(payment_data) <- c("Payment_type", "Count")
      
      # replace the payment type numbers to string values
      for(i in 1:nrow(pay_data)){
        if(pay_data[i,'Payment_type']==1){
          payment_data[i,'Payment_type'] = "Credit Card"
          payment_data[i,'Count'] = pay_data[i, 'count']
        }else if(pay_data[i,'Payment_type']==2){
          payment_data[i,'Payment_type'] = "Cash"
          payment_data[i,'Count'] = pay_data[i, 'count']
        }
      }
      
      # Plot the graph
      plot_ly(payment_data, labels = ~Payment_type, values = ~Count) %>%
        add_pie(hole = 0.6) %>%
        layout(title = 'Payment Type Percentage', showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }  
  })
  
  
  #Replace the missing values for hour and average tip 
  #to corresponding hour value and average tip as 0
  f <- function(new_tip, days, i){
    vec2 <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
    for(each in days){
      d <- new_tip[new_tip$pickup_day_of_week==each,]
      vec1 <- d$pickup_hour
      diff <- setdiff(vec2,vec1)
      for(each in diff){
        val = nrow(d)+1
        d[val,'pickup_day_of_week'] = d[1,'pickup_day_of_week']
        d[val,'pickup_hour']=each
        d[val, 'avg_tip'] = 0.0
      }
      if(i==0){
        new_data <- d
      }else{
        new_data <- rbind(new_data,d)
      }
      i <- i+1
    }
    return(new_data)
  }
  
  # render heatmap for average tip
  output$tip <- renderPlotly({
    if(length(input$airport) > 0){
      
      #group the data by pickup day and hour and calculate the mean tip amount 
      tip_data_week <- plot_data() %>%
        group_by(pickup_day_of_week,pickup_hour)%>%
        summarize(avg_tip = mean(Tip_amount))
      
      tip_data_week$pickup_day_of_week <- factor(tip_data_week$pickup_day_of_week, levels= c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
      new_tip <- tip_data_week[order(tip_data_week$pickup_day_of_week),]
      
      
      days <- unique(new_tip$pickup_day_of_week)
      new_data <- f(new_tip,days, 0)
      new_data <- new_data[order(new_data$pickup_day_of_week,new_data$pickup_hour),]
      
      # create a matrix that can be represented as heatmap 
      smk <- matrix(new_data$avg_tip,ncol = 24,byrow = TRUE)
      colnames(smk) <- unique(new_data$pickup_hour)
      rownames(smk) <- unique(new_data$pickup_day_of_week)
      tip1 <- t(smk)
      plot_ly(x = colnames(tip1), y = rownames(tip1), z = tip1, 
              key = tip1, type = "heatmap", source = "heatplot") %>%
        layout(title = "AverageTip%", xaxis = list(title = "Day Of The Week"), 
               yaxis = list(title = "Time Of The Day"))
    }  
  })

  #On call of reactive function observe function will also be evoked
  observe({
    if(length(input$airport) > 0){
      # group the airport data by airport, pickup longitude and latitude
      pl_data <- plot_data() %>%
        group_by(airport,pick_longitude,pick_latitude) %>%
        summarize(count = n())
      
      #Calculate the intermediate locations when pickup is airport and destination is dropoff latitude and longitude
      flows <- gcIntermediate(plot_data()[,30:31], plot_data()[,10:11], sp = TRUE, addStartEnd = TRUE)
      flows$origins <- plot_data()$airport
      
      pal <- colorFactor(brewer.pal(3, 'Dark2'), flows$origins)
    }  
    
    #Depending upon the check boxes draw the polylines indicating the path from source to destination
    if(length(input$airport)==3){
      leafletProxy("siteMap") %>% clearShapes() %>% clearMarkers() %>%setView(pl_data$pick_longitude[1], pl_data$pick_latitude[1], zoom = 10) %>% addPolylines(data = flows, group = ~origins, color = ~pal(origins)) %>% 
      addMarkers(lng = pl_data$pick_longitude[pl_data$airport==input$airport[1]], lat = pl_data$pick_latitude[pl_data$airport==input$airport[1]], icon = airPlaneIcon, label = paste("Airport", input$airport[1], "\n", "PickUp_Count:", pl_data$count[pl_data$airport==input$airport[1]])) %>%
      addMarkers(lng = pl_data$pick_longitude[pl_data$airport==input$airport[2]], lat = pl_data$pick_latitude[pl_data$airport==input$airport[2]], icon = airPlaneIcon, label = paste("Airport:", input$airport[2], "\n", "PickUp_Count:", pl_data$count[pl_data$airport==input$airport[2]])) %>%
      addMarkers(lng = pl_data$pick_longitude[pl_data$airport==input$airport[3]], lat = pl_data$pick_latitude[pl_data$airport==input$airport[3]], icon = airPlaneIcon, label = paste("Airport", input$airport[3], "\n", "PickUp_Count:", pl_data$count[pl_data$airport==input$airport[3]]))
    }else if(length(input$airport)==2){
      leafletProxy("siteMap") %>% clearShapes() %>% clearMarkers() %>% setView(pl_data$pick_longitude[1], pl_data$pick_latitude[1], zoom = 10) %>% addPolylines(data = flows, group = ~origins, color = ~pal(origins)) %>%
        addMarkers(lng = pl_data$pick_longitude[pl_data$airport==input$airport[1]], lat = pl_data$pick_latitude[pl_data$airport==input$airport[1]], icon = airPlaneIcon, label = paste("Airport", input$airport[1], "\n", "PickUp_Count:", pl_data$count[pl_data$airport==input$airport[1]])) %>%
        addMarkers(lng = pl_data$pick_longitude[pl_data$airport==input$airport[2]], lat = pl_data$pick_latitude[pl_data$airport==input$airport[2]], icon = airPlaneIcon, label = paste("Airport:", input$airport[2], "\n", "PickUp_Count:", pl_data$count[pl_data$airport==input$airport[2]]))
    }else if(length(input$airport)==1){
      leafletProxy("siteMap") %>% clearShapes() %>% clearMarkers() %>% setView(pl_data$pick_longitude[1], pl_data$pick_latitude[1], zoom = 10) %>% addPolylines(data = flows, group = ~origins, color = ~pal(origins)) %>%
        addMarkers(lng = pl_data$pick_longitude[pl_data$airport==input$airport[1]], lat = pl_data$pick_latitude[pl_data$airport==input$airport[1]], icon = airPlaneIcon, label = paste("Airport", input$airport[1], "\n", "PickUp_Count:", pl_data$count[pl_data$airport==input$airport[1]]))
    }else if(length(input$airport)==0){
      leafletProxy("siteMap") %>% clearShapes() %>% clearMarkers()
    }
    
  })
  
  # depending upon the input borough retrieve all the pickup and dropoff zones
  # and the trip counts between them 
  get_zone_data <- function(){
    est_data <- data_hail[(data_hail$pickup_borough==input$borough)&(data_hail$dropoff_borough==input$borough),]
    zone_data <- est_data %>%
      group_by(pickup_zone,dropoff_zone) %>%
      summarize(count = n())
    zone_data$pickup_zone <- as.character(zone_data$pickup_zone)
    zone_data$dropoff_zone <- as.character(zone_data$dropoff_zone)
    v <- zone_data$pickup_zone
    v1 <- zone_data$dropoff_zone
    v2 <- zone_data$count
    df <- data.frame(v,v1,v2, stringsAsFactors=FALSE)
    colnames(df) <- c("pickup_zone","dropoff_zone", "count")
    df <- df[order(-df$count),]
    df <- df[1:65,]
    return(df)
  }
  
  #Render chord diagram indicating the trips between the zones for the selected borough
  output$zoneCount <- renderChorddiag({
    df <- get_zone_data()
    z_chord <- xtabs(count~pickup_zone+dropoff_zone, data=df)
    c <- colnames(z_chord)
    r <- rownames(z_chord)
    z_chord <- unname(z_chord)
    colnames(z_chord) <- c
    rownames(z_chord) <- r
    chorddiag(z_chord, type = "bipartite", showTicks = F, groupnameFontsize = 10, groupnamePadding = 10, margin = 90)
  })
  
  
  #Update the Pickup and dropoff zone dropdowns depending upon the selected borough
  observe({
    zone_data <- get_zone_data()
    pickup <- unique(zone_data$pickup_zone)
    dropoff <- unique(zone_data$dropoff_zone)

    updateSelectInput(session, "pickupZone",
                      label = "NY Pickup Zone",
                      choices = pickup
    )

    updateSelectInput(session, "dropZone",
                      label = "NY Dropoff Zone",
                      choices = dropoff
    )

  })
  
  #Retrieve all the unique zones in a particular borough
  get_zone_lat_lon <- function(){
    zone_data <- get_zone_data()
    pickup <- unique(zone_data$pickup_zone)
    
    zn_df <- data.frame(unique(pickup),stringsAsFactors=FALSE)
    colnames(zn_df) <- c("pickup_zone")
    
    for(i in 1:nrow(zn_df)){
      zn_df[i, 'pickup_longitude'] <- taxi_zones[taxi_zones$zone==zn_df[i,'pickup_zone'],'longitude']
      zn_df[i, 'pickup_latitude'] <- taxi_zones[taxi_zones$zone==zn_df[i,'pickup_zone'],'latitude']
    }
    return(zn_df)
  }
  
  #Render a leaflet focusing on new york and plot all the zones as circle markes on it 
  output$map <- renderLeaflet({
    
    zn_df <- get_zone_lat_lon()
    leaflet() %>% 
      addTiles() %>%
      addProviderTiles('CartoDB.Positron')%>%
      addCircleMarkers(data = zn_df, ~zn_df$pickup_longitude, ~zn_df$pickup_latitude, label=as.character(zn_df$pickup_zone),radius=2,color = 'red')
  })
  
  # render path on leaflet depending upon the pickup and drop off zones and selected hour of the day 
  observe({
    pickup_zone <- input$pickupZone
    dropoff_zone <- input$dropZone
    
    # When Pickup and dropoff are different render a path indicating the average cost on the leaflet
    if(pickup_zone != dropoff_zone){
      est_data <- data_hail[(data_hail$pickup_borough==input$borough)&(data_hail$dropoff_borough==input$borough)
                            &(data_hail$pickup_zone==pickup_zone)&(data_hail$dropoff_zone==dropoff_zone),]
      
      pickup_latitude <- taxi_zones[taxi_zones$zone==pickup_zone,'latitude']
      pickup_longitude <- taxi_zones[taxi_zones$zone==pickup_zone,'longitude']
      drop_longitude <- taxi_zones[taxi_zones$zone==dropoff_zone,'longitude']
      drop_latitude <- taxi_zones[taxi_zones$zone==dropoff_zone,'latitude']

      p1 <- c(pickup_longitude,pickup_latitude)
      p2 <- c(drop_longitude,drop_latitude)
      
      flows <- gcIntermediate(p1,p2, sp = TRUE, addStartEnd = TRUE)
      flows$pickup_zone <- pickup_zone
      flows$drop_zone <- dropoff_zone

      est_data_grp <- est_data %>%
        group_by(pickup_hour) %>%
        summarize(avg_cost = mean(Fare_amount))
      
      cost <- as.numeric(est_data_grp[est_data_grp$pickup_hour == input$time,'avg_cost'])
      
      if(is.na(cost)){
        cost <- mean(est_data_grp$avg_cost)
      }
      
      hover <- paste0(flows$pickup_zone, " to ", 
                      flows$drop_zone,
                    ' Average Cost:',round(cost,2))
      
      leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>%
        setView(pickup_longitude, pickup_latitude, zoom = 13) %>% 
        addPolylines(data = flows, label = hover, color = 'green')%>%
        addCircleMarkers(pickup_longitude,pickup_latitude,label = pickup_zone, radius=3,color = 'red')%>%
        addCircleMarkers(drop_longitude,drop_latitude,label = dropoff_zone, radius=3,color = 'red')
      
      
    } else {
      #When Pickup and dropoff zones are same render a circle marker indicating the average travel cost
      zn_df <- get_zone_lat_lon()
      est_data <- data_hail[(data_hail$pickup_borough==input$borough)&(data_hail$dropoff_borough==input$borough)
                            &(data_hail$pickup_zone==pickup_zone)&(data_hail$dropoff_zone==dropoff_zone),]
      
      pickup_latitude <- taxi_zones[taxi_zones$zone==pickup_zone,'latitude']
      pickup_longitude <- taxi_zones[taxi_zones$zone==pickup_zone,'longitude']
      
      est_data_grp <- est_data %>%
        group_by(pickup_hour) %>%
        summarize(avg_cost = mean(Fare_amount))
      
      cost <- as.numeric(est_data_grp[est_data_grp$pickup_hour == input$time,'avg_cost'])
      
      if(is.na(cost)){
        cost <- mean(est_data_grp$avg_cost)
      }
      
      hover <- paste0(pickup_zone, " to ", 
                      dropoff_zone,
                      ' Average Cost:',round(cost,2))
      
      leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>%
        addCircleMarkers(data = zn_df, ~zn_df$pickup_longitude, ~zn_df$pickup_latitude, label=as.character(zn_df$pickup_zone),radius=2,color = 'red') %>%
        setView(pickup_longitude, pickup_latitude, zoom = 13) %>%
        addCircleMarkers(pickup_longitude, pickup_latitude, label=hover,radius=5,color = 'green')
    }

  })
  
  # render line chart indicating the trip duration with respect to hour of the day for selected borough and zones
  output$line <- renderPlot({
    pickup_zone <- input$pickupZone
    dropoff_zone <- input$dropZone
    est_data <- data_hail[(data_hail$pickup_borough==input$borough)&(data_hail$dropoff_borough==input$borough)
                          &(data_hail$pickup_zone==pickup_zone)&(data_hail$dropoff_zone==dropoff_zone),]
    
    est_data_grp <- est_data %>%
      group_by(pickup_hour) %>%
      summarize(avg_cost = mean(Fare_amount), avg_time = mean(tripTimeMins))
    
    ggplot(data=est_data_grp, aes(x=pickup_hour, y=avg_time))+geom_line(col='red')+xlim(0,23)+labs(x="Hour Of The Day",y="Trip Duration In Mins",title = "Trip Duration with respect to Hour Of the Day")
    
  })
 
}