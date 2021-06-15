#' Timeseries plot of E4 data
#' @description Creates 4 graphs of the E4 data, as used in the Empatica dashboard.
#' @param data List of e4 data.
#' @param main_title Main title of the plot, defaults to empty string.
#' @param calendar_data Name to give to the timeseries data.
#' @param average_lines Do you want to add averages to the plot
#' @param yaxis_ranges What Range do you want to give the y axis?
#' @param colours The colours of the graph, defaults to blue, orange, red, green
#' @export
e4_timeseries_plot <- function(data, 
                               main_title = "",
                               calendar_data = NULL,
                               series_options = NULL){
  
  
  my_dygraph <- function(ts, 
                         main_title = FALSE, 
                         ylab_title = FALSE, 
                         draw_x_axis = FALSE, 
                         color = "black",
                         y_line_type = NULL,
                         y_line_val = 0,
                         events = calendar_data,
                         events_label = FALSE,
                         yaxis_range = NULL
  ){
    
    
    begin_time <- min(index(ts)) - 30*60
    
    out <- dygraph(ts, main = main_title, group = "plot2", 
                   ylab = ylab_title, height = 150, width = 900) %>%   
      dyHighlight(highlightCircleSize = 5) %>%
      dyOptions(drawPoints = FALSE, 
                drawXAxis = draw_x_axis,
                connectSeparatedPoints = TRUE,
                colors = color) %>%
      dyUnzoom()
    # dyAxis(name = "x", valueRange = c(begin_time, NULL))
    
    
    if(!is.null(y_line_type)){
      
      if(y_line_type == "mean"){
        y_line_val <- mean(ts, na.rm = TRUE) 
        label <- "Mean"
      } else {
        label <- NULL
      }
      
      out <- out %>%
        dyLimit(y_line_val, label, strokePattern = "dashed", color = color)
      
    }
    
    if(!is.null(yaxis_range)){
      out <- out %>%
        dyAxis("y", valueRange = yaxis_range)
    }
    
    
    if(!is.null(events)){
      
      # 'Start' events
      for(i in 1:nrow(events)){
        
        txt <- ifelse(events_label, events$Text[i], "")
        
        out <- out %>%
          dyEvent(events$Start[i], txt, labelLoc="bottom")
        
      }
      
      
      # Shading events
      eventsub <- filter(events, !is.na(Start) & !is.na(End))
      if(nrow(eventsub)){
        for(i in 1:nrow(eventsub)){
          
          out <- out %>%
            dyShading(from = eventsub$Start[i],
                      to = eventsub$End[i],
                      color = eventsub$Color[i])
          
        }
      }
      
    }
    
    return(out)
  }
  
  list(
    my_dygraph(data$EDA, 
               main_title = main_title, 
               ylab_title = "EDA", 
               events_label = TRUE,
               y_line_type = series_options$EDA$line_type,
               y_line_val = series_options$EDA$custom_y_val,
               yaxis_range = series_options$EDA$yaxis_range,
               color = .cc$visualisation$eda$color),
    my_dygraph(data$HR, 
               ylab_title = "HR", 
               y_line_type = series_options$HR$line_type,
               y_line_val = series_options$HR$custom_y_val,
               yaxis_range = series_options$HR$yaxis_range,
               color = .cc$visualisation$hr$color),
    my_dygraph(data$TEMP, 
               ylab_title = "Temperature", 
               y_line_type = series_options$TEMP$line_type,
               y_line_val = series_options$TEMP$custom_y_val,
               yaxis_range = series_options$TEMP$yaxis_range,
               color = .cc$visualisation$temp$color),
    my_dygraph(data$MOVE, 
               draw_x_axis = TRUE, 
               ylab_title = "Movement", 
               y_line_type = series_options$MOVE$line_type,
               y_line_val = series_options$MOVE$custom_y_val,
               yaxis_range = series_options$MOVE$yaxis_range,
               color = .cc$visualisation$move$color) %>%
      dyRangeSelector()
  )
}




