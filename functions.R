psrc_line_chart <- function(df, x, y, fill, lwidth=1, colors, labels=scales::label_comma(), breaks=NULL, title=NULL, source=NULL, xdate="no") {

      c <- ggplot(data=df,
            aes(x=.data[[x]],
                y=.data[[y]],
                group=.data[[fill]]))  + 
            geom_line(aes(color=.data[[fill]]), linewidth=lwidth, linejoin = "round", na.rm=TRUE) +
            scale_fill_manual(values = colors) +
            scale_color_manual(values = colors)  +
            scale_y_continuous(labels = labels, expand=expansion(mult = c(0, .2)))  +   # expand is to accommodate value labels
            labs(title=title, caption=source) +
            psrcplot::psrc_style() +
            theme(plot.title = element_text(family = "Poppins", face = "bold", size = 14),
                  panel.grid.major.y = element_line(linewidth = 0.1, color="#dcdcdc"),
                  panel.grid.major.x = element_line(linewidth = 0.1, color="#dcdcdc"),
                  plot.caption =  element_text(family="Poppins", face="plain", size = 10, color="black", hjust=0),
                  plot.caption.position = "plot")

      if (xdate=="yes"){
            c <- c + 
                  scale_x_date(labels = scales::date_format("%b-%Y")) +
                  theme(axis.title.x=element_blank()) +
                  theme(axis.title.y=element_blank())
          }else{
            c <- c + 
                  scale_x_continuous(n.breaks=breaks) +
                  theme(axis.title.x=element_blank()) +
                  theme(axis.title.y=element_blank())
          }

  return(c)

}

psrc_bar_chart <- function(df, x, y, fill, colors, labels=scales::label_comma(), title=NULL, source=NULL, pos="dodge", legend = TRUE) {

      c <- ggplot(data=df,
            aes(x=.data[[x]],
                y=.data[[y]],
                fill=.data[[fill]]))  + 
            geom_bar(position=pos, stat="identity", na.rm=TRUE) +
            scale_fill_manual(values = colors) +
            scale_y_continuous(labels = labels, expand=expansion(mult = c(0, .2)))  +   # expand is to accommodate value labels
            labs(title=title, caption=source) +
            psrcplot::psrc_style() +
            theme(plot.title = element_text(family = "Poppins", face = "bold", size = 14),
                  panel.grid.major.y = element_line(linewidth = 0.1, color="#dcdcdc"),
                  panel.grid.major.x = element_line(linewidth = 0.1, color="#dcdcdc"),
                  plot.caption =  element_text(family="Poppins", face="plain", size = 10, color="black", hjust=0),
                  plot.caption.position = "plot",
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            coord_flip()
      
      if (legend == FALSE) {

            c <- c + theme(legend.position = "none")
      }

      return (c)
}

psrc_column_chart <- function(df, x, y, fill, colors, labels=scales::label_comma(), title=NULL, source=NULL, pos="dodge", legend = TRUE) {

      c <- ggplot(data=df,
            aes(x=.data[[x]],
                y=.data[[y]],
                fill=.data[[fill]]))  + 
            geom_bar(position=pos, stat="identity", na.rm=TRUE) +
            scale_fill_manual(values = colors) +
            scale_y_continuous(labels = labels, expand=expansion(mult = c(0, .2)))  +   # expand is to accommodate value labels
            labs(title=title, caption=source) +
            psrcplot::psrc_style() +
            theme(plot.title = element_text(family = "Poppins", face = "bold", size = 14),
                  panel.grid.major.y = element_line(linewidth = 0.1, color="#dcdcdc"),
                  panel.grid.major.x = element_line(linewidth = 0.1, color="#dcdcdc"),
                  plot.caption =  element_text(family="Poppins", face="plain", size = 10, color="black", hjust=0),
                  plot.caption.position = "plot",
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank())
      
      if (legend == FALSE) {

            c <- c + theme(legend.position = "none")
      }
      
      return (c)
}

psrc_data_table <- function(df, title, num_cols, per_cols, lab_color) {

      tbl_title <- paste0("**",title,"**")

      tbl <- gt(df) |>
            tab_header(title = md(tbl_title)) |>
            fmt_number(columns=num_cols, decimals = 0) |>
            fmt_percent(columns=per_cols, decimals = 0) |>
            opt_row_striping() |>
            cols_align(align = "center", columns = c(num_cols, per_cols)) |>
            tab_options(column_labels.background.color = lab_color) |>
            tab_style(style = cell_borders(sides = c("left","right"), color = "white", weight = px(2), style = "solid"), locations = cells_body()) |>
            tab_style(style = cell_borders(sides = c("left","right"), color = "white", weight = px(2), style = "solid"), locations = cells_column_labels()) |>
            tab_style(style = cell_borders(sides = c("top","left","right"), color = "white", weight = px(2), style = "solid"), locations = cells_title()) |>
            opt_align_table_header(align = "left") |>
            tab_options(table.width = pct(100),
                        table.font.names = "Poppins",
                        heading.title.font.size = px(16))
      
      return(tbl)
}

psrc_charging_stations_map <- function(lyr, col) {

      public_lyr <- lyr |> filter(str_detect(group, "Public"))
      private_lyr <- lyr |> filter(str_detect(group, "Private"))
  
      working_map <- leaflet(options = leafletOptions(zoomControl = FALSE)) |>
            
            addProviderTiles(providers$CartoDB.Positron) |>
    
            addCircleMarkers(data = public_lyr, color = col[[1]], radius = 1, fillOpacity = 1, opacity = 1) |>
            
            addCircleMarkers(data = private_lyr, color = col[[2]], radius = 1, fillOpacity = 1, opacity = 1) |>
            
            #setView(lng = -122.257, lat = 47.615, zoom = 8.5) |>
            
            addLegend(colors= col,
                  labels= c("Public", "Private"),
                  position = "bottomleft")
  
  return(working_map)

}
