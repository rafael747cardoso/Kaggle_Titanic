
### Density with mean and standard deviations for 2 sets on the same variable

plot_density_2_sets = function(X, ind_1, ind_2){
    
    # Treat categoric:
    if(!is.numeric(X)){
        X[is.na(X)] = "Missing"
        X[X == ""] = "Missing"
        XX = X
        lvls = unique(X)
        for(l in 1:length(lvls)){
            XX[XX == lvls[l]] = l
        }
        df_lvls = data.frame(
            X_num = as.numeric(XX),
            X_name = X,
            stringsAsFactors = FALSE
        )
        X = df_lvls$X_num
        is_cat = TRUE
    } else{
        is_cat = FALSE
    }
    
    X_1 = X[ind_1]
    mean_X_1 = mean(X_1)
    std_X_1 = sd(X_1)
    dens_1 = density(X_1)
    color_x_1 = "#f98c03"
    color_fill_1 = "rgba(249, 140, 3, 0.3)"
    color_mean_1 = "#f92703"
    color_minus_std_1 = "#d1352e"
    color_plus_std_1 = "#aa4945"

    X_2 = X[ind_2]
    mean_X_2 = mean(X_2)
    std_X_2 = sd(X_2)
    dens_2 = density(X_2)
    color_x_2 = "#0e5ef4"
    color_fill_2 = "rgba(14, 94, 244, 0.3)"
    color_mean_2 = "#1536d7"
    color_minus_std_2 = "#4b5db3"
    color_plus_std_2 = "#5b76f8"

    npoints = 2
    p = plot_ly() %>%
        # X_1:
        add_trace(
            x = dens_1$x,
            y = dens_1$y,
            type = "scatter",
            mode = "lines",
            fill = "tozeroy",
            color = color_x_1,
            colors = c(color_x_2, color_x_1),
            opacity = 0.9,
            hovertemplate = paste0("<b>X_1: %{x:,}<br>",
                                   "Density: %{y:,}</b><extra></extra>"),
            name = "Data 1"
        ) %>%
        add_trace(
            x = rep(mean_X_1 + std_X_1, npoints),
            y = seq(from = min(dens_1$y),
                    to = 1.1*max(dens_1$y),
                    length.out = npoints),
            type = "scatter",
            mode = "lines",
            fill = "toself",
            fillcolor = color_fill_1,
            line = list(
                color = color_plus_std_1
            ),
            name = "Mean + Std 1"
        ) %>%
        add_trace(
            x = rep(mean_X_1 - std_X_1, npoints),
            y = seq(from = min(dens_1$y),
                    to = 1.1*max(dens_1$y),
                    length.out = npoints),
            type = "scatter",
            mode = "lines",
            fill = "tonextx",
            fillcolor = color_fill_1,
            line = list(
                color = color_minus_std_1
            ),
            name = "Mean - Std 1"
        ) %>%
        add_trace(
            x = rep(mean_X_1, npoints),
            y = seq(from = min(dens_1$y),
                    to = 1.1*max(dens_1$y),
                    length.out = npoints),
            type = "scatter",
            mode = "lines",
            line = list(color = color_mean_1),
            name = "Mean 1"
        ) %>%
        # X_2:
        add_trace(
            x = dens_2$x,
            y = dens_2$y,
            type = "scatter",
            mode = "lines",
            fill = "tozeroy",
            color = color_x_2,
            colors = c(color_x_2, color_x_1),
            opacity = 0.9,
            hovertemplate = paste0("<b>X_2: %{x:,}<br>",
                                   "Density: %{y:,}</b><extra></extra>"),
            name = "Data 2"
        ) %>%
        add_trace(
            x = rep(mean_X_2 + std_X_2, npoints),
            y = seq(from = min(dens_2$y),
                    to = 1.1*max(dens_2$y),
                    length.out = npoints),
            type = "scatter",
            mode = "lines",
            fill = "toself",
            fillcolor = color_fill_2,
            line = list(
                color = color_plus_std_2
            ),
            name = "Mean + Std 2"
        ) %>%
        add_trace(
            x = rep(mean_X_2 - std_X_2, npoints),
            y = seq(from = min(dens_2$y),
                    to = 1.1*max(dens_2$y),
                    length.out = npoints),
            type = "scatter",
            mode = "lines",
            fill = "tonextx",
            fillcolor = color_fill_2,
            line = list(
                color = color_minus_std_2
            ),
            name = "Mean - Std 2"
        ) %>%
        add_trace(
            x = rep(mean_X_2, npoints),
            y = seq(from = min(dens_2$y),
                    to = 1.1*max(dens_2$y),
                    length.out = npoints),
            type = "scatter",
            mode = "lines",
            line = list(color = color_mean_2),
            name = "Mean 2"
        )
    if(is_cat){
        p = p  %>%
        layout(
            xaxis = list(
                title = paste0("<b>X</b>"),
                tickmode = "array",
                tickvals = df_lvls$X_num,
                ticktext = df_lvls$X_name
            ),
            yaxis = list(
                title = paste0("<b>Density</b>")
            ),
            showlegend = TRUE
        )
    } else{
        p = p  %>%
        layout(
            xaxis = list(
                title = paste0("<b>X</b>")
            ),
            yaxis = list(
                title = paste0("<b>Density</b>")
            ),
            showlegend = TRUE
        )
    }
    
    return(p)
}

