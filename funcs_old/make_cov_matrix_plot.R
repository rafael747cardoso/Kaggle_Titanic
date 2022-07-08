
### Covariance matrix plot

make_cov_matrix_plot = function(df, response, log_scale){
    
    require(reshape2)
    require(scales)
    
    # Covariances:
    df_cov = df %>%
                 dplyr::select(-all_of(response)) %>% 
                 tidyr::drop_na() %>%
                 cov()
    df_plot = melt(data = df_cov,
                   value.name = "Vars_cov")
    covs = df_plot$Vars_cov
    
    # Scale:
    log_covs = c()
    for(i in 1:length(covs)){
        if(covs[i] == 0){
            log_covs_i = 0
        }
        if(covs[i] > 0){
            log_covs_i = log(covs[i])
        }
        if(covs[i] < 0){
            log_covs_i = -log(abs(covs[i]))
        }
        log_covs = c(log_covs,
                     log_covs_i)
    }
    if(log_scale){
        df_plot$Vars_cov = log_covs
        legend_title = "Log(Covariance)"
    } else{
        df_plot$Vars_cov = covs
        legend_title = "Covariance"
    }
    
    # Colorscale:
    colorscale = my_colorscale(low = "#4249ef",
                               zero = "#FFFFFF",
                               high = "#ef5642",
                               Z = df_plot$Vars_cov)
    
    # Plot:
    plot_ly(
        data = df_plot,
        x = ~Var1,
        y = ~Var2,
        z = ~Vars_cov,
        type = "heatmap",
        colorscale = colorscale,
        colorbar = list(
            title = paste0("<b>", legend_title, "</b>"),
            len = 1
        ),
        hovertemplate = paste0("<b>",
                               "%{x}<br>",
                               "%{y}<br>",
                               legend_title, ": %{z:}</b><extra></extra>")
    ) %>%
    layout(
        xaxis = list(
            title = "",
            tickfont = list(
                size = 18
            ),
            categoryorder = "array"
        ),
        yaxis = list(
            title = "",
            tickfont = list(
                size = 18
            )
        ),
        hoverlabel = list(
            font = list(
                size = 18
            )
        )
    )

}
