
### ROC curve and AUC value

make_roc_curve = function(probs, y_obs){
    
    require(ROCR)
    
    ROCRpred = ROCR::prediction(predictions = probs,
                                labels = y_obs)
    ROCRperf = ROCR::performance(prediction.obj = ROCRpred,
                                 measure = "tpr",
                                 x.measure = "fpr")
    df_roc = data.frame("False_positive_rate" = ROCRperf@x.values[[1]],
                        "True_positive_rate" = ROCRperf@y.values[[1]],
                        "Thresholds" = ROCRperf@alpha.values[[1]]) %>% 
                 dplyr::filter(Thresholds >= 0 & 
                               Thresholds <= 1)
    preds = ROCR::prediction(predictions = probs, 
                             labels = y_obs)
    auc = attr(x = ROCR::performance(prediction.obj = preds,
                                     measure = "auc"), 
               which = "y.values")[[1]]
    
    my_palette = colorRampPalette(c("#111539", "#97A1D9"))
    plot_ly() %>%
        add_trace(
            data = df_roc,
            x = ~False_positive_rate,
            y = ~True_positive_rate,
            color = my_palette(3)[2],
            colors = my_palette(3)[2],
            type = "scatter",
            mode = "lines",
            fill = "tozeroy",
            fillcolor = my_palette(3)[3],
            line = list(
                width = 5
            ),
            hovertemplate = paste0("<b>False positive rate: %{x} <br> ",
                                   "True positive rate: %{y} </b><extra></extra>")
        ) %>%
        add_trace(
            x = c(0, 1),
            y = c(0, 1),
            type = "scatter",
            mode = "lines",
            line = list(
                width = 2,
                color = "red",
                dash = "dash"
            )
        ) %>%
        layout(
            title = list(
                text = paste0("AUC = ", round(auc, digits = 3)),
                titlefont = list(
                    size = 20
                ),
                tickfont = list(
                    size = 18
                )
            ),
            xaxis = list(
                title = paste0("<b>False positive rate</b>"),
                titlefont = list(
                    size = 20
                ),
                tickfont = list(
                    size = 18
                ),
                categoryorder = "array"
            ),
            yaxis = list(
                title = paste0("<b>True positive rate</b>"),
                titlefont = list(
                    size = 20
                ),
                tickfont = list(
                    size = 18
                )
            ),
            margin = list(
                l = 10,
                r = 10,
                t = 50,
                b = 10
            ),
            hoverlabel = list(
                font = list(
                    size = 18
                )
            ),
            showlegend = FALSE
        )
}

