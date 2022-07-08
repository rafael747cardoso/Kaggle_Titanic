
### ROC curve and AUC value

make_roc_curve = function(probs, y_obs){

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
               which = "y.values")[[1]] %>%
              round(., 3)
    
    my_palette = colorRampPalette(c("#111539", "#97A1D9"))
    ggplot(
        data = df_roc,
        aes(
            x = False_positive_rate,
            y = True_positive_rate
        )
    ) +
    geom_line(
        show.legend = FALSE,
        color = my_palette(3)[2],
        size = 2
    ) +
    geom_area(
        position = "identity",
        fill = my_palette(3)[3]
    ) +
    geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        show.legend = FALSE,
        color = "white",
        size = 1.1,
        alpha = 0.6
    ) +
    geom_point(
        aes(
            x = 0,
            y = 0,
            colour = " "
        ),
        size = 0
    ) +
    scale_colour_manual(
        values = c(" " = "white"),
        name = paste0("AUC = ", auc)
    ) +
    coord_fixed(ratio = 1) + 
    theme(
        axis.text.x = element_text(
            size = 14,
            angle = 0,
            hjust = 0.5,
            vjust = 1
        ),
        axis.text.y = element_text(
            size = 14
        ),
        axis.title.x = element_text(
            size = 15,
            face = "bold"
        ),
        axis.title.y = element_text(
            size = 15,
            face = "bold"
        ),
        panel.background = element_rect(
            fill = "white"
        ),
        panel.grid.major = element_line(
            size = 0.2,
            linetype = "solid",
            colour = "#eaeaea"
        ),
        panel.grid.minor = element_line(
            size = 0.1,
            linetype = "solid",
            colour = "#eaeaea"
        ),
        plot.margin = margin(
            t = 0, 
            r = 5,
            b = 5, 
            l = 10,
            unit = "pt"
        ),
        legend.title = element_text(
            size = 14
        ),
        legend.background = element_rect(
            fill = "transparent"
        ),
        legend.key = element_rect(
            fill = "white"
        ),
        legend.position = "top"
    ) +
    xlab("False positive rate") +
    ylab("True positive rate")
    
}

