
### Cross-validation plot for kNN

make_cv_knn_plot = function(df_ks, df_best){
    ggplot() +
    geom_point(
        data = df_ks,
        aes(
            x = ks,
            y = mean_cv_CER
        ),
        color = "gray",
        size = 1,
        alpha = 0.7
    ) +
    geom_line(
        data = df_ks,
        aes(
            x = ks,
            y = mean_cv_CER
        ),
        color = "red",
        size = 1
    ) +
    geom_point(
        data = df_best,
        aes(
            x = ks,
            y = mean_cv_CER,
            colour = "Best model"
        ),
        size = 2,
        alpha = 1
    ) +
    scale_colour_manual(
        name = paste0("k = ", df_best$ks[1]),
        values = c("Best model" = "blue")
    ) +
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
        legend.text = element_text(
            size = 14
        ),
        legend.background = element_rect(
            fill = "transparent"
        )
    ) +
    xlab("k") +
    ylab("Mean CV CER")
}
