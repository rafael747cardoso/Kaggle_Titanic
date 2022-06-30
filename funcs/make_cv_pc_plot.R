
### CV CER by number of PC

make_cv_pc_plot = function(df_eval, df_best){
    ggplot() +
    geom_point(
        data = df_eval,
        aes(
            x = num_pc,
            y = cv_CER
        ),
        color = "gray",
        size = 1,
        alpha = 0.7
    ) +
    geom_line(
        data = df_eval,
        aes(
            x = num_pc,
            y = cv_CER
        ),
        color = "red",
        size = 1
    ) +
    geom_point(
        data = df_best,
        aes(
            x = num_pc,
            y = cv_CER,
            colour = "Best model"
        ),
        size = 2,
        alpha = 1
    ) +
    scale_colour_manual(
        name = paste0("p = ", df_best$num_pc[1]),
        values = c("Best model" = "blue")
    ) +
    scale_x_continuous(
        breaks = get_integer_breaks(df_eval$num_pc)
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
    xlab("Number of PC") +
    ylab("CV CER")

}
