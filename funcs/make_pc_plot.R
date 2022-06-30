
### Principal components plot

make_pc_plot = function(pc){
    ggbiplot(
        pcobj = pc,
        obs.scale = 1,
        var.scale = 1,
        groups = as.factor(df_train_pcalr[, response_var]),
        ellipse = TRUE,
        circle = TRUE,
        ellipse.prob = 0.68
    ) +
        scale_color_discrete(name = "") +
        theme(
            legend.direction = "horizontal",
            legend.position = "top"
        )
}
