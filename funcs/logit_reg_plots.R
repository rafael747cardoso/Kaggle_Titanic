
### Logistic regression plots

logit_reg_plots = function(df_model){
    
    # Train/test split:
    q = 0.5
    n_all = nrow(df_model)
    inds = sample(x = 1:n_all,
                  size = trunc(q*n_all))
    df_train = df_model[inds, ]
    df_test = df_model[-inds, ]
    
    # Evaluation:
    fit = glm(formula = paste(response_var, "~.",
                              collapse = ""),
              family = "binomial",
              data = df_train)
    probs = predict(object = fit,
                    newdata = df_test,
                    type = "response")
    threshold = 0.5
    y_pred = ifelse(probs > threshold,
                    1,
                    0)
    conf_matrix = table(y_pred,
                        df_test[, response_var],
                        dnn = c("Predicted", "Actual"))

    score = score_accuracy(y_pred = y_pred,
                           y_real = df_test[, response_var])

    df_pred = data.frame("probs" = as.numeric(probs),
                         "y_pred" = y_pred,
                         "y_real" = as.numeric(as.character(df_test[, response_var]))) %>%
                  dplyr::arrange(probs)
    df_pred$rank = 1:nrow(df_pred)
    
    s = sum(probs)
    df_TN = df_pred %>%
                dplyr::filter(y_pred == 0 & y_real == 0) %>%
                dplyr::select(probs) %>%
                dplyr::mutate(pop = "TN")
    df_FN = df_pred %>%
                dplyr::filter(y_pred == 0 & y_real == 1) %>%
                dplyr::select(probs) %>%
                dplyr::mutate(pop = "FN")
    df_TP = df_pred %>%
                dplyr::filter(y_pred == 1 & y_real == 1) %>%
                dplyr::select(probs) %>%
                dplyr::mutate(pop = "TP")
    df_FP = df_pred %>%
                dplyr::filter(y_pred == 1 & y_real == 0) %>%
                dplyr::select(probs) %>%
                dplyr::mutate(pop = "FP")
    
    df_pops = rbind(df_TN,
                    df_FN,
                    df_TP,
                    df_FP)

    ### Plots
    
    # Confusion matrix plot:
    my_colors = c("#ce80dc", "#872ed5")
    my_palette = colorRampPalette(colors = my_colors)
    df_cm = as.data.frame(conf_matrix)
    p1 = ggplot(
        data = df_cm,
            aes(
                x = Predicted,
                y = Actual,
                fill = Freq
            )
        ) +
        geom_tile() +
        coord_fixed(
            ratio = 1
        ) +
        scale_fill_gradientn(
            colors = my_palette(100),
            na.value = "white",
            guide = guide_colorbar(
                direction = "vertical",
                barheight = unit(
                    x = "200",
                    units = "pt"
                ),
                draw.ulim = FALSE,
                title.position = "top",
                title.hjust = 0.5,
                label.hjust = 0.5            
            )
        ) +
        geom_text(
            aes(
                x = Predicted, 
                y = Actual, 
                label = Freq
            ),
            color = "white",
            size = 7
        ) +
        theme(
            legend.title = element_text(
                size = 15,
                face = "bold"
            ),
            legend.text = element_text(
                size = 13
            ),
            axis.text.x = element_text(
                size = 14,
                angle = 0,
                hjust = 1,
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
            )
        ) +
        labs(
            x = "Predicted",
            y = "Actual",
            fill = paste0("Counts")
        )
    
    # Two population densities:
    p2 = ggplot() +
        geom_density(
            data = df_pops,
            aes(
                x = probs,
                color = pop
            )
        ) +
        geom_vline(
            aes(
                xintercept = threshold,
                color = "Threshold"
            ),
            size = 1,
            linetype = "dashed",
            show.legend = TRUE
        ) +
        scale_color_manual(
            values = c("FN" = "#ee321a",
                       "FP" = "#1d87ee",
                       "TN" = "#e98638",
                       "TP" = "#55b5d6",
                       "Threshold" = "gray"),
            name = "Populations"
        ) +
        theme(
            axis.text.x = element_text(
                size = 14,
                angle = 0, 
                hjust = 1,
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
            legend.title = element_text(
                size = 15,
                face = "bold"
            ),
            legend.text = element_text(
                size = 14
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
            )
        ) +
        xlab("Probability") +
        ylab("Density")
    
    # ROC curve:
    p3 = make_roc_curve(probs = probs,
                        y_obs = df_test[, response_var])

    # Ranking:
    p4 = ggplot() +
        geom_point(
            data = df_pred[df_pred$y_real == 0, ],
            aes(
                x = rank,
                y = probs,
                colour = "Actual 0"
            ),
            size = 2,
            alpha = 0.8
        ) +
        geom_point(
            data = df_pred[df_pred$y_real == 1, ],
            aes(
                x = rank,
                y = probs,
                colour = "Actual 1"
            ),
            size = 4,
            alpha = 0.3
        ) +
        geom_hline(
            aes(
                yintercept = threshold,
                color = "Threshold"
            ),
            size = 1,
            linetype = "dashed",
            show.legend = TRUE
        ) +
        scale_color_manual(
            values = c("Actual 0" = "#ec3b20",
                       "Actual 1" = "#2076ec",
                       "Threshold" = "gray"),
            name = ""
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
            legend.title = element_text(
                size = 15,
                face = "bold"
            ),
            legend.text = element_text(
                size = 14
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
            )
        ) +
        xlab("Rank") +
        ylab("Prediction probability")
    
    # Assemble the plots:
    grid.arrange(p1, p2, p3, p4,
                 nrow = 2,
                 ncol = 2,
                 top = textGrob("Logistic Regression",
                                gp = gpar(
                                    fontsize = 16,
                                    font = 3
                                )
                            )
                 )
}

