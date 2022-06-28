
### Accuracy of binary classification

score_accuracy = function(y_pred, y_real){
    m = table(y_pred,
              y_real,
              dnn = c("Prediction", "Truth"))
    estimated_score = (m[1, 1] + m[2, 2])/(sum(m))
    return(estimated_score)
}
