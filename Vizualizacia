doGraph_BoxPlot <- 
function (real, pred, facetby = NULL) 
{
    if (is.null(facetby)) {
        ggplot(data = gather(data = data.frame(real, pred), typ, 
            prijem, pred:real), aes(x = typ, y = prijem)) + geom_boxplot() + 
            theme_bw() + labs(x = "Predict vs Real", y = "Value")
    }
    else {
        ggplot(data = gather(data = data.frame(real, pred, facetby), 
            typ, prijem, pred:real), aes(x = typ, y = prijem)) + 
            geom_boxplot() + theme_bw() + labs(x = "Predict vs Real", 
            y = "Value") + facet_wrap(~facetby)
    }
}



doGraph_KS <- 
function (real, pred, facetby = NULL) 
{
    if (is.null(facetby)) {
        ggplot(data = data.frame(real, pred), aes(x = pred, colour = factor(real))) + 
            stat_ecdf(size = 1) + theme_bw() + labs(x = "Predict", 
            colour = "Real", y = "ECDF") + ggtitle("Kolmogorov-Smirnov graph")
    }
    else {
        ggplot(data = data.frame(real, pred, facetby), aes(x = pred, 
            colour = factor(real))) + stat_ecdf(size = 1) + theme_bw() + 
            labs(x = "Predict", colour = "Real", y = "ECDF") + 
            ggtitle("Kolmogorov-Smirnov graph") + facet_wrap(~facetby)
    }
}


doGraph_BarPlot <- 
function (real, pred, position = "fill", facetby = NULL) 
{
    if (is.null(facetby)) {
        ggplot(data = data.frame(real, pred), aes(x = pred, fill = factor(real))) + 
            geom_histogram(alpha = 1, position = position, colour = "black", 
                binwidth = 0.05) + theme_bw() + labs(x = "Predict", 
            fill = "Real") + ggtitle("Good/Bad barplot")
    }
    else {
        ggplot(data = data.frame(real, pred, facetby), aes(x = pred, 
            fill = factor(real))) + geom_histogram(alpha = 1, 
            position = position, colour = "black", binwidth = 0.05) + 
            theme_bw() + labs(x = "Predict", fill = "Real") + 
            ggtitle("Good/Bad barplot") + facet_wrap(~facetby)
    }
}


doGraph_GoodBad <- 
function (real, pred, alpha = 0.3, type = "logit", facetby = NULL) 
{
    if (type == "logit") {
        if (is.null(facetby)) {
            graph <- ggplot(data = data.frame(real, pred), aes(x = pred, 
                fill = factor(real))) + geom_density(alpha = 0.3) + 
                theme_bw() + labs(x = "Predict", colour = "Real") + 
                ggtitle("Good/Bad distribution graph")
        }
        else {
            graph <- ggplot(data = data.frame(real, pred, facetby), 
                aes(x = pred, fill = factor(real))) + geom_density(alpha = 0.3) + 
                theme_bw() + labs(x = "Predict", colour = "Real") + 
                ggtitle("Good/Bad distribution graph") + facet_wrap(~facetby)
        }
    }
    if (type == "linear") {
        if (is.null(facetby)) {
            graph <- ggplot(data = gather(data = data.frame(real, 
                pred), typ, prijem, pred:real), aes(x = prijem, 
                fill = typ)) + geom_density(alpha = 0.3) + theme_bw() + 
                labs(x = "Predict", colour = "Real") + ggtitle("Good/Bad distribution graph")
        }
        else {
            graph <- ggplot(data = gather(data = data.frame(real, 
                pred, facetby), typ, prijem, pred:real), aes(x = prijem, 
                fill = typ)) + geom_density(alpha = 0.3) + theme_bw() + 
                labs(x = "Predict", colour = "Real") + ggtitle("Good/Bad distribution graph") + 
                facet_wrap(~facetby)
        }
    }
    print(graph)
}
