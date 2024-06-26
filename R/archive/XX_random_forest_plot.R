png(file="output/random_forest_demo_plot.png", width = 1000, heigh = 1000)

par(mar = c(2.5, 2.5, 1, 1))
layout(matrix(c(1, 2, 3, 4, 1, 5, 3, 6), ncol=2), heights=c(1, 3, 1, 3))
plot.new()
text(0.2, 0.2, "Random forest representation", cex = 3, font = 2, col = "#2F2E41")
rpart.plot(tree_01, type=5)
plot.new()
rpart.plot(tree_02, type=5)
rpart.plot(tree_03, type=5)
rpart.plot(tree_04, type=5)
dev.off()