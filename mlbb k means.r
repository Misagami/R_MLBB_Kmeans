# reading and printing CSV file
mlbb <- read.csv("mlbb_hero.csv")
print(mlbb)
print("---------------------------------------------------")
summary(is.na(mlbb))
#conclusion = no missing data till now
print("---------------------------------------------------")

# check for df no. of rows and columns
print(is.data.frame(mlbb))
print(ncol(mlbb))
print(nrow(mlbb))
print("---------------------------------------------------")


# print max ban rate
ban <- max(mlbb$ban_rate)
print(ban)

# print max pick rate
pick <- max(mlbb$pick_rate)
print(pick)

# print max win rate
win <- max(mlbb$win_rate)
print(win)
print("---------------------------------------------------")

# list assassins
val3 <- subset(mlbb, role == "assassin")
print(val3)
print("---------------------------------------------------")

# avg win rate
win_res <- sum(mlbb$win_rate) / nrow(mlbb)
print(win_res)
print("---------------------------------------------------")

#using win pick and ban rate for clustering
#3*3 matrix
library(corrplot)
x <- mlbb[, 16:18]
x <- scale(x)
round(cor(x), 2)  #finding correlation
#chosen variables have small correlation so we can use these to analyze with k means

library(factoextra)
fviz_nbclust(x, kmeans, method = "wss")
fviz_nbclust(x, kmeans, method = "silhouette")
print("---------------------------------------------------")

cluster_6 <- kmeans(x, centers = 6)
cluster_6
print("---------------------------------------------------")

group <- cluster_6$cluster
final <- data.frame(mlbb, group)
head(final) #6*19
print("---------------------------------------------------")

# install.packages('ExcelFunctionsR')
library("ExcelFunctionsR")

avg_win_rate <- c(
    round(AVERAGEIFS(final$win_rate, final$group, 1), 2),
    round(AVERAGEIFS(final$win_rate, final$group, 2), 2),
    round(AVERAGEIFS(final$win_rate, final$group, 3), 2),
    round(AVERAGEIFS(final$win_rate, final$group, 4), 2),
    round(AVERAGEIFS(final$win_rate, final$group, 5), 2),
    round(AVERAGEIFS(final$win_rate, final$group, 6), 2)
)
avg_pick_rate <- c(
    round(AVERAGEIFS(final$pick_rate, final$group, 1), 2),
    round(AVERAGEIFS(final$pick_rate, final$group, 2), 2),
    round(AVERAGEIFS(final$pick_rate, final$group, 3), 2),
    round(AVERAGEIFS(final$pick_rate, final$group, 4), 2),
    round(AVERAGEIFS(final$pick_rate, final$group, 5), 2),
    round(AVERAGEIFS(final$pick_rate, final$group, 6), 2)
)
avg_ban_rate <- c(
    round(AVERAGEIFS(final$ban_rate, final$group, 1), 2),
    round(AVERAGEIFS(final$ban_rate, final$group, 2), 2),
    round(AVERAGEIFS(final$ban_rate, final$group, 3), 2),
    round(AVERAGEIFS(final$ban_rate, final$group, 4), 2),
    round(AVERAGEIFS(final$ban_rate, final$group, 5), 2),
    round(AVERAGEIFS(final$ban_rate, final$group, 6), 2)
)
group <- c(1:6)
summary <- data.frame(group, avg_win_rate, avg_pick_rate, avg_ban_rate)
print("---------------------------------------------------")

summary
print("---------------------------------------------------")

# bar graph
grp <- c(1, 2, 3, 4, 5, 6)
win <- c(50.57, 50.03, 50.04, 48.10, 51.76, 47.08)
png(file = "bar_chart.png")
barplot(grp, names.arg = win, xlab = "Groups", ylab = "Average Win Rate", col = "grey", main = "Cluster Chart", border = "black")
dev.off()
