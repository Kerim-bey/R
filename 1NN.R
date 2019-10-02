euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction = 
euclideanDistance) 
{ 
l <- dim(xl)[1] 
n <- dim(xl)[2] - 1 

distances <- matrix(NA, l, 2) 

for (i in 1:l) 
{ 
distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z)) 
} 

orderedXl <- xl[order(distances[, 2]), ] 

euclideanDistance <- function(u, v) 
{ 
sqrt(sum((u - v)^2)) 
} 

return (orderedXl); 
} 

colors <- c("setosa" = "red", "versicolor" = "green3", 
"virginica" = "blue") 
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col 
= colors[iris$Species], asp = 1) 

z <- c(2.2, 1) 
xl <- iris[, 3:5] 
orderedXl <- sortObjectsByDist(xl,z) 
n <- dim(orderedXl)[2] - 1 

class <- orderedXl[1, n + 1] 
points(z[1], z[2], pch = 24, bg = colors[class], asp = 1)