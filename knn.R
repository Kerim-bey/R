
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
#функция сортировки
sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  #матрица расстояний
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  #сортируем
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

kNN <- function(xl, z, k)
{
  #вызываем функцию сортировки
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  #классы первых k соседей
  classes <- orderedXl[1:k, n + 1]
  #таблица классов
  counts <- table(classes)
  #выбираем класс,встречающийся больше всего раз
  class <- names(which.max(counts))
  return (class)
}

colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col
     = colors[iris$Species], asp = 1)

z <- c(2.7, 1)
xl <- iris[, 3:5]
class <- kNN(xl, z, k=6)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)

