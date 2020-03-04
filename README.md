# R
# Метрические алгоритмы классификации 
 
Предположим, что в пространстве объектов 𝑋 задана некоторая функция расстояния (метрика) 𝜌(𝑥,𝑥M), характеризующая степень близости объектов, причём, необязательно метрика. Кроме этого, будем считать, что объекты из одного класса близки согласно функции 𝜌, а объекты разных классов — далеки. Данное предположение называют гипотезой компактности. Обратим внимание, что для вычисления 𝜌 нам необязательно знать признаковое описание объектов.  
 
# Методы ближайших соседей
 
Объекты, которые находятся близко друг другу склонны иметь близкие значения предсказываемых величин — это главная идея метода ближайших соседей. Если вы знаете предсказываемое значение для одного из элементов, вы можете предсказать его и для ближайших соседей.
 
Основная сложность в применении этого метода состоит в том, что нам необходимо четко определить понятие «близости». Это, в свою очередь, приводит нас к понятию расстояния между элементами. На практике можно использовать различные виды расстояний, но чаще всего достаточно простого евклидова. 
 
В качестве обучающей выборки была взята матрица ирисов фишера по длине и ширине лепестка. Требовалось построить карту классификации на основе данных обучающей выборки. В качестве метода классификации использовался метод 1NN.
 
# 1NN
 
В качестве функции расстояния будем использовать обычное евклидово расстояние.
1. Сортируем объекты согласно расстояния до объекта.
2. Обучающая выборка сортируется по возрастанию расстояния от каждого объекта выборки до классифицируемого.
3. Классифицируемому объекту присваивается тот же класс, что и ближайшего к нему объекта выборки.

![](https://github.com/Kerim-bey/R/blob/master/img/1NN.png)

# KNN

 Для повышения надёжности классификации объект относится к тому классу, которому принадлежит большинство из его соседей — k ближайших к нему объектов обучающей выборки.   
 Реализация kNN функции:  
 ```    
 kNN <- function(xl, z, k)
{
  ###### Сортируем выборку согласно классифицируемого
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  ###### Получаем классы первых k соседей
  classes <- orderedXl[1:k, n + 1]
  ###### Составляем таблицу встречаемости каждого класса
  counts <- table(classes)
  ###### Находим класс, который доминирует среди первых k соседей
  class <- names(which.max(counts))
  return (class)
}
```

![](https://github.com/Kerim-bey/R/blob/master/img/knn2.png)     
![](https://github.com/Kerim-bey/R/blob/master/img/kNN%20классификация.png)
![](https://github.com/Kerim-bey/R/blob/master/img/LOO.png)
  
Оптимальное k = 6.  
Преимущества:
 1) Простота реализации.
 2) При k, подобранном около оптимального, алгоритм достаточно хорошо классифицирует.        
Недостатки:
 1) Нужно хранить всю выборку.
 2) При k = 1 неустойчивость к погрешностям.
 3) Крайне бедный набор параметров.
 4) Точки, расстояние между которыми одинаково, не все будут учитываться.

# kwNN
По сравнению с kNN, kwNN принимает во внимание не только количество соседей определенного класса, но и удаленность от классифицируемого обьекта. Выбирается k ближайших соседей. Каждому соседу присваивается вес(мера удаленности соседа от классифицируемого обьекта). Объекту присваивается класс, вес которого больше.   
```
###### Весовая функция:
weightsKWNN = function(i, k)  
{  
  (k + 1 - i) / k  
}  
``` 
![](https://github.com/Kerim-bey/R/blob/master/img/kwnnLooMap.png)  

# Сравнение качества алгоритмов kNN и kwNN.
kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, и размерность пространства K, то количество операций для классификации тестовой выборки может быть оценено как O(KMN).

kwNN отличается от kNN, тем что учитывает порядок соседей классифицируемого объекта, улучшая качество классификации.

# Парзеновское окно  
Для оценки близости объекта u к классу y алгоритм использует следующую функцию:  
![](https://github.com/Kerim-bey/R/blob/master/img/парзен.svg)  
где K(z) - функция ядра.  
Основные типы ядер:  
![](https://github.com/Kerim-bey/R/blob/master/img/ядро.png)  
Код для реализации данных типов ядер:  
```
###### Епанечникова:  
kernelE = function(r){ return ((3/4*(1-r^2)*(abs(r)<=1)))}  
###### Квартическое:   
kernelQ = function(r){ return ((15 / 16) * (1 - r ^ 2) ^ 2 * (abs(r) <= 1))}  
###### Треугольное:  
kernelT = function(r){ return ((1 - abs(r)) * (abs(r) <= 1)) }  
###### Гауссовское:    
kernelG = function(r){ return (((2*pi)^(-1/2)) * exp(-1/2*r^2))}  
###### Прямоугольное:  
kernelR = function(r){ return ((0.5 * (abs(r) <= 1) ))}
```
Код алгоритма:  
```
PW = function(XL,y,h,metricFunction = euclideanDistance){    
    l <- dim(xl)[1]  
    weights = rep(0,3)  
    names(weights) = unique(xl[,3])  
    for(i in 1:l)  
    {  
      x=XL[i,1:2]  
      class=XL[i,3]  
      r = metricFunction(x,y)/h  
      weights[class]=kernelR(r)+weights[class];  
    }  
    if(max(weights)==0){ return ("0") }  
    else{ return (names(which.max(weights))) }  
}  
```  
# Карты классификаций  
![](https://github.com/Kerim-bey/R/blob/master/img/pw_map_1.png)  
![](https://github.com/Kerim-bey/R/blob/master/img/pw_map_2.png)  
![](https://github.com/Kerim-bey/R/blob/master/img/pw_map_3.png) 

# Линейные алгоритмы классификации
Пусть 𝑋 = ℝ^(n) и 𝑌 = {−1;+1}. Алгоритм
``` 
𝑎(𝑥,𝑤) = 𝑠𝑖𝑔𝑛<𝑤, 𝑥>, 𝑤 ∈ ℝ^(n),
``` 
является линейным алгоритмом классификации.

Линейный классификатор - классификатор вида  ![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn%20(1).gif), где w - вектор весов, w0 - порог принятия решений.
![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn.gif) - отступ объекта x, если он отрицателен, то алгоритм допускает ошибку.
Для минимизации суммарных потерь ![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn%20(2).gif) применим метод стохастический метод.
# ADALINE
Алгоритм классификации ADALINE — адаптивный линейный
элемент, в качестве функции потерь используется квадратичная
функция потерь:
``` 
lossQuad <- function(x)
{
return ((x-1)^2)
}
``` 
Результат работы алгоритма при помощи ADALINE:

![](https://github.com/Kerim-bey/R/blob/master/img/adaline.png)

# Персептрон Розенблатта
Персептрон Розенблатта — линейный классификатор,
обучаемый с помощью стохастического градиента с правилом Хэбба и
кусочно-линейной функции потерь:
``` 
## Функция потерь для правила Хэбба
lossPerceptron <- function(x)
{
return (max(-x, 0))
}
``` 

Результат работы алгоритма:

![](https://github.com/Kerim-bey/R/blob/master/img/hebb.png)

# Линии уровня нормального распределения

Вероятностное распределение с плотностью ![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn%20(3).gif) называется n-мерным многомерным нормальным распределением с математическим ожиданием ![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn%20(4).gif).
Предполагается, что матрица симетрична, невырожденная, положительно определенная.

Реализация на R
``` 
line_norm <- function(center,A)
{
  det<-det(A)
  a <- A[2,2]/det
  b <- -A[2,1]/det
  c <- -A[1,2]/det
  d <- A[1,1]/det
  
  x0 <- center[1]
  y0 <- center[2]
  
  X <- seq(-2.5, 2.5, 0.1)
  Y <- seq(-2.5, 2.5, 0.1)
  
  
  A <- d
  B <- a
  C <- -c -b
  D <- -2*d*x0 + y0*(c+b)
  E <- -2*a*y0 + x0*(c+b)
  F <- d*x0^2 + a*y0^2 + x0*y0*(-c-b)
  
  func <- function(x, y) {
    1/(2*pi*sqrt(det))*exp((-1/2)*(x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  }
  Z <- outer(X, Y, func)
  
  contour(X, Y, Z)
}
```
# Некореллированы

![](https://github.com/Kerim-bey/R/blob/master/img/realnotcor.png)

# Одинаковая дисперсия

![](https://github.com/Kerim-bey/R/blob/master/img/notcor.png)

# Кореллированы

![](https://github.com/Kerim-bey/R/blob/master/img/cor.png)

# PLUG-IN
Восстанавливая параметры нормального распределения 𝜇O, 𝛴O для каждого класса 𝑦 ∈ 𝑌 и подставляя в формулу оптимального байесовского классификатора восстановленные плотности, получим подстановочный (plug-in) алгоритм классификации. Параметры нормального распределения оценивают согласно принципа максимума правдоподобия:

![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn%20(5).gif)

![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn%20(6).gif)

Реализация: 

``` 
## Восстановление центра нормального распределения
estimateMu <- function(objects)
{
## mu = 1 / m * sum_{i=1}^m(objects_i)
rows <- dim(objects)[1]
cols <- dim(objects)[2]
mu <- matrix(NA, 1, cols)
for (col in 1:cols)
{
mu[1, col] = mean(objects[,col])
}
return(mu)
}

## Восстановление ковариационной матрицы нормального
распределения
estimateCovarianceMatrix <- function(objects, mu)
{
rows <- dim(objects)[1]
cols <- dim(objects)[2]
sigma <- matrix(0, cols, cols)
for (i in 1:rows)
{
sigma <- sigma + (t(objects[i,] - mu) %*%
(objects[i,] - mu)) / (rows - 1)
}
return (sigma)
}

```

Результат работы: 

![](https://github.com/Kerim-bey/R/blob/master/plug.png)


# Линейный дискриминант Фишера – ЛДФ

Линейный дискриминант Фишера (ЛДФ), который, в отличии от подстановочного алгоритма, при построении предполагает, что ковариационные матрицы классов равны, и для их восстановления нужно использовать все (всех классов) объекты обучающей выборки.

Параметры нормального распределения оценивают согласно принципа максимума правдоподобия:

![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn%20(6).gif)

![](https://github.com/Kerim-bey/R/blob/master/CodeCogsEqn%20(7).gif)

Реализация: 

``` 
## Восстановление центра нормального распределения
estimateMu <- function(x)
{
  m <- dim(x)[2]
  mu <- matrix(NA, 1, m)
  for(i in 1:m)
  {
    mu[1,i] <- mean(x[,i])
  }
  return(mu)
}

## Оценка ковариационной матрицы для ЛДФ
estimateFisherCovarianceMatrix <- function(objects1, objects2, mu1, mu2)
{
  rows1 <- dim(objects1)[1]
  rows2 <- dim(objects2)[1]
  rows <- rows1 + rows2
  cols <- dim(objects1)[2]
  sigma <- matrix(0, cols, cols)
  for (i in 1:rows1)
  {
    sigma <- sigma + (t(objects1[i,] - mu1) %*%
                        (objects1[i,] - mu1)) / (rows + 2)
  }
  for (i in 1:rows2)
  {
    sigma <- sigma + (t(objects2[i,] - mu2) %*%
                        (objects2[i,] - mu2)) / (rows + 2)
  }
  return (sigma)
}

```

Результат работы: 

![](https://github.com/Kerim-bey/R/blob/master/fisher.png)
