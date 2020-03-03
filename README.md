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
