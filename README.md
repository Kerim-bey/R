# R
# Метрические алгоритмы классификации 
 
Предположим, что в пространстве объектов 𝑋 задана некоторая функция расстояния (метрика) 𝜌(𝑥,𝑥M), характеризующая степень близости объектов, причём, необязательно метрика. Кроме этого, будем считать, что объекты из одного класса близки согласно функции 𝜌, а объекты разных классов — далеки. Данное предположение называют гипотезой компактности. Обратим внимание, что для вычисления 𝜌 нам необязательно знать признаковое описание объектов.  
 
Методы ближайших соседей
 
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


![](https://github.com/Kerim-bey/R/blob/master/img/knn2.png)     
![](https://github.com/Kerim-bey/R/blob/master/img/kNN%20классификация.png)
![](https://github.com/Kerim-bey/R/blob/master/img/LOO.png)

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

####### Весовая функция:
weightsKWNN = function(i, k)  
{  
  (k + 1 - i) / k  
}  
  
![](https://github.com/Kerim-bey/R/blob/master/img/kwnnLooMap.png)
