## Recursion Schemes
___
### Sources:
- [Рекурсивные типы](https://youtu.be/6cqBwroa9OU)

#### Scala libs:
- [Matryoshka](https://github.com/precog/matryoshka)

#### Структурная система типов:
* Будем строить типы из единичного типа **()**, который обозначим **1**.
* Дизъюнктивную сумму типов $X$ и $Y$ обозначим $X + Y$.
* Декартово произведение типов $X$ и $Y$ обозначим $X \times Y$.
* Два типа **a** и **b** ***изоморфны***, если существуют взаимно-обратные функции
```haskell
from :: a -> b
to   :: b -> a
```
  такие, что 
```haskell
to . from ≡ id
from . to ≡ id
```
* Тип $Y^X$ — это функциональный тип $X \rightarrow Y$.
* Вводятся переменные типа и операция абстракции по таким переменным: $\lambda X.\,T[X]$.
* Пример для типа
```haskell
data Maybe x = Nothing | Just x
```
   конструктор типа `Maybe` записывается как $\lambda X.\,1+X$.

#### Рекурсивные типы:
* Список $\mathbf{L = List A}$ значений типа A это либо пустой список, либо одноэлементный, либо двухэлементный и т.д.
> $\mathbf{L = 1 + A + A^2 + A^3 +\dots}$
> $\mathbf{L = 1 + A \times (1 + A + A^2 + A^3 +\dots)}$
> $\mathbf{L = 1 + A \times L}$
```haskell
data List a = Nil | Cons a (List a)
```
> $\mathbf{L = (\lambda X.\,1 + A \times X)\,L}$
> $\mathbf{L = FIX\,(\lambda X.\,1 + A \times X)}$
> $\mathbf{List =\lambda A. FIX\,(\lambda X.\,1 + A \times X)}$
```haskell
newtype Fix f  = In { out :: f (Fix f) }

In :: f (Fix f) -> Fix f
out :: Fix f -> f (Fix f)
```
