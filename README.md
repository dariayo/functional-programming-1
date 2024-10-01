# Лабораторная работа №1

`Шевченко Дарья 369053. Вариант 9,21`

## Задача 9. Special Pythagorean Triplet

A Pythagorean triplet is a set of three natural numbers, $a \lt b \lt c$, for which,
$$a^2 + b^2 = c^2.$$

For example, $3^2 + 4^2 = 9 + 16 = 25 = 5^2$.

There exists exactly one Pythagorean triplet for which $a + b + c = 1000$.Find the product $abc$.

### Реализация рекурсией

```fsharp
let rec findB a b =
    let c = 1000 - a - b

    match a * a + b * b = c * c, b < 1000 - a - 1 with
    | true, _ -> (a, b, c)
    | false, true ->
        let result = findB a (b + 1)

        match result with
        | (-1, -1, -1) -> (-1, -1, -1)
        | _ -> result
    | false, false -> (-1, -1, -1)

let rec findA a =
    match a < 999 with
    | true ->
        let result = findB a (a + 1)

        match result with
        | (-1, -1, -1) ->
            let nextResult = findA (a + 1)

            match nextResult with
            | (-1, -1, -1) -> (-1, -1, -1)
            | _ -> nextResult
        | _ -> result
    | false -> (-1, -1, -1)

let solveEuler9 =
    let a, b, c = findA 1
    a * b * c
```

### Реализация хвостовой рекурсией

```fsharp
let rec findPythagoreanTripletTailRec a b =
    let c = 1000 - a - b

    match a * a + b * b = c * c, b < 1000 - a with
    | true, _ -> (a, b, c)
    | false, true -> findPythagoreanTripletTailRec a (b + 1)
    | false, false -> findPythagoreanTripletTailRec (a + 1) (a + 2)

```

### Модульная реализация + map

```fsharp
let generateTriplets target =
    [ 1..target ]
    |> List.collect (fun a -> [ a + 1 .. target ] |> List.map (fun b -> (a, b, target - a - b)))

let filterValidTriplets triplets =
    triplets |> List.filter (fun (a, b, c) -> a * a + b * b = c * c)

let findFirstValidTriplet target =
    generateTriplets target
    |> filterValidTriplets
    |> List.find (fun (a, b, c) -> a + b + c = target)
```

### Ленивые коллекции

```fsharp
let lazyTripletSeq target =
    seq {
        for a in 1..target do
            for b in a + 1 .. target do
                let c = target - a - b

                if a * a + b * b = c * c then
                    yield (a, b, c)
    }
    |> Seq.find (fun (a, b, c) -> a + b + c = target)
    |> fun (a, b, c) -> a * b * c
```

### Реализация на Python

```python
def find_pythagorean_triplet(target):
    for a in range(1, target):
        for b in range(a + 1, target - a):
            c = target - a - b
            if a**2 + b**2 == c**2:
                return a * b * c
    return None
```

## Задача 21. Amicable Numbers

Let $d(n)$ be defined as the sum of proper divisors of $n$ (numbers less than $n$ which divide evenly into $n$)

If $d(a) = b$ and $d(b) = a$, where $a \ne b$, then $a$ and $b$ are an amicable pair and each of $a$ and $b$ are called amicable numbers.

For example, the proper divisors of $220$ are $1, 2, 4, 5, 10, 11, 20, 22, 44, 55$ and $110$; therefore $d(220) = 284$. The proper divisors of $284$ are $1, 2, 4, 71$ and $142$; so $d(284) = 220$.

Evaluate the sum of all the amicable numbers under $10000$.

### Реализация рекурсией

```fsharp
let rec sumOfDivisorsNonTailRec n i =
    match i with
    | 0 -> 0
    | _ when n % i = 0 -> i + sumOfDivisorsNonTailRec n (i - 1)
    | _ -> sumOfDivisorsNonTailRec n (i - 1)

let rec findAmicableNumbersNonTailRec limit current sum =
    match current >= limit with
    | true -> sum
    | false ->
        let b = sumOfDivisorsNonTailRec current (current / 2)

        match b <> current, b < limit, sumOfDivisorsNonTailRec b (b / 2) = current with
        | true, true, true -> findAmicableNumbersNonTailRec limit (current + 1) (sum + current)
        | _ -> findAmicableNumbersNonTailRec limit (current + 1) sum

```

### Реализация хвостовой рекурсией

```fsharp
let sumOfDivisorsTailRec n =
    let rec loop i acc =
        match i with
        | 0 -> acc
        | _ when n % i = 0 -> loop (i - 1) (acc + i)
        | _ -> loop (i - 1) acc

    loop (n / 2) 0

let findAmicableNumbersTailRec limit =
    let rec loop current sum =
        match current >= limit with
        | true -> sum
        | false ->
            let b = sumOfDivisorsTailRec current

            match b <> current, b < limit, sumOfDivisorsTailRec b = current with
            | true, true, true -> loop (current + 1) (sum + current)
            | _ -> loop (current + 1) sum

    loop 1 0
```

### Модульная реализация

```fsharp
let divisors n =
    [ 1 .. n / 2 ] |> List.filter (fun x -> n % x = 0)

let sumDivisorsModular n = divisors n |> List.fold (+) 0

let isAmicableModular n =
    let sum1 = sumDivisorsModular n
    let sum2 = sumDivisorsModular sum1
    sum1 <> n && sum2 = n

let sumAmicableNumbersModular limit =
    [ 2 .. limit - 1 ] |> List.filter isAmicableModular |> List.fold (+) 0
```

### Map

```fsharp
let sumAmicableWithMap limit =
    [ 2 .. limit - 1 ]
    |> List.map (fun n -> if isAmicableModular n then n else 0)
    |> List.sum

```

### Ленивые коллекции

```fsharp
let sumOfDivisorsMod n =
    [ 1 .. n / 2 ] |> List.filter (fun x -> n % x = 0) |> List.sum

let findAmicableNumbersWithSeq limit =
    Seq.initInfinite id
    |> Seq.take limit
    |> Seq.filter (fun a ->
        let b = sumOfDivisorsMod a
        b <> a && b < limit && sumOfDivisorsMod b = a)
    |> Seq.sum
```

### Реализация с циклом

```fsharp
let sumOfDivisors n =
    let mutable total = 0

    for i = 1 to n / 2 do
        if n % i = 0 then
            total <- total + i

    total

let findAmicableNumbers limit =
    let mutable amicableNumbers = Set.empty

    for number = 2 to limit - 1 do
        match Set.contains number amicableNumbers with
        | true -> ()
        | false ->
            let partner = sumOfDivisors number

            match partner <> number, partner < limit, sumOfDivisors partner = number with
            | true, true, true ->
                amicableNumbers <- Set.add number amicableNumbers
                amicableNumbers <- Set.add partner amicableNumbers
            | _ -> ()

    Set.fold (+) 0 amicableNumbers
```

### Реализация на Python

```python
def sum_of_divisors(n):
    total = 0
    for i in range(1, n // 2 + 1):
        if n % i == 0:
            total += i
    return total

def find_amicable_numbers(limit):
    amicable_numbers = set()
    
    for number in range(2, limit):
        if number not in amicable_numbers:
            partner = sum_of_divisors(number)
            if partner != number and sum_of_divisors(partner) == number:
                amicable_numbers.add(number)
                amicable_numbers.add(partner)
    
    return sum(amicable_numbers)
```

## Выводы

F# показался мне достаточно простым и понятным по синтаксису, было легко привыкнуть.

Я познакомилась с такими функциями, как List.map, List.filter и List.fold, они сделали код более компактным и понятным. Использование функции filter для отсеивания неподходящих значений делает код более декларативным.

Также я воспользовалась ленивыми коллекциями (seq), которые, например, в задаче с Пифагоровыми тройками позволяют эффективно обрабатывать данные по мере необходимости, что улучшает производительность.
