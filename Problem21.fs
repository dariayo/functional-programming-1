module Problem21

//Tail recursive
let sumOfDivisorsTailRec n =
    let rec loop i acc =
        if i = 0 then acc
        elif n % i = 0 then loop (i - 1) (acc + i)
        else loop (i - 1) acc

    loop (n / 2) 0

let findAmicableNumbersTailRec limit =
    let rec loop current sum =
        if current >= limit then
            sum
        else
            let b = sumOfDivisorsTailRec current

            if b <> current && b < limit && sumOfDivisorsTailRec b = current then
                loop (current + 1) (sum + current)
            else
                loop (current + 1) sum

    loop 1 0

//Recursive

let rec sumOfDivisorsNonTailRec n i =
    if i = 0 then 0
    elif n % i = 0 then i + sumOfDivisorsNonTailRec n (i - 1)
    else sumOfDivisorsNonTailRec n (i - 1)

let rec findAmicableNumbersNonTailRec limit current sum =
    if current >= limit then
        sum
    else
        let b = sumOfDivisorsNonTailRec current (current / 2)

        if b <> current && b < limit && sumOfDivisorsNonTailRec b (b / 2) = current then
            findAmicableNumbersNonTailRec limit (current + 1) (sum + current)
        else
            findAmicableNumbersNonTailRec limit (current + 1) sum



//Module realization
let divisors n =
    [ 1 .. n / 2 ] |> List.filter (fun x -> n % x = 0)

let sumDivisorsModular n = divisors n |> List.fold (+) 0

let isAmicableModular n =
    let sum1 = sumDivisorsModular n
    let sum2 = sumDivisorsModular sum1
    sum1 <> n && sum2 = n

let sumAmicableNumbersModular limit =
    [ 2 .. limit - 1 ] |> List.filter isAmicableModular |> List.fold (+) 0

//Map
let sumAmicableWithMap limit =
    [ 2 .. limit - 1 ]
    |> List.map (fun n -> if isAmicableModular n then n else 0)
    |> List.sum

//Lazy collections
let sumOfDivisorsMod n =
    [ 1 .. n / 2 ] |> List.filter (fun x -> n % x = 0) |> List.sum

let findAmicableNumbersWithSeq limit =
    Seq.initInfinite id
    |> Seq.take limit
    |> Seq.filter (fun a ->
        let b = sumOfDivisorsMod a
        b <> a && b < limit && sumOfDivisorsMod b = a)
    |> Seq.sum

//Realization with for
let sumOfDivisors n =
    let mutable total = 0
    for i = 1 to n / 2 do
        if n % i = 0 then
            total <- total + i
    total

let findAmicableNumbers limit =
    let mutable amicableNumbers = Set.empty
    for number = 2 to limit - 1 do
        if not (Set.contains number amicableNumbers) then
            let partner = sumOfDivisors number
            if partner <> number && partner < limit && sumOfDivisors partner = number then
                amicableNumbers <- Set.add number amicableNumbers
                amicableNumbers <- Set.add partner amicableNumbers
    Set.fold (+) 0 amicableNumbers