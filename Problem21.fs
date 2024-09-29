module Problem21

//Tail recursive
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

//Recursive

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