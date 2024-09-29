module Problem9

//Tail recursive
let rec findPythagoreanTripletTailRec a b =
    let c = 1000 - a - b

    match a * a + b * b = c * c, b < 1000 - a with
    | true, _ -> (a, b, c)                
    | false, true -> findPythagoreanTripletTailRec a (b + 1)
    | false, false -> findPythagoreanTripletTailRec (a + 1) (a + 2) 


//Recursive
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


// Map + Module realization
let generateTriplets target =
    [ 1..target ]
    |> List.collect (fun a -> [ a + 1 .. target ] |> List.map (fun b -> (a, b, target - a - b)))

let filterValidTriplets triplets =
    triplets |> List.filter (fun (a, b, c) -> a * a + b * b = c * c)

let findFirstValidTriplet target =
    generateTriplets target
    |> filterValidTriplets
    |> List.find (fun (a, b, c) -> a + b + c = target)

//Lazy collectons
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
