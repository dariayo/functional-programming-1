module Program =
    open Problem9
    open Problem21

    [<EntryPoint>]
    let main argv =
        printfn "Project Euler 9"

        let (a1, b1, c1) = findPythagoreanTripletTailRec 1 2
        printfn "Хвостовая рекурсия: %d" (a1 * b1 * c1)

        printfn "Обычная рекурсия: %d" solveEuler9

        let (a, b, c) = findFirstValidTriplet 1000
        let product = a * b * c
        printfn "Map и модульная реализация: %d" product

        let product = lazyTripletSeq 1000
        printfn "Работа с бесконечными списками: %d" product

        printfn "Project Euler 21"
        printfn "Хвостовая рекурсия: %d" (findAmicableNumbersTailRec 10000)

        printfn "Обычная рекурсия: %d" (findAmicableNumbersNonTailRec 10000 1 0)

        printfn "Модульная реализация: %d" (sumAmicableNumbersModular 10000)

        printfn "Map: %d" (sumAmicableWithMap 10000)

        printfn "Работа с бесконечными списками: %d" (findAmicableNumbersWithSeq 10000)

        let result = findAmicableNumbers 10000
        printfn "Цикл : %d" result

        0
