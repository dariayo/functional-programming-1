namespace Tests

open NUnit.Framework
open Problem9
open Problem21

[<TestFixture>]
module Tests =

    [<Test>]
    let ``Test tail recursive Pythagorean triplet`` () =
        let (a, b, c) = findPythagoreanTripletTailRec 1 2
        Assert.AreEqual(31875000, a * b * c)

    [<Test>]
    let ``Test solveEuler9 recursive approach`` () =
        let result = solveEuler9
        Assert.AreEqual(31875000, result)

    [<Test>]
    let ``Test module realization`` () =
        let (a, b, c) = findFirstValidTriplet 1000
        Assert.AreEqual(31875000, a * b * c)

    [<Test>]
    let ``Test lazy triplet sequence`` () =
        let result = lazyTripletSeq 1000
        Assert.AreEqual(31875000, result)

    [<Test>]
    let ``Test tail recursive`` () =
        let result = findAmicableNumbersTailRec 10000
        Assert.AreEqual(31626, result)

    [<Test>]
    let ``Test recursive`` () =
        let result = findAmicableNumbersNonTailRec 10000 1 0
        Assert.AreEqual(31626, result)

    [<Test>]
    let ``Test module realization problem 21`` () =
        let result = sumAmicableNumbersModular 10000
        Assert.AreEqual(31626, result)

    [<Test>]
    let ``Test map realization problem 21`` () =
        let result = sumAmicableWithMap 10000
        Assert.AreEqual(31626, result)

    [<Test>]
    let ``Test lazy realization problem 21`` () =
        let result = findAmicableNumbersWithSeq 10000
        Assert.AreEqual(31626, result)

    [<Test>]
    let ``Test cycle realization`` () =
        let result = findAmicableNumbers 10000
        Assert.AreEqual(31626, result)
