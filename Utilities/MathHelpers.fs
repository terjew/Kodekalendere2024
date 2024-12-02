namespace Utilities

module MathHelpers = 
    let generatePrimeArray limit =
        let primeArray = Array.create (limit + 1) true
        let rec setArray l h s x =
            if l <= h then
                primeArray.[l] <- x
                setArray (l + s) h s x
        primeArray.[0] <- false; primeArray.[1] <- false
        for i = 0 to primeArray.Length - 1 do
            if primeArray.[i]
            then setArray (i + i) (primeArray.Length - 1) i false
        primeArray

    let isPrime a =
        match a with
        | a when a < 2 -> false
        | a ->
            let divisors = seq { 2 .. int (sqrt (float a)) }
            not <| Seq.exists (fun d -> a % d = 0) divisors

    let rec digitSum number carry =
        let digit = number % 10
        match number - digit with 
        | 0 -> carry + digit
        | _ -> digitSum (number / 10) (carry + digit)

    let solveQuadraticEquation (a,b,c) = 
        let q = System.Math.Sqrt(b*b - 4.0*a*c)
        ((-b + q)/(2.0*a), (-b - q)/(2.0*a))
    
    let ordered v1 v2 =
        (min v1 v2, max v1 v2)
