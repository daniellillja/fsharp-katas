namespace fsharp_katas
//https://web.archive.org/web/20120702222856/http://fsharp-euler.wikispaces.com/

module ``Project Euler`` =
    open System

    let euler1 max =
        seq{3..max-1} 
            |> Seq.filter (fun num -> (num % 3 = 0) || (num % 5) = 0)
            |> Seq.sum

    let euler2 max =
        let fibs = (1,1) |> Seq.unfold (fun (current, next) -> Some(current, (next, current + next)))
        let evenFibs = fibs |> Seq.filter (fun num -> num % 2 = 0)
        let fibsInRange max = evenFibs |> Seq.takeWhile (fun num -> num <= int(max))
        Seq.sum (fibsInRange max)


    let euler3 number =
        let possibleFactorsOf (n:int64) =
            let upperBound = int64(Math.Sqrt(double(n)))
            [2L..upperBound]
        
        let findFactorsOf (n:int64) = possibleFactorsOf n |> Seq.filter (fun x -> n % x = 0L)

        let isPrime n = findFactorsOf n |> Seq.length = 0

        findFactorsOf number |> Seq.filter isPrime |> Seq.max 
