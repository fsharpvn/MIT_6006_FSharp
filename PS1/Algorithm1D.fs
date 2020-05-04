namespace Ps1

module Array =

    let getDimensions array =
        let rows = Array2D.length1 array
        let cols = Array2D.length2 array
        (rows,cols)

    let createProblems array = 
        let rows,cols = getDimensions array
        PeakProblem(array, (0,0, rows, cols))

    let convertFrom1D (list: int []) : int[,]  = 
        let m2 = Array2D.zeroCreate list.Length 1 
             
        for i=0 to list.Length-1 do         
            m2.[i,0] <- list.[i]
        m2        

module Algorithm = 

    let rec algorithm1(problem: PeakProblem, trace: TraceRecord option) =
        let col = problem.NumCol
        let row = problem.NumRow

        // if it is empty, we're done       

        if col <= 0 || row <= 0 then None

        /// the algorith only works for 1D peak' problems --- if the dimenssions are
        /// wrong, we should just give up, because whatever answer we give will 
        /// not be correct

        elif col <> 1 then None
        else                     

            /// the recursive subproblems will involve half the number of rows

            let mid = row/2

            /// information about the two subproblems

            let (subStartR1, subNumR1) = (0, mid)
            let (subStartR2, subNumR2) = (mid + 1, problem.NumRow - (mid + 1))

            let subProblems =
                [] 
                |> List.append [(subStartR1, 0, subNumR1, 1)]
                |> List.append [(subStartR2, 0, subNumR2, 1)]            

            /// see if the center location has a better neighbor

            let center = mid,0        
            let neighbor = problem.GetBetterNeighbor(center, trace)
       
            /// this is a peak, so return it

            if neighbor = center then 
                match trace with 
                    | None -> ()
                    | Some T -> T.FoundPeak(center)
                Some center
            else 

                /// otherwise, figure out which subproblem contains the neighbor, and 
                /// recurse in that half
                
                let sub = problem.GetSubproblemContaining(subProblems, neighbor)
                match trace with 
                | None -> None
                | Some T -> 
                    //T.SetProblemDimenssions(sub)
                    let result = algorithm1(sub, trace)
                    problem.GetLocationInSelf(sub, result)
module Main = 
    
    [<EntryPoint>]
    let main argv =

        let problemList = [|1; 2; 3; 4; 5; 6; 5; 4; 3|]
        let problem = 
            problemList
            |> Array.convertFrom1D 
            |> Array.createProblems

        let algorithmList = [("Algorithm 1", Algorithm.algorithm1)]
        let mutable steps = []

        algorithmList     
        |> List.iter (fun (name, algorithm1) -> 
            let tracer = TraceRecord({Type = "test"; Coords = (0,0)})
            let peak = algorithm1(problem, Some tracer)
            // steps
            // |> List.append tracer.Seq
            let status = 
                match peak with 
                | None -> "is NOT a peak (INCORRECT!)"
                | Some p -> 
                    if problem.IsPeak(p) then "is a peak" 
                    else "is NOT a peak (INCORRECT!)"
            printfn "%s : %s => %A" name (peak.ToString()) status
        )

        0 // return an integer exit code
