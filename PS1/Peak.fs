namespace Ps1 

type PeakProblem = struct 

    /// A class representing an instance of a peak-finding problem
    
    val Array: int [,]
    val Bounds: int*int*int*int
    val StartRow: int
    val StartCol: int
    val NumRow: int
    val NumCol: int
    
    new (array, bounds) =     

        /// A method for initialization an instance of the PeakProblem type or class.
        /// Takes an array and an argument indicating which rows to include

        let (startRow, startCol, numRow, numCol) = bounds
        {
            Array = array
            Bounds = bounds
            StartRow = startRow; StartCol = startCol; NumRow = numRow; NumCol = numCol 
        }

    override self.ToString() = sprintf "[%A,%A]" self.Array self.Bounds

    member self.NoPeak = (0,0)

    member self.Get(location: Location) = 

        /// Return the value of the array at the given location, 
        /// offset by the coordinates (startRow, startCol)

        let (r, c) = location 
        if not (0 <= r && r < self.NumRow) then 0
        elif not (0 <= c && c < self.NumCol) then 0
        else self.Array.[(self.StartRow + r),(self.StartCol + c)]

    member self.GetBetterNeighbor(location: Location, trace: TraceRecord option) : Location =

        /// if (r,c) has a better neighbor, return the neighbor.
        /// Otherwise return the location (r,c)

        let (r,c) = location
        let mutable best = location

        if r-1 >= 0 && self.Get(r-1, c) > self.Get(best) then 
            best <- (r-1, c)
        elif c-1 >= 0 && self.Get(r, c-1) > self.Get(best) then 
            best <- (r, c-1)
        elif r+1 >= 0 && self.Get(r+1, c) > self.Get(best) then 
            best <- (r+1, c)
        elif c+1 >= 0 && self.Get(r, c+1) > self.Get(best) then 
            best <- (r, c+1)

        match trace with 
        | None -> ()
        | Some p -> p.GetBetterNeighbor(location, best)

        best       

    member self.GetSubProblem(bounds): PeakProblem =

        /// Return a subproblem with the given bounds. The boud is a quadruple
        /// of numbers: (starting row, starting column, # of rows, # of colums)

        let (sRow, sCol, nRow, nCol) = bounds
        let newBounds = (self.StartRow + sRow, self.StartCol + sCol, nRow, nCol)
        PeakProblem(self.Array, newBounds)

    member self.GetSubproblemContaining(boundList, location) : PeakProblem =

        /// Return the subproblem containing the given location. Picks the first
        /// of the subproblems in the list which satisfies that constrains, and then 
        /// constructs the subproblem using getSubproblem()

        let row,col = location
        
        /// Create a template to return subproblem
        let mutable problem = self
        
        for (sRow, sCol, nRow, nCol) in boundList do
            if sRow  <= row && row < sRow + nRow then 
                if sCol <= col && col < sCol + nCol then 
                    problem <- self.GetSubProblem(sRow, sCol, nRow, nCol)            

        problem

    member self.GetLocationInSelf(problem: PeakProblem, location: Location) : Location=

        /// Remaps the location in the given problem to the same location in 
        /// the problem that this function is being called from

        
        
        let row, col = location
        let newRow = row + problem.StartRow - self.StartRow
        let newCol = col + problem.StartCol - self.StartCol
        (newRow, newCol)

    member self.IsPeak(location) : bool =

        /// Return true if the given location is a peak in the current subproblem
        
        self.GetBetterNeighbor(location, None) = location
end