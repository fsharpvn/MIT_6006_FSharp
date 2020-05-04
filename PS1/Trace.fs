namespace Ps1

type Location = int*int

type Trace =
    {
        Type: string
        Coords: Location
    }

type TraceRecord = struct

    /// A class for storing the trace of an algorithm, 
    /// to be exported and displayed using HTML visualizer

    val mutable Seq: Trace list

    new (trace: Trace) = {Seq = [{Type = trace.Type; Coords = trace.Coords}]}

    member self.GetMaximum(arguments, maximum) =

        /// A function for recording the fact that the getMaximum function of 
        /// some subproblems has been called    

        let a = { Type = "findingMaximum"; Coords = arguments }
        self.Seq <- a :: self.Seq

        let b = { Type = "findingMaximum"; Coords = maximum }
        self.Seq <- b::self.Seq

    member self.GetBetterNeighbor(neighbor, better) = 

        /// A function for recording the fact that the getBetterNeighbor function
        /// or some subproblem has been called

        let a = {Type = "findingMaximum";Coords = neighbor}
        self.Seq <- a::self.Seq

        let b = {Type = "findingMaximum";Coords = better}
        self.Seq <- b::self.Seq

    member self.FoundPeak(peak: Location) =

        /// A function for recording the fact that the peak has been found

        let a = {Type = "foundPeak"; Coords = peak}
        self.Seq <- a::self.Seq
end