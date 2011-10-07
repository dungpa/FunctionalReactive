//following code directly from http://lorgonblog.spaces.live.com/blog/cns!701679AD17B6D310!193.entry

open System
open System.Diagnostics
open System.Threading
 
// IsPrime : int -> bool
let IsPrime x =
    let mutable i = 2
    let mutable foundFactor = false
    while not foundFactor && i < x do
        if x % i = 0 then
            foundFactor <- true
        i <- i + 1
    not foundFactor

let nums = [| for i in 10000000..10004000 -> i |]

#time "on";;

// primeInfo = array<int * bool>   
let primeInfo =
    nums
    |> Array.map (fun x -> (x,IsPrime x));;

// we need to "join" at the end to know when we're done
//, and these will help do that
let primeInfo' = 
    let numRemainingComputations = ref nums.Length
    let mre = new ManualResetEvent(false)
    // primeInfo = array<int * bool>   
    let pi = Array.create nums.Length (0,false)

    nums
        |> Array.iteri
            (fun i x -> ignore (ThreadPool.QueueUserWorkItem(fun o ->
                pi.[i] <- (x, IsPrime x)
                // if we're the last one, signal that we're done
                if Interlocked.Decrement(numRemainingComputations) = 0 then
                    mre.Set() |> ignore)))    
    // wait until all done
    mre.WaitOne() |> ignore
    pi;;

let primeInfo'' =
    nums   
    |> Array.map (fun x -> async { return (x, IsPrime x) } )  
    |> Async.Parallel  
    |> Async.RunSynchronously;;

let primeInfo''' =
    nums   
    |> Array.Parallel.map (fun x -> (x, IsPrime x));;

//end following code directly from http://lorgonblog.spaces.live.com/blog/cns!701679AD17B6D310!193.entry
