//begin code from http://blogs.msdn.com/b/dsyme/archive/2010/01/10/async-and-parallel-design-patterns-in-f-reporting-progress-with-events-plus-twitter-sample.aspx

open System.IO
open System
open System.Threading
open System.Net
open Microsoft.FSharp.Control.WebExtensions

type SynchronizationContext with
    /// A standard helper extension method to raise an event on the GUI thread
    member syncContext.RaiseEvent (event: Event<_>) args =
        syncContext.Post((fun _ -> event.Trigger args),state=null)
 
    /// A standard helper extension method to capture the current synchronization context.
    /// If none is present, use a context that executes work in the thread pool.
    static member CaptureCurrent () =
        match SynchronizationContext.Current with
            | null -> new SynchronizationContext()
            | ctxt -> ctxt


type AsyncWorker<'T>(jobs: seq<Async<'T>>) =  
    // This declares an F# event that we can raise
    let jobCompleted  = new Event<int * 'T>()
    /// Start an instance of the work
    member x.Start()    =
        // Capture the synchronization context to allow us to raise events back on the GUI thread
        let syncContext = SynchronizationContext.CaptureCurrent()
 
        // Mark up the jobs with numbers
        let jobs = jobs |> Seq.mapi (fun i job -> (job,i+1)) 
        let work = 
            Async.Parallel
               [ for (job,jobNumber) in jobs ->
                   async { let! result = job
                           syncContext.RaiseEvent jobCompleted (jobNumber,result)
                           return result } ]
 
        Async.Start(work |> Async.Ignore)
    /// Raised when a particular job completes
    member x.JobCompleted  = jobCompleted.Publish

let rec fib i = if i < 2 then 1 else fib (i-1) + fib (i-2)   
let worker =
    new AsyncWorker<_>( [ for i in 1 .. 100 -> async { return fib (i % 40) } ] )
 
worker.JobCompleted.Add(fun (jobNumber, result) ->
    printfn "job %d completed with result %A" jobNumber result)
worker.Start() 

let httpAsync(url:string) =
     async{ let req = WebRequest.Create(url)
            use! resp = req.AsyncGetResponse()
            use stream = resp.GetResponseStream()
            use reader = new StreamReader(stream)
            let text = reader.ReadToEnd()
            return text 
           }
            
let urls = [ "http://www.live.com";
              "http://news.live.com";
              "http://www.yahoo.com";
              "http://news.yahoo.com";
              "http://www.google.com";
              "http://news.google.com"; ]
 
let jobs =  [ for url in urls -> httpAsync url ]

#time "on";;   
let httpWorker = new AsyncWorker<_>(jobs)

httpWorker.JobCompleted.Add(fun (jobNumber, result) ->
        printfn "job %d completed with result %A" jobNumber result.Length)
 
httpWorker.Start();;
//end code from http://blogs.msdn.com/b/dsyme/archive/2010/01/10/async-and-parallel-design-patterns-in-f-reporting-progress-with-events-plus-twitter-sample.aspx
