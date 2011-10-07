//following code from http://blogs.msdn.com/b/dsyme/archive/2010/01/09/async-and-parallel-design-patterns-in-f-parallelizing-cpu-and-i-o-computations.aspx

open System.IO
open System
open System.Net
open Microsoft.FSharp.Control.WebExtensions


let httpsync url =
    let req =  WebRequest.Create(Uri url)
    use resp = req.GetResponse()
    use stream = resp.GetResponseStream()
    use reader = new StreamReader(stream)
    let contents = reader.ReadToEnd()
    contents

let http url =
    async { let req =  WebRequest.Create(Uri url)
            use! resp = req.AsyncGetResponse()
            use stream = resp.GetResponseStream()
            use reader = new StreamReader(stream)
            let contents = reader.ReadToEnd()
            return contents }
 
let sites = ["http://www.bing.com"; "http://www.google.com"; "http://www.yahoo.com";
            "http://www.facebook.com"; "http://www.youtube.com"; "http://www.reddit.com"; "http://www.digg.com";
            "http://www.twitter.com"; "http://www.gmail.com"; "http://www.docs.google.com"; "http://www.maps.google.com";
            "http://www.microsoft.com"; "http://www.netflix.com"; "http://www.hulu.com"]
 
#time "on";;
let htmlOfSitesSync=
    [for site in sites -> httpsync site];;

let htmlOfSites =
    Async.Parallel [for site in sites -> http site ]
    |> Async.RunSynchronously;;

//end code from http://blogs.msdn.com/b/dsyme/archive/2010/01/09/async-and-parallel-design-patterns-in-f-parallelizing-cpu-and-i-o-computations.aspx
