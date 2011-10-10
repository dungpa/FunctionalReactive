module AsyncWorkflow

open System
open System.IO
open System.Net

type CacheMsg =
    | Stop
    | Get of MailboxProcessor<CacheMsg> * string
    | Fetch of string * string
 
type CacheServer() =
    let lifeSpan = 1.0
    let getHttp url =
        async { let req =  WebRequest.Create(Uri url)
                use! resp = req.AsyncGetResponse()
                use stream = resp.GetResponseStream()
                use reader = new StreamReader(stream)
                let contents = reader.ReadToEnd()
                return contents }

    let server =
        MailboxProcessor.Start(fun inbox ->
            let rec loop (caches : Map<string, string*DateTime>) =
                async { let! msg = inbox.Receive()
                        match msg with
                        | Stop -> return ()
                        | Get(client, url) -> 
                            match Map.tryFind url caches with
                            | Some (contents, expireDate) -> 
                                if DateTime.Now <= expireDate then 
                                    client.Post(Fetch(contents, string expireDate))
                                    Console.WriteLine("Server: cached contents")
                                    return! loop (caches)
                                else
                                    let! contents = getHttp url
                                    let expireDate = DateTime.Now.AddSeconds(lifeSpan)
                                    client.Post(Fetch(contents, string expireDate))
                                    Console.WriteLine("Server: flushed contents")
                                    return! loop(Map.add url (contents, expireDate) (Map.remove url caches))
                            | None -> 
                                let! contents = getHttp url
                                let expireDate = DateTime.Now.AddSeconds(lifeSpan)
                                client.Post(Fetch(contents, string expireDate))
                                Console.WriteLine("Server: fresh contents")
                                return! loop(Map.add url (contents, expireDate) caches)
                        | _ -> return ()
                            }
            loop (Map.empty))
 
    member x.Get(client, url) = server.Post(Get(client, url))
    member x.Stop() = server.Post(Stop)

type Client (server: CacheServer) =
    let client =
        MailboxProcessor.Start(fun inbox -> 
            let rec loop () = async { 
                let! msg = inbox.Receive()
                match msg with            
                | Fetch(contents, expireDate) -> 
                    Console.WriteLine("Contents=%s"+contents.Substring(0, 20))
                    Console.WriteLine("Expired at %s"+expireDate)
                    return! loop()
                | _ ->
                    return ()
                 }
                    
            loop())    
    member x.Get(url) = server.Get(client, url)
    member x.Stop() = client.Post(Stop)

// Main
let N = 14
let sites = ["http://www.bing.com"; "http://www.google.com"; "http://www.yahoo.com";
            "http://www.facebook.com"; "http://www.youtube.com"; "http://www.reddit.com"; "http://www.digg.com";
            "http://www.twitter.com"; "http://www.gmail.com"; "http://www.docs.google.com"; "http://www.maps.google.com";
            "http://www.microsoft.com"; "http://www.netflix.com"; "http://www.hulu.com"]
let rnd = new Random()
let server = new CacheServer();;
let clients = List.map (fun _ -> new Client(server)) [1..20]

let run =
    for c in clients do
        let i = rnd.Next()%N
        c.Get(sites.[i]);;
                                 