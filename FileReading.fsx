open System
open System.IO
open System.Threading


//following code from http://www.infoq.com/articles/pickering-fsharp-async

let openFileSynchronous() =
    use fs = new FileStream(@"C:\Program Files\Internet Explorer\iexplore.exe",
                            FileMode.Open, FileAccess.Read, FileShare.Read)
    let data = Array.create (int fs.Length) 0uy
    let bytesRead = fs.Read(data, 0, data.Length)
    printfn "Read Bytes: %i, First bytes were: %i %i %i ..." 
        bytesRead data.[1] data.[2] data.[3]

let openFileCallback() =
    let fs = new FileStream(@"C:\Program Files\Internet Explorer\iexplore.exe",
                            FileMode.Open, FileAccess.Read, 
                            FileShare.Read)
    let data = Array.create (int fs.Length) 0uy
    let callback ar =
        let bytesRead = fs.EndRead(ar)
        //fs.Dispose()
        printfn "Read Bytes: %i, First bytes were: %i %i %i ..."
            bytesRead data.[1] data.[2] data.[3]
    fs.BeginRead(data, 0, data.Length, (fun ar -> callback ar), null) |> ignore

let openFileAsynchronous =
    async { 
    use  fs = new  FileStream(@"C:\Program Files\Internet Explorer\iexplore.exe", FileMode.Open, FileAccess.Read, FileShare.Read)
    let  data = Array.create (int fs.Length) 0uy
    let!  bytesRead = fs.AsyncRead(data, 0, data.Length)
    do  printfn "Read Bytes: %i, First bytes were: 
        %i %i %i ..." bytesRead data.[1] data.[2] data.[3]
    }

#time "on";;

let f1 = openFileCallback();;
let f2 = openFileSynchronous();;
let f3 = Async.RunSynchronously openFileAsynchronous;;

//end code from http://www.infoq.com/articles/pickering-fsharp-async
