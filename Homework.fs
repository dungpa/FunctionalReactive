module Homework

open System

#if INTERACTIVE
  #r "FSharp.PowerPack.Parallel.Seq";;
  #time;;
#endif

(*The following gives us random arrays *)
let rand = new Random()

let rec getRandListHelper n res =
    if n>0 then getRandListHelper (n-1) (rand.Next(1000000)::res) else res

let getRandArray n =
    getRandListHelper n [] |> Seq.toArray

(*Here is a function that performs a quicksort on a given array. *)
let aRand (s:int[]) = s.[rand.Next(Array.length s)]

let rec qsort (s : int[]) =
    if s.Length < 2 then s
    else let p = aRand s
         let les = Array.filter ((>)p) s
         let eqs = Array.filter ((=)p) s
         let ges = Array.filter ((<)p) s
         let svs = Array.map qsort [|les;ges|]
         Array.append svs.[0] (Array.append eqs svs.[1])

let x = getRandArray 1000000 

let result = qsort x

(*Solve these problems:

PROBLEM 1: Create a function "qsortPar1" that tries to do as many steps
in parallel as possible. On a machine with infinite processors this
program would be extremely fast, but on our machines it will probably
be much slower than even the sequential program. 

Report the performance change for sizes 1000, 10000, 100000, 1000000.
Report anything that takes longer than a minute as a timeout. 
(If your interactive window is stuck you can right click it and reset session)
Also describe the processor you run it on.

For this problem you may find the F# power pack useful. 
You can download it at http://fsharppowerpack.codeplex.com/

If you would like more information about asyncs and the Async class these might be helpful:
http://msdn.microsoft.com/en-us/library/ee370232.aspx
http://msdn.microsoft.com/en-us/library/dd233250.aspx

The code
#if INTERACTIVE
  #r "FSharp.PowerPack.Parallel.Seq";;
  #time;;
#endif
allows the interactive window to use the powerpack.
You might still have errors in your visual studio window
but your program is acceptable if it runs in the interactive window.
The #time switch will cause the interactive window to print how long any operation it runs took.
For example if you type #time;; into your interactive window you will see:
--> Timing now on

From then on in that interactive session, any time you run a command in your interactive window it will look like:

Real: 00:00:01.417, CPU: 00:00:01.653, GC gen0: 110, gen1: 32, gen2: 5

val result : int [] = ....

The number we are interested in is Real

ANSWER 1:
qsort 1000:
qsort 10000:
qsort 100000:
qsort 1000000:
qsortPar1 1000:
qsortPar1 10000:
qsortPar1 100000:
qsortPar1 1000000:

Processor:

PROBLEM 2: Explain in a sentence what caused the performance decrease you saw. 
Compare this problem to the prime number example from class.
Explain how the design is similar or different to the prime number example 
and relate this similarity or difference to the performance changes from each.

The examples from class can be found at http://www.cs.princeton.edu/courses/archive/fall10/cos597C/docs/asynchclassdemo.fs

ANSWER 2:
Explain performance change:

Comparison to prime number example:


Problem 3: Write a new function qsortPar2 that improves in performance over qsort and qsortPar1. 
Because qsortPar1 is completely parallelized you should only be decreasing the amount of parallelization
in this step. Report the performance increase. Finally describe in a few sentences any other designs you tried
leading up to your final implemenation of qsortPar2

ANSWER 3:
qsort 1000:
qsort 10000:
qsort 100000:
qsort 1000000:
qsortPar2 1000:
qsortPar2 10000:
qsortPar2 100000:
qsortPar2 1000000:

Description of design process:

*)
