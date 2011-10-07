module FRP

type Ctx = {time:float}
let t0 : Ctx = {time=0.0}
type 'a Behavior = Behavior of (Ctx -> ('a * 'a Behavior))
type 'a Event = Event of (Ctx -> (float * (unit -> 'a)))

let rec time = Behavior (fun {time=v} -> (v, time))

let at b = match b with Behavior cv -> cv
let atv b = match b with Behavior cv -> fun c -> fst(cv c)
let atb b = match b with Behavior cv -> fun c -> snd(cv c)
let occ e c = match e with Event tv -> tv c

let rec lift0 v = Behavior (fun _ -> (v, lift0 v))

let rec mapB f b = Behavior(fun c -> let v,b' = at b c in f v, mapB f b')
let rec timeTransformB b (bt: float Behavior) =
  Behavior(fun c -> let vt,bt' = at bt c in let v,b' = at b {c with time=vt} in (v, timeTransformB b' bt'))

let rec binaryFunB f a b = Behavior (fun c ->
  let av,a' = at a c
  let bv,b' = at b c
  f av bv, binaryFunB f a' b')

let rec unaryFunB f a = Behavior (fun c ->
  let av,a' = at a c
  f av, unaryFunB f a')

let addB = binaryFunB (+)
let addfB a b : float Behavior = binaryFunB (+) a b
let eqB a b = binaryFunB (=) a b
let gtB a b = binaryFunB (>) a b
let gteB a b = binaryFunB (>=) a b
let ltB a b = binaryFunB (<) a b
let lteB a b = binaryFunB (<=) a b
let andB a b = binaryFunB (&&) a b
let orB a b = binaryFunB (||) a b

let (+=>) e f = Event(fun c -> let t,a = occ e c in t, fun () -> f t (a())) 
let (==>) e f = e +=> fun t x -> f x
let ( *=> ) e f = e +=> fun t x -> f t
let (-=>) e b = e +=> fun t x -> b

let rec untilB a e = Behavior(fun c ->
  let av,a' = at a c
  let t, fb = occ e c
  if c.time >= t then at (fb()) {c with time=t} else (av, untilB a' e)
)

let pairB a b = binaryFunB (fun av bv -> (av,bv)) a b
let fstB a = unaryFunB fst a
let sndB a = unaryFunB snd a

//let snapshot e b = Event(fun c ->
//  let t, a' = occ e c
//  let rec snp a = Behavior(fun c ->
//    let bv, b' = at b {c with time=t}
//    let av, a' = at a c
//    ((av, bv), snp a')
//  )
//  t, fun () -> snp (a'())
//  )

let snapshot e b = Event(fun c ->
  let t,a = occ e c
  t, if c.time >= t then (fun() -> pairB (a()) (lift0 (atv b {c with time=t}))) else (fun () -> pairB (a()) b)
)

let unitB = lift0 ()

let predicate b t =
  Event(fun c -> (if atv b c then t else infinity), fun () -> unitB)

let constE t v = Event(fun _ -> t, v)

let rec externalE c = Event(fun {time=t} -> (if c() then t else infinity), fun() -> unitB)



let rec seqB a b = Behavior(fun c ->
  let (),a' = at a c
  let (),b' = at b c
  ((), seqB a' b')
)

let rec consB a b = Behavior(fun c ->
  let av,a' = at a c
  let bv,b' = at b c
  (av::bv, consB a' b')
)

type 'a Behavior with
  static member (+) (a, b) : int Behavior = binaryFunB (+) a b
  static member (-) (a, b) : int Behavior = binaryFunB (-) a b
  static member (*) (a, b) : int Behavior = binaryFunB (*) a b
  static member (/) (a, b) : int Behavior = binaryFunB (/) a b
  static member (+) (a:int Behavior, b:int) = binaryFunB (+) a (lift0 b)
  static member (-) (a:int Behavior, b:int) = binaryFunB (-) a (lift0 b)
  static member (*) (a:int Behavior, b:int) = binaryFunB (*) a (lift0 b)
  static member (/) (a:int Behavior, b:int) = binaryFunB (/) a (lift0 b)
  static member (+) (a:int, b:int Behavior) = binaryFunB (+) (lift0 a) b
  static member (-) (a:int, b:int Behavior) = binaryFunB (-) (lift0 a) b
  static member (*) (a:int, b:int Behavior) = binaryFunB (*) (lift0 a) b
  static member (/) (a:int, b:int Behavior) = binaryFunB (/) (lift0 a) b
  static member (+) (a, b) : float Behavior = binaryFunB (+) a b
  static member (-) (a, b) : float Behavior = binaryFunB (-) a b
  static member (*) (a, b) : float Behavior = binaryFunB (*) a b
  static member (/) (a, b) : float Behavior = binaryFunB (/) a b
  static member (+) (a:float Behavior, b:float) = binaryFunB (+) a (lift0 b)
  static member (-) (a:float Behavior, b:float) = binaryFunB (-) a (lift0 b)
  static member (*) (a:float Behavior, b:float) = binaryFunB (*) a (lift0 b)
  static member (/) (a:float Behavior, b:float) = binaryFunB (/) a (lift0 b)
  static member (+) (a:float, b:float Behavior) = binaryFunB (+) (lift0 a) b
  static member (-) (a:float, b:float Behavior) = binaryFunB (-) (lift0 a) b
  static member (*) (a:float, b:float Behavior) = binaryFunB (*) (lift0 a) b
  static member (/) (a:float, b:float Behavior) = binaryFunB (/) (lift0 a) b

let sinB = unaryFunB (fun (v:float) -> sin v)
let cosB = unaryFunB (fun (v:float) -> cos v)
let sqrtB = unaryFunB sqrt
let atan2B = binaryFunB atan2


// converts a literal value into a behavior
let (!!) = lift0

// has behavior a until e1 fires, then has behavior b until e2 fires
// this ignores the resulting behaviors of e1 and e2
let rec toggle a e1 b e2 =
  untilB a (e1 ==> fun _ -> toggle b e2 a e1 )




////////////////////////////////////////////
// UI UI UI UI UI UI UI UI UI UI

open System
open System.Windows
open System.Windows.Forms

type Graphics =
  | Circle of float * float * float * Drawing.Color              // x y radius color
  | Ellipse of float * float * float * float * Drawing.Color     // x y w h color
  | Rectangle of float * float * float * float * Drawing.Color   // x y w h color
  | Line of ((float * float) list) * Drawing.Color   // x y w h color
  | Layers of Graphics * Graphics // layers graphics on top of each other
  | BlankGraphic // displays nothing

let ellipse color x y w h =
  let rec shapeBeh color x y w h = Behavior(fun c ->
    let xv,x' = at x c
    let yv,y' = at y c
    let wv,w' = at w c
    let hv,h' = at h c
    let cv,color' = at color c
    (Ellipse(xv, yv, wv, hv, cv), shapeBeh color' x' y' w' h')
  )
  shapeBeh color x y w h 

let rectangle color x y w h =
  let rec shapeBeh color x y w h = Behavior(fun c ->
    let xv,x' = at x c
    let yv,y' = at y c
    let wv,w' = at w c
    let hv,h' = at h c
    let cv,color' = at color c
    (Rectangle(xv, yv, wv, hv, cv), shapeBeh color' x' y' w' h')
  )
  shapeBeh color x y w h 

let circle color x y r =
  let rec shapeBeh color x y r = Behavior(fun c ->
    let xv,x' = at x c
    let yv,y' = at y c
    let rv,r' = at r c
    let cv,color' = at color c
    (Circle(xv, yv, rv, cv), shapeBeh color' x' y' r')
  )
  shapeBeh color x y r

// Some basic colors
let red = !!Drawing.Color.Red
let blue = !!Drawing.Color.Blue
let violet = !!Drawing.Color.Violet
let green = !!Drawing.Color.Green
let cyan = !!Drawing.Color.Cyan
let black = !!Drawing.Color.Black
let white = !!Drawing.Color.White
let yellow = !!Drawing.Color.Yellow
let gray = !!Drawing.Color.Gray
let orange = !!Drawing.Color.Orange
let darkGray = !!Drawing.Color.DarkGray

// A window to display graphical behaviors
type FRPForm() as form =
  inherit Form()
  let theTimer = new Timer()
  let mutable CurrentVisual : Graphics = BlankGraphic
  let mutable mouseLeftPressed = false
  let mutable mouseRightPressed = false
  let mutable behavior : Graphics Behavior = !!BlankGraphic
  let sliderControlA = new TrackBar()
  let sliderControlB = new TrackBar()
  do form.InitializeForm
  member this.InitializeForm =
    sliderControlA.Location<- System.Drawing.Point(0,0)
    sliderControlA.Minimum<- 0
    sliderControlA.Maximum<- 100
    sliderControlA.Width<- 300
    sliderControlB.Location<- System.Drawing.Point(0,sliderControlA.Height)
    sliderControlB.Minimum<- -20
    sliderControlB.Maximum<- 20
    sliderControlB.Width<- 300
    this.BackColor<- Drawing.Color.Black
    this.Width <- 300
    this.Height <- 300
    this.FormBorderStyle <- FormBorderStyle.Sizable
    this.Text<- "FRP"
    this.Controls.Add(sliderControlA)
    this.Controls.Add(sliderControlB)
    theTimer.Interval<- 20
    theTimer.Start()
    theTimer.Tick |> Event.add(fun _ -> form.Run behavior)
    this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.UserPaint ||| ControlStyles.DoubleBuffer, true)
    this.SetStyle(ControlStyles.FixedHeight ||| ControlStyles.FixedWidth, false)
    this.MouseDown  |> Event.add(fun (e:MouseEventArgs) ->
      if e.Button=MouseButtons.Left then mouseLeftPressed<- true
      if e.Button=MouseButtons.Right then mouseRightPressed<- true
      )
    this.MouseUp |> Event.add(fun (e:MouseEventArgs) ->
      if e.Button=MouseButtons.Left then mouseLeftPressed<- false
      if e.Button=MouseButtons.Right then mouseRightPressed<- false
      )
  member this.Run b =
    let t = {time=DateTime.Now.ToOADate() * 100000.0}
    let bv,b' = at b t
    this.Behavior<- b'
    CurrentVisual<- bv
    this.Invalidate()
  member this.mouseL = externalE (fun() -> mouseLeftPressed)
  member this.mouseL' = externalE (fun() -> not mouseLeftPressed)
  member this.mouseR = externalE (fun() -> mouseRightPressed)
  member this.mouseR' = externalE (fun() -> not mouseRightPressed)
  member this.mouseX = Behavior (fun _ -> ((float) (this.PointToClient Cursor.Position).X, this.mouseX))
  member this.mouseY = Behavior (fun _ -> ((float) (this.PointToClient Cursor.Position).Y, this.mouseY))
  member this.widthB = Behavior (fun _ -> ((float) this.Width, this.widthB))
  member this.heightB = Behavior (fun _ -> ((float) this.Height, this.heightB))
  member this.sliderA = Behavior (fun _ -> ((float) sliderControlA.Value, this.sliderA))
  member this.sliderB = Behavior (fun _ -> ((float) sliderControlB.Value, this.sliderB))
  member this.Behavior with set(value) = behavior<- value
  override this.OnPaint e =
    base.OnPaint e
    let rec draw g =
      let color c =
        new Drawing.SolidBrush(c)
      let drawEllipse (x:float) (y:float) (w:float) (h:float) c =
        e.Graphics.FillEllipse(c,float32 x,float32 y,float32 w,float32 h)
      let drawRectangle (x:float) (y:float) (w:float) (h:float) c =
        e.Graphics.FillRectangle(c,float32 x,float32 y,float32 w,float32 h)
      match g with
      | Layers (g1,g2) -> draw g1; draw g2
      | Circle(x,y,r,c) -> drawEllipse (x-r) (y-r) (2.0*r) (2.0*r) (color c)
      | Ellipse(x,y,w,h,c) -> drawEllipse x y w h (color c)
      | Rectangle(x,y,w,h,c) -> drawRectangle x y w h (color c)
      | _ -> ()
    draw CurrentVisual

let layerTwoGraphics a b = binaryFunB (fun g1 g2 -> Layers(g1,g2)) a b
let layerGraphics = List.fold (fun a b -> layerTwoGraphics b a) !!BlankGraphic
  


let andE e1 e2 = Event(fun c ->
  let t1,b1 = occ e1 c
  let t2,b2 = occ e2 c
  if c.time >= t1 && c.time >= t2 then
    (Math.Max(t1,t2), fun () -> pairB (b1()) (b2()))
  else
    (infinity, fun () -> pairB (b1()) (b2()))
)

let orE e1 e2 = Event(fun c ->
  let t1,b1 = occ e1 c
  let t2,b2 = occ e2 c
  if c.time >= t1 then
    (t1, b1)
  elif c.time >= t2 then
    (t2, b2)
  else
    (infinity, b1) // dummy value
)
  


// A behavior that is true only if the mouse is within the shape
let rec mouseWithin (form:FRPForm) g = Behavior(fun c ->
  let inCircle x y r mx my = r >= (Math.Sqrt(mx*mx + my*my))
  let inRect x y w h mx my = x <= mx && x+w>=mx && my>=y && (y+h)>=my
  let gv,g' = at g c
  let mx = atv form.mouseX c
  let my = atv form.mouseY c
  let v =
    match gv with 
    | Circle(x,y,r,c) -> inCircle x y r mx my
    | Rectangle(x,y,w,h,c) -> inRect x y w h mx my
    | _ -> false // other shapes are currently not handled
  (v, mouseWithin form g')
  )

// event: when the mouse is within the shape and the left button is pressed
let isClicked (form:FRPForm) g = andE (predicate (mouseWithin form g) 0.0) form.mouseL

// Converts a list of behaviors into a behavior that results in a list of behaviors
// (wraps the list up as a behavior)
let rec listB list = Behavior(fun c ->
  let lv,l' = List.unzip (List.map (fun a -> at a c) list)
  (lv, listB l')
)

// Gets the nth item in a list of behaviors
let nthB list index = binaryFunB (fun lv idx -> List.nth lv idx) list index

