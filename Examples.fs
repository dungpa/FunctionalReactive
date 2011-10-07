module Examples

open System
open System.Windows
open System.Windows.Forms
open FRP

//// EXAMPLES:
//atv (!!5.0) t0
//atv (sinB time) {t0 with time=17.0}
//atv (timeTransformB (time) (5.0 + time / 2.0)) t0
//
//// untilB & constE
//atv (untilB !!1.0 (constE 2.0 !!3.0)) t0
//atv (untilB !!1.0 (constE 2.0 !!3.0)) {t0 with time=5.0}
//atv (untilB !!1.0 ((constE 2.0 !!3.0) -=> !!5.0)) {t0 with time=2.5}
//
//// ==>     (transform resulting event behavior)
//atv (untilB !!1.0 ((constE 2.0 !!3) ==> (fun b -> b + 10.0))) t0
//atv (untilB !!1.0 ((constE 2.0 !!3) ==> (fun b -> (mapB float b) + 10.0))) {t0 with time=3.0}
//
//// *=>     (transform resulting event time)
//atv (untilB !!1.0 ((constE 2.0 !!3.0) *=> (fun t -> !!t + 10.0))) t0
//atv (untilB !!1.0 ((constE 2.0 !!3.0) *=> (fun t -> !!t + 10.0))) {t0 with time=3.0}

// Used for running simple behaviors on the command line
// this will loop infinitely
let rec run' b t : unit =
  let tt = {time=t}
  let bv,b' = at b tt
  printf "%O\n" bv
  run' b' (t+1.0)

//// sinB
//run' (sinB (time * 10.0)) 0.0
//
//// mapB
//run' (mapB (fun v -> (v, cos v)) time) 0.0
//
//// timeTransformB
//run' (timeTransformB time !!5.0) 0.0
//run' (time + timeTransformB time (0.0 - time)) 0.0
//
//// unitB
//atv unitB t0
//
//// predicate
//run' (untilB time (predicate (gtB time !!10000.0) 0.0 -=> !!(-5.0))) 0.0
//run' (untilB time (predicate (gtB (sinB time) !!0.0) 10000.0 -=> sinB time)) 0.0
//
//// toggle
//run' (toggle !!1.0     (predicate (gtB (sinB time) !!0.0) 0.0)
//             !!(-1.0)  (predicate (gtB !!0.0 (sinB time)) 0.0)
//             ) 0.0

// EXAMPLE: some random graphics
let randomGraphics (form: FRPForm) =
  let slider = form.sliderA
  let mouseL = form.mouseL
  let mouseL' = form.mouseL'
  let mouseX = form.mouseX
  let mouseY = form.mouseY
  let rec sequenceOfCircles g =
    // Event: when the mouse is clicked, capture the current mouse coordinates
    // and store them in a tuple
    let click = snapshot mouseL (pairB mouseX mouseY)
    let addCircle pp =
      let p = sndB pp
      let x = fstB p // from the snapshot
      let y = sndB p // from the snapshot
      // Add the circle
      layerTwoGraphics (circle white x y !!10.0) g      
    // when the left button is pressed, add a circle
    // wait for the release of the left button before recursing
    // (don't create more circles until the left is released)
    untilB g (click ==> fun a ->
      let g' = addCircle a
      untilB g' (mouseL' ==> fun _ -> sequenceOfCircles g'))
  (List.fold (fun a b -> layerTwoGraphics b a) !!BlankGraphic [
    sequenceOfCircles !!BlankGraphic
    rectangle !!Drawing.Color.Red !!150.0 !!300.0 (!!10.0*slider) slider
    circle blue (mouseX+slider+5.0) (mouseY+5.0) slider
    timeTransformB (circle green !!100.0 !!200.0 (20.0 * (2.0 + sinB time))) (Math.PI * time*slider/10.0)
    circle violet !!200.0 !!200.0 (untilB !!50.0 (form.mouseL -=> !!20.0))
    circle cyan (toggle !!300.0 mouseL mouseX mouseL') (toggle !!300.0 mouseL mouseY mouseL') (toggle !!20.0 mouseL !!50.0 mouseL')
  ])


// EXAMPLE: a solor system with several planets and moons orbiting around the sun
let solarSystem (form: FRPForm) =
  let x = form.widthB / 2.0
  let y = form.heightB / 2.0
  let sun = circle yellow x y !!20.0
  let earthX = (x+(cosB time)*130.0)
  let earthY = (y+(sinB time)*130.0)
  let earth = circle blue earthX earthY !!10.0
  let moon = circle gray (earthX + (sinB time)*20.0) (earthY + (cosB time)*20.0) !!5.0
  let venus = circle orange (x+(cosB (2.0*time))*80.0) (y+(sinB (2.0*time))*80.0) !!9.0
  let mercury = circle darkGray (x+(cosB (3.0*time))*50.0) (y+(sinB (3.0*time))*50.0) !!5.0
  let marsX = (x+(cosB (0.75*time))*170.0)
  let marsY = (y+(sinB (0.75*time))*170.0)
  let mars = circle red marsX marsY  !!9.0
  let phobos = circle gray (marsX + (sinB (1.0*time))*11.0) (marsY + (cosB (1.0*time))*14.0) !!2.0
  let deimos = circle gray (marsX + (sinB (5.0*time))*20.0) (marsY + (cosB (5.0*time))*14.0) !!3.0
  layerGraphics [sun; mercury; venus; earth; moon; mercury; mars; phobos; deimos]


// EXAMPLE: This is a program that lets you draw dots! Use the top slider to select the size,
// and the bottom slider to adjust the wobble rate. You can select between drawing red and
// blue dots be clicking on either colored-rectangle.
let painter (form: FRPForm) =
  let mouseX = form.mouseX
  let mouseY = form.mouseY
  let mouseL = form.mouseL
  let mouseL' = form.mouseL'
  let radius = form.sliderA + 3.0  // Top slider; add a constant to prevent invisible dots
  let rate = form.sliderB // bottom slider
  // a button for selecting the color red
  let redSelector = rectangle red !!10.0 !!110.0 !!50.0 !!25.0
  // a button for selecting the color blue
  let blueSelector = rectangle blue !!10.0 !!210.0 !!50.0 !!25.0
  // converts a [button] shape into a button event -- it fires when the
  // shape is clicked and selects the provided value
  let select selector value = (isClicked form selector -=> value)
  // this fires when either color is selected
  let colorSelection = (orE (select redSelector red) (select blueSelector blue))
  // the initial color will be red
  let initialColor = red
  // draw a cursor to preview what the next drawn dot will look like
  let rec cursor color = untilB (circle color mouseX mouseY radius) (colorSelection ==> setCursor)
  and setCursor color = untilB (circle color mouseX mouseY radius) (mouseL' -=> cursor color)
  // this recursively adds dots to the screen as they are create by mouse clicks
  let rec drawingCircles g color =
    // a click event. when it fires, it grabs the current mouse coordinates, etc.
    // it also captures the current time 
    let click = snapshot mouseL (listB [mouseX;mouseY;radius;rate])
    // this responds to the click event by creating the new dot
    // and returning the orignal graphics with the new dot added
    let addCircle creationTime parameters =
      let xys = sndB parameters
      let x = nthB xys !!0
      let y = nthB xys !!1
      let radius = nthB xys !!2
      let rate = nthB xys !!3
      // put the dot on top of the existing dots g
      let g' = layerTwoGraphics g (circle color x y (radius+(sinB ((creationTime+time)*rate))))
      // only show the new dot once the user releases the mouse button
      // notice that the recursive call is contained within a function (but throws away
      // the argument): this is to prevent recursion until after the mouse is released
      // (otherwise we'd get a stack overflow)
      untilB g' (mouseL' ==> fun _-> drawingCircles g' color)
    // show the current dots (g) until either a new color is selected or a new dot is added
    untilB g (orE (colorSelection ==> setColorDrawingCircles g) (click +=> addCircle))
  and setColorDrawingCircles g color = untilB g (mouseL' -=> drawingCircles g color)
  // create a blank canvas where dots can be drawn
  let canvas = drawingCircles !!BlankGraphic red
  // on screen, show: the cursor on top of the buttons, on top of the canvas
  layerGraphics [cursor initialColor; redSelector; blueSelector; canvas]


