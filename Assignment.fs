module Assignment

open System
open System.Windows
open System.Windows.Forms
open FRP
open Examples


// Assignment: Orbital Painter
// Create an "orbital painter" which starts out with a yellow sun in the center of the window
// Let the "active planet" be the last planet created (initially the sun)
// When the user clicks the mouse, create a new planet/moon that orbits around
// the active planet (this new planet becomes the new active planet). The radius of the orbit should
// be the distance that the mouse was from the active planet when the mouse was clicked (use snapshot)
// The planet should begin its transit at the spot where the mouse clicked (use atan2B).
// Use sliderA to set the diameter for the next new moon, and sliderB to control
// the angular velocity (which allows both negative and positive velocities)
let orbitalPainter (form: FRPForm) =
  let mouseX = form.mouseX
  let mouseY = form.mouseY
  let mouseL = form.mouseL
  let mouseL' = form.mouseL'
  let newPlanetRadius = 3.0 + form.sliderA / 3.0
  let angularVelocity = form.sliderB / 5.0
  let cursor = circle green mouseX mouseY newPlanetRadius
  let rec orbitalPainter' activePlanet centerX centerY =
    activePlanet // replace with code that adds new orbiting planets
  let x = !!100.0 // replace with a behavior that is the center of the form
  let y = !!100.0 // replace with behavior that is the center of the form
  let sun = !!BlankGraphic // replace with a default yellow circle
  layerGraphics [cursor; orbitalPainter' sun x y]


// Create a window
let form = new FRPForm()

// SET YOUR BEHAVIOR HERE
//form.Behavior<- randomGraphics form
//form.Behavior<- painter form
form.Behavior<- solarSystem form
form.Behavior<- orbitalPainter form

do Application.Run(form)
