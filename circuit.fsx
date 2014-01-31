open System
open System.Drawing
open System.Windows.Forms

// Create a form to display the graphics
let width, height = 1000, 600         
let form = new Form(Width = width, Height = height)
let box = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill)
let image = new Bitmap(width, height)
let graphics = Graphics.FromImage(image)
let brush = new SolidBrush(Color.FromArgb(0, 0, 0))

box.Image <- image
form.Controls.Add(box) 

// Compute the endpoint of a line
// starting at x, y, going at a certain angle
// for a certain length. 
let endpoint x y angle length =
    x + length * cos angle,
    y + length * sin angle

let flip x = (float)height - x

// Utility function: draw a line of given width, 
// starting from x, y
// going at a certain angle, for a certain length.
let drawLine (target : Graphics) (brush : Brush) 
             (x : float) (y : float) 
             (angle : float) (length : float) (width : float) =
    let x_end, y_end = endpoint x y angle length
    let origin = new PointF((single)x, (single)(y |> flip))
    let destination = new PointF((single)x_end, (single)(y_end |> flip))
    let pen = new Pen(brush, (single)width)
    target.DrawLine(pen, origin, destination)

let draw x y angle length width = 
    drawLine graphics brush x y angle length width

// helper function for drawing the resistor -- recursively draw list of segments
let rec drawPath originX originY (vectors:(float*float) list) = 
    match vectors with
    | (theta, r) :: t -> 
        draw originX originY theta r 3.
        let newX, newY = endpoint originX originY theta r
        drawPath newX newY t
    | [] -> ()

let pi = Math.PI
let up = 0.5 * pi
let down = 1.5 * pi
let upRight = 0.25 * pi
let downLeft = -0.25 * pi

type [<Measure>] C          // coulomb measures charge
type [<Measure>] s          // second measures time
type [<Measure>] J          // joule measures energy
type [<Measure>] A = C/s    // ampere measures time flow rate of charge
type [<Measure>] V = J/C    // volt measures energy per unit charge, EMF
type [<Measure>] F = C/J    // farad measures capacitance, how many coulombs
                            //    of charge on an object per joule of energy input
type [<Measure>] O = V/A    // ohms measure resistance, how many volts required
                            //    per resulting amp of charge flow obtained

type CircuitElement = 
    | Wire
    | Battery of float<V>   // voltage of the battery
    | Resistor of float<O>  // resistance of the resistor
    | Capacitor of float<F> // capacitance of the capacitor
    | Series of CircuitElement * CircuitElement
    | Parallel of CircuitElement * CircuitElement

let (---) c1 c2 = Series (c1,c2)
   
let (|||) c1 c2 = Parallel (c1,c2)

let myCircuit = (Wire --- 
                    (
                        ((Resistor 5.0<O>) --- (Capacitor 3.0<F>) --- (Battery 1.0<V>)) 
                        ||| 
                        ((Battery 2.0<V>) --- 
                            (
                                (Resistor 4.0<O>) 
                                ||| 
                                ((Resistor 5.0<O>) --- (Resistor 1.0<O>)))
                            )
                    )
                --- Wire)

let rec resistance c =
    match c with
    | Wire | Battery _ -> 0.0<O>
    | Capacitor _ -> infinity * 1.0<O>
    | Resistor r -> r
    | Series (c1,c2) -> (resistance c1) + (resistance c2)
    | Parallel (c1,c2) -> 1.0/(1.0/(resistance c1) + 1.0/(resistance c2))

let drawWire x1 x2 y = 
    draw x1 y 0. (x2-x1) 3.

let drawCapacitor x1 x2 y =
    let wirelen = (0.5 * (x2 - x1) - 10.)
    let t1x = x1 + wirelen
    let t2x = x2 - wirelen
    draw x1 y 0. ((0.5 * (x2-x1))-10.) 3.
    draw (x1 + (0.5 * (x2 - x1) + 10.)) y 0. ((0.5 * (x2-x1))-10.) 3.
    draw t1x y up 20. 3.
    draw t1x y down 20. 3.
    draw t2x y up 20. 3.
    draw t2x y down 20. 3.

let drawBattery x1 x2 y =
    let wirelen = (0.5 * (x2 - x1) - 10.)
    let t1x = x1 + wirelen
    let t2x = x2 - wirelen
    draw x1 y 0. ((0.5 * (x2-x1))-10.) 3.
    draw (x1 + (0.5 * (x2 - x1) + 10.)) y 0. ((0.5 * (x2-x1))-10.) 3.
    draw t1x y up 20. 3.
    draw t1x y down 20. 3.
    draw t2x y up 10. 3.
    draw t2x y down 10. 3.

let drawResistor x1 x2 y =
    let wirelen = (0.5 * (x2-x1) - 30.)
    let t1x = x1 + wirelen
    let t2x = x2 - wirelen
    draw x1 y 0. wirelen 3.
    draw x2 y pi wirelen 3.
    drawPath t1x y [(upRight, 11.);(downLeft, 22.);(upRight,22.);(downLeft,22.);(upRight,11.)]

let rec circuitLength c =
    match c with
    | Wire | Battery _ |  Resistor _ | Capacitor _ -> 1     
    | Parallel (c1,c2) -> [c1;c2] |> Seq.map circuitLength |> Seq.max
    | Series (c1,c2) -> (circuitLength c1) + (circuitLength c2)

let rec circuitWidth c =
    match c with
    | Wire | Battery _ | Resistor _ | Capacitor _ -> 1
    | Parallel (c1,c2) -> (circuitWidth c1) + (circuitWidth c2)
    | Series (c1,c2) -> [c1;c2] |> Seq.map circuitWidth |> Seq.max
 
let rec drawCircuit c x1 x2 y =
    match c with
    | Wire -> drawWire x1 x2 y
    | Battery _ -> drawBattery x1 x2 y
    | Resistor _ -> drawResistor x1 x2 y
    | Capacitor _ -> drawCapacitor x1 x2 y
    | Series (c1,c2) ->
        let c1Length = (float)(circuitLength c1)
        let totalLength = c1Length + (float)(circuitLength c2) 
        let midpoint =  (c1Length/totalLength) * (x1 + x2)
        drawCircuit c1 x1 midpoint y
        drawCircuit c2 midpoint x2 y
    | Parallel (c1,c2) ->
        let widthTop = 60.0 * (float)(circuitWidth c1)
        let widthBottom = 60.0 * (float)(circuitWidth c2)
        let width = widthTop + widthBottom
        drawCircuit c1 x1 x2 (y + widthTop)
        drawCircuit c2 x1 x2 (y - widthBottom)
        draw x1 (y-widthBottom) up width 2.
        draw x2 (y-widthBottom) up width 2.



drawCircuit myCircuit 50. 950. 400. 

        

form.ShowDialog()













