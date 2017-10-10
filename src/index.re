let canvas = Canvas.getCanvasById "canvas";

let canvasParent = Canvas.parentNode canvas;

Canvas.setWidth canvas (Canvas.clientWidth canvasParent);

Canvas.setHeight canvas (Canvas.clientHeight canvasParent);

let context = Canvas.getContext canvas `twoD;

let width = float_of_int @@ Canvas.getWidth canvas;

let height = float_of_int @@ Canvas.getHeight canvas;

let center = (width /. 2., height /. 2.);

module Particle = {
  type t = {
    position: Vector2.t,
    velocity: Vector2.t,
    colour: (int, int, int)
  };
  let make position velocity colour => {position, velocity, colour};
  let draw context {position, colour} => Canvas.drawCircle context ::colour center::position 5.0;
  let update g {position, velocity, colour} => {
    let newVelocity = Vector2.add velocity (Vector2.mul Vector2.down g);
    {position: Vector2.add position newVelocity, velocity: newVelocity, colour}
  };
};

type state = {
  particles: list Particle.t,
  gravity: float
};

let state: ref state = ref {particles: [], gravity: 0.015};

type msg =
  | Tick
  | SpawnParticle Vector2.t Vector2.t
  | ChangeGravity float;

let update msg {particles, gravity} =>
  switch msg {
  | Tick => {particles: List.map (Particle.update gravity) particles, gravity}
  | SpawnParticle pos vel =>
    let colour = (Random.int 256, Random.int 256, Random.int 256);
    {particles: [Particle.make pos vel colour, ...particles], gravity}
  | ChangeGravity v =>
    let g = v /. 100. *. 0.1;
    {particles, gravity: g}
  };

let draw state => {
  Canvas.clearCanvas canvas context;
  List.iter (Particle.draw context) state
};

let dispatch msg => {
  let newState = update msg !state;
  state := newState;
  draw (!state).particles
};

external setInterval : (unit => unit) => int => unit = "setInterval" [@@bs.val];

setInterval (fun () => dispatch Tick) 10;

setInterval (fun () => dispatch (SpawnParticle center (Vector2.randomUnit ()))) 100;

type event;

let jsEventValueGetter: event => float = [%bs.raw
  {|
    function (e) {
        return e.target.value;
    }
 |}
];

type domElement;

external addEventListener : domElement => string => (event => unit) => unit = "" [@@bs.send];

let i = [%bs.raw {| document.getElementById('range') |}];

addEventListener i "change" (fun e => dispatch (ChangeGravity (jsEventValueGetter e)));
