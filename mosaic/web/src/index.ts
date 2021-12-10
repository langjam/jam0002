import * as rs from "../pkg/index.js"
import { ColorProfile, createTile } from "./tileObj"
import { groutColor, skyColor, tileSize } from "./constants"
import { examples } from "./examples"
import * as t from "three"
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls"
import { TrackballControls } from "three/examples/jsm/controls/TrackballControls"

console.log(rs.hi())

let canvas = document.getElementById("canvas") as HTMLCanvasElement

const scene = new t.Scene()
const camera = new t.PerspectiveCamera(75, 0, 0.1, 250)
const renderer = new t.WebGLRenderer({ canvas, antialias: true })
renderer.physicallyCorrectLights = true

const code = document.getElementById("code") as HTMLTextAreaElement
const errorBox = document.getElementById("error")!
const controlsBox = document.getElementById("controls")!
const speedBox = document.getElementById("speedBox")!
const speedInput = document.getElementById("speed")!
const pauseButton = document.getElementById("pause")!
const input = document.getElementById("input") as HTMLTextAreaElement
const output = document.getElementById("output") as HTMLTextAreaElement

code.value = examples.cgol[1]
input.value = examples.cgol[2]!

const examplesBox = document.getElementById("examples")!
for(let id in examples) {
  let span = document.createElement("span")
  span.addEventListener("click", () => {
    code.value = examples[id][1]
    let x = "code"
    if(examples[id][2]) {
      input.value = examples[id][2]!
      x = "io"
    }
    onChange()
    switchSection(document.querySelector(`#header > span[data-section='${x}']`)!)
  })
  span.textContent = examples[id][0]
  examplesBox.appendChild(span)
}

let currentSection: HTMLElement = examplesBox
for(let sectionLink of document.querySelectorAll("#header > span"))
  sectionLink.addEventListener("click", () => switchSection(sectionLink as HTMLElement))
function switchSection(sectionLink: HTMLElement){
  currentSection.style.display = "none"
  currentSection = document.getElementById(sectionLink.attributes.getNamedItem("data-section")!.value)!
  currentSection.style.display = "flex"
    document.querySelector("#header > .selected")!.classList.remove("selected")
    sectionLink.classList.add("selected")
}


const stepButton = document.getElementById("step")!

let light = new t.DirectionalLight()
light.position.set(-2, 5, -10)
light.intensity = 1
light.updateMatrixWorld()
scene.add(light)
scene.add(new t.AmbientLight(0xffffff, 2))

const [skyBoxUp, skyBoxDown, skyBoxSide] = [[skyColor, skyColor], [groutColor, groutColor], [skyColor, groutColor]].map(([a, b]) => {
  let canvas = document.createElement("canvas")
  canvas.width = 1000
  canvas.height = 1000
  let ctx = canvas.getContext("2d")!
  let grd = ctx.createLinearGradient(0, 0, 0, 500)
  grd.addColorStop(0, a)
  grd.addColorStop(1, b)
  ctx.fillStyle = grd
  ctx.fillRect(0, 0, 1000, 500)
  ctx.fillStyle = b
  ctx.fillRect(0, 500, 1000, 500)
  return canvas
})
scene.background = new t.CubeTexture([
  skyBoxSide,
  skyBoxSide,
  skyBoxUp,
  skyBoxDown,
  skyBoxSide,
  skyBoxSide,
])
scene.background.needsUpdate = true
scene.fog = new t.Fog(skyColor, 175, 250)

scene.autoUpdate = false

input.addEventListener("blur", onChange)
code.addEventListener("blur", onChange)
function onChange(){
  for(let k in tiles) {
    let tile = tiles[k][0]
    scene.remove(tile)
    tilePool[tile.name].push(tile)
    delete tiles[k]
  }
  program?.free()
  program = undefined
  try {
    program = rs.Program.new(code.value, input.value)
  }
  catch (e) {
    errorBox.textContent = `Error: ${e}`
    controlsBox.style.display = "none"
    return
  }
  errorBox.textContent = ""
  controlsBox.style.display = "flex"
  console.log(program)
  run()
}

let tiles: Record<string, [t.Object3D, string]> = {}
let program: rs.Program | undefined
let paused = false
let speed = 100

pauseButton.addEventListener("click", () => {
  paused = !paused
  pauseButton.textContent = paused ? "Play" : "Pause"
  if(!paused) {
    clearTimeout(timeout)
    run()
  }
})

speedInput.addEventListener("blur", () => {
  let newSpeed = +speedInput.textContent!
  if(isNaN(newSpeed)) newSpeed = speed
  if(newSpeed < 5) newSpeed = 5
  speed = newSpeed
  speedInput.textContent = speed + ""
  clearTimeout(timeout)
  run()
})

stepButton.addEventListener("click", step)

speedBox.addEventListener("click", () => {
  speedInput.focus()
})

let timeout: number | undefined
function run(){
  if(!paused) {
    let t = Date.now()
    let r = step()
    if(r !== null)
      timeout = setTimeout(run, (r ? speed * 3 : speed) - (Date.now() - t))
  }
}

function step(){
  if(!program) return false
  let r = program.step()
  updateMosaic()
  return r
}

camera.position.set(0, 50, 0)

const orbitControls = new OrbitControls(camera, renderer.domElement)
orbitControls.target.set(0, 0, 0)
orbitControls.enableDamping = true
orbitControls.dampingFactor = 0.05
orbitControls.screenSpacePanning = false
orbitControls.enableRotate = false
orbitControls.mouseButtons.LEFT = t.MOUSE.PAN
orbitControls.enableZoom = false
const trackballControls = new TrackballControls(camera, renderer.domElement)
trackballControls.noRoll = true
trackballControls.noPan = true
trackballControls.noRotate = true
trackballControls.minDistance = 5
trackballControls.maxDistance = 150
trackballControls.target = orbitControls.target

orbitControls.maxPolarAngle = Math.PI * .45

function tick(){
  window.requestAnimationFrame(tick)
  if(canvas.width !== window.innerWidth || canvas.height !== window.innerHeight) {
    renderer.setSize(window.innerWidth, window.innerHeight)
    camera.aspect = window.innerWidth / window.innerHeight
    camera.setViewOffset(window.innerWidth + 500, window.innerHeight, 0, 0, window.innerWidth, window.innerHeight)
    camera.updateProjectionMatrix()
  }
  trackballControls.update()
  const dist = orbitControls.getDistance()
  orbitControls.minPolarAngle = orbitControls.maxPolarAngle = Math.PI / 2 *  (1 - dist / 150) ** 2
  orbitControls.update()
  renderer.render(scene, camera)
}

function updateMosaic(){
  if(!program) return
  output.value = program.output()
  let { x_min, x_max, y_min, y_max } = program.grid_region()
  for(let x = x_min; x <= x_max; x++)
    for(let y = y_min; y <= y_max; y++) {
      let key = `${x},${y}`
      let cell = program.grid_get(x, y)
      let str = cell ? cell.color + cell.symbol : undefined
      if((tiles[key]?.[1] ?? "..") === (str ?? "..")) continue
      if(tiles[key]) {
        let tile = tiles[key][0]
        scene.remove(tile)
        tilePool[tile.name]!.push(tile)
      }
      if((str ?? "..") !== "..") {
        let char = str![1] === "." ? " " : str![1]
        let tile = getTile(...getColor(str![0]), char)
        tile.position.set(x * tileSize, 0, y * tileSize)
        tile.updateMatrixWorld()
        scene.add(tile)
        tiles[key] = [tile, str!]
      }
      else
        delete tiles[key]
    }
}


const whiteColorProfile: ColorProfile = {
  baseColor: new t.Color("#ddd"),
  variation: { h: 0, s: 0, l: .5 },
  sideMuting: { s: 0, l: .8 },
}
const blackColorProfile:ColorProfile = {
  baseColor: (() => {
    let hsl = new t.Color(groutColor).getHSL({ h: 0, s: 0, l: 0 })
    hsl.l += .04
    return new t.Color().setHSL(hsl.h, hsl.s, hsl.l)
  })(),
  variation: { h: .05, s: .05, l: .08 },
  sideMuting: { s: 1, l: 1 },
}
const bgColorProfile: Omit<ColorProfile, "baseColor"> = {
  variation: { h: .075, s: .1, l: .3 },
  sideMuting: { s: .7, l: .8 },
}
let colors: Record<string, [ColorProfile, ColorProfile]> = {
  ".": [blackColorProfile, whiteColorProfile],
  ",": [blackColorProfile, whiteColorProfile],
  "!": [whiteColorProfile, blackColorProfile],
}
let lower = [..."abcdefghijklmnopqrstuvwxyz0123456789`-=/;'[]\\"]
let upper = [..."ABCDEFGHIJKLMNOPQRSTUVWXYZ)>@#$%^&*(~+<?:\"{}|"]
let hues =  [..."qzj5upi;c2oes7hyvl-/4w8n`1rag'=t[km93fx\\6d]0b"]
let foo = hues.map((x, i) => x + x + " " + upper[lower.indexOf(x)] + upper[lower.indexOf(x)])
console.log(lower.map((x, i) => x + x + " " + upper[i] + upper[i] + " .. " + foo[i]).join("\n"))
function getColor(c: string){
  if(colors[c])
    return colors[c]
  let color = new t.Color()
  let n = c.charCodeAt(0)
  let isUpper = upper.includes(c)
  let lowerVersion = isUpper ? lower[upper.indexOf(c)] : c
  let hueInd = hues.indexOf(lowerVersion)
  let hue = hueInd === -1 ? Math.random() : hueInd / hues.length
  color.setHSL(hue, .9, .3)
  colors[c] = [{ ...bgColorProfile, baseColor: color }, whiteColorProfile]
  if(isUpper)
    colors[c].reverse()
  return colors[c]
}


let tilePool: Record<string, t.Object3D[]> = {}
function getTile(bg: ColorProfile, fg: ColorProfile, symb: string){
  let key = JSON.stringify({ bg, fg, symb })
  let existing = (tilePool[key] ??= []).pop()
  if(existing) return existing
  let tile = createTile(bg, fg, symb)
  tile.name = key
  return tile
}

onChange()
tick()
