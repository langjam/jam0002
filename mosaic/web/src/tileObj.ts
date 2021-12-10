import { allPolyEdges, Edge, Point, reconstructPolygon, v } from "./geo"
import { getSymbolPolygon } from "./getSymbolPolygon"
import { bspToConvexPolygons, diff, intersect, pointInsideBsp, polygonToBsp } from "./bsp"
import { makeVoronoi } from "./voronoi"
import { addGrout } from "./addGrout"
import { size, bgCellSize, fgCellSize, symbolGrout, thickness, tileSize, groutColor } from "./constants"
import * as t from "three"

interface Stone {
  poly: Point[],
  edges: Edge[],
  convexPolys: Point[][],
  mainColor: t.Color,
  sideColor: t.Color,
  roughness: number,
  metalness: number,
  slantDir: number,
  slantSlope: number,
  center: Point,
}

export function createTile(bg: ColorProfile, fg: ColorProfile, symbol: string): t.Object3D{
  let symbolPoly = getSymbolPolygon(symbol, size)

  const stones: Stone[] = []

  for(const [colorProfile, isFg, cellSize] of [[bg, false, bgCellSize], [fg, true, fgCellSize]] as const) {
    let letterBsp = polygonToBsp(symbolPoly.map(x => addGrout(x, symbolGrout, !isFg)!))
    let voronoi = makeVoronoi(p => isFg === !!pointInsideBsp(letterBsp, p), size, cellSize)
    for(const origPoly of voronoi) {
      let origBsp = polygonToBsp([origPoly])
      let diffedEdges = [...(isFg ? intersect : diff)(origBsp, letterBsp)]
      for(let polySansGrout of reconstructPolygon(diffedEdges)) {
        let poly = addGrout(polySansGrout)
        if(poly) {
          let [min, max] = getBoundingBox(poly)
          let convexPolys = [...bspToConvexPolygons(polygonToBsp([poly]), [])]
          let [convexMin, convexMax] = getBoundingBox(convexPolys.flat())

          let tolerance = 1
          if(
            false
            || convexMin[0] + tolerance < min[0]
            || convexMin[1] + tolerance < min[1]
            || convexMax[0] - tolerance > max[0]
            || convexMax[1] - tolerance > max[1]
          )
            return createTile(bg, fg, symbol)



          let edges = allPolyEdges([poly])

          let mainColor = sampleColorProfile(colorProfile)
          let sideColor = getSideColor(colorProfile, mainColor)

          let center = [min[0] / 2 + max[0] / 2, min[1] / 2 + max[1] / 2] as const
          let slantDir = Math.random() * Math.PI * 2
          let slantAmount = Math.random() * thickness
          let slantSlope = slantAmount / (v.mag(v.sub(max, min)) * tileSize / size)

          stones.push({
            poly,
            edges,
            convexPolys,
            mainColor,
            sideColor,
            roughness: .35 + Math.random() * .05,
            metalness: .05 + Math.random() * .05,
            slantDir,
            slantSlope,
            center,
          })
        }
      }
    }
  }

  const lod = new t.LOD()
  lod.addLevel(highResTile(stones), 40)
  lod.addLevel(lowResTile(stones), 50)
  return lod
}

function lowResTile(stones: Stone[]){
  const textureSize = 500

  const createTexture = () => {
    const canvas = document.createElement("canvas")
    const ctx = canvas.getContext("2d")!
    canvas.width = textureSize
    canvas.height = textureSize
    const texture = new t.CanvasTexture(canvas)
    ctx.imageSmoothingQuality = "high"
    texture.onUpdate = () => delete texture.image
    return [ctx, new t.CanvasTexture(canvas)] as const
  }

  const [albedo, albedoMap] = createTexture()
  const [surface, surfaceMap] = createTexture()
  const [normal, normalMap] = createTexture()

  albedo.fillStyle = groutColor
  surface.fillStyle = rgb(0, 1, 0)
  normal.fillStyle = rgb(.5, .5, 1)
  for(const ctx of [albedo, surface, normal])
    ctx.fillRect(0, 0, textureSize, textureSize)


  for(const stone of stones) {
    albedo.fillStyle = rgb(stone.mainColor.r, stone.mainColor.g, stone.mainColor.b)
    surface.fillStyle = rgb(0, stone.roughness, stone.metalness)
    const h = Math.sqrt(1 / stone.slantSlope ** 2 + 1)
    let x = [Math.cos(stone.slantDir) * .5 / h + .5, -Math.sin(stone.slantDir) * .5 / h + .5, 1 / stone.slantSlope * .5 / h + .5] as const
    normal.fillStyle = rgb(...x)
    for(const ctx of [albedo, surface, normal]) {
      ctx.beginPath()
      ctx.moveTo(...v.scl(stone.poly[stone.poly.length - 1], textureSize / size))
      for(let p of stone.poly)
        ctx.lineTo(...v.scl(p, textureSize / size))
      ctx.fill()
      ctx.fill()
      ctx.closePath()
    }
  }

  const geo = new t.PlaneGeometry(tileSize, tileSize)
  const mat = new t.MeshStandardMaterial({ transparent: true })
  mat.map = albedoMap
  mat.roughnessMap = surfaceMap
  mat.metalnessMap = surfaceMap
  mat.normalMap = normalMap
  mat.normalMapType = t.ObjectSpaceNormalMap

  const tile = new t.Mesh(geo, mat)
  tile.rotation.x = -Math.PI / 2
  tile.position.y = thickness

  return tile

  function rgb(r: number, g: number, b: number){
    return `rgb(${r * 256 | 0}, ${g * 256 | 0}, ${b * 256 | 0})`
  }
}

function highResTile(stones: Stone[]){
  const tile = new t.Group()
  for(let stone of stones) {
    let edges = stone.edges
    let polys = stone.convexPolys

    let slantDirV = v.scl([Math.cos(stone.slantDir), Math.sin(stone.slantDir)], stone.slantSlope * tileSize / size)

    let th = (x: number, y: number) =>
      thickness - v.dot(slantDirV, v.sub([x, y], stone.center))

    const topGeo = makeGeo(
      polys.flatMap(poly => poly.length > 2 ? [...Array(poly.length - 2)].flatMap((_, i) =>
        [
          poly[i + 1],
          poly[0],
          poly[i + 2],
        ].map(([x, y]) => [x * tileSize / size, th(x, y), y * tileSize / size]),
      ) : []),
    )

    const sideGeo = makeGeo(
      edges.flatMap(edge =>
        ([
          [edge[1], 1],
          [edge[0], 1],
          [edge[1], 0],
          [edge[0], 1],
          [edge[0], 0],
          [edge[1], 0],
        ] as const).map(([[x, y], z]) =>
            [x * tileSize / size, z * th(x, y), y * tileSize / size] as const,
        ),
      ),
    )

    const topMat = new t.MeshStandardMaterial({
      color: stone.mainColor,
      roughness: stone.roughness,
      metalness: stone.metalness,
    })

    const sideMat = new t.MeshStandardMaterial({
      color: stone.sideColor,
    })

    const topMesh = new t.Mesh(topGeo, topMat)
    const sideMesh = new t.Mesh(sideGeo, sideMat)
    tile.add(topMesh, sideMesh)
  }
  tile.position.set(-.5 * tileSize, 0, -.5 * tileSize)
  return tile
}

function makeGeo(verts: (readonly [number, number, number])[]){
  const geometry = new t.BufferGeometry()
  const vertices = new Float32Array(verts.length * 3)

  let i = 0

  for(let v of verts) {
    vertices[i++] = v[0]
    vertices[i++] = v[1]
    vertices[i++] = v[2]
  }

  geometry.setAttribute("position", new t.BufferAttribute(vertices, 3))
  geometry.computeVertexNormals()

  return geometry
}

export interface ColorProfile {
  baseColor: t.Color,
  variation: { h: number, s: number, l: number },
  sideMuting: { s: number, l: number },
}

function sampleColorProfile(profile: ColorProfile){
  let hsl = profile.baseColor.getHSL({ h: 0, s: 0, l: 0 })
  hsl.h += (Math.random() - .5) * profile.variation.h
  hsl.h = (hsl.h + 1) % 1
  hsl.s += (Math.random() - .5) * profile.variation.s
  hsl.s = Math.max(Math.min(hsl.s, 1), 0)
  hsl.l += (Math.random() - .5) * profile.variation.l
  hsl.l = Math.max(Math.min(hsl.l, 1), 0)
  let color = new t.Color()
  color.setHSL(hsl.h, hsl.s, hsl.l)
  return color
}

function getSideColor(profile: ColorProfile, primary: t.Color){
  let hsl = primary.getHSL({ h: 0, s: 0, l: 0 })
  hsl.s *= profile.sideMuting.s
  hsl.l *= profile.sideMuting.l
  let color = new t.Color()
  color.setHSL(hsl.h, hsl.s, hsl.l)
  return color
}

function getBoundingBox(poly: Point[]): [Point, Point]{
  return [
    poly.reduce((a, b) => [Math.min(a[0], b[0]), Math.min(a[1], b[1])]),
    poly.reduce((a, b) => [Math.max(a[0], b[0]), Math.max(a[1], b[1])]),
  ]
}
