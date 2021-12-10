
import { Point } from "./geo"
import * as opentype from "opentype.js"
// @ts-ignore
import fontData from "../RobotoMono-Regular.ttf"

const font = opentype.parse(fontData)

const fontSizeScale = 700 / 750

const forceCenter = new Set("*[]{}()")

export function getSymbolPolygon(symbol: string, size: number){
  let fontSize = fontSizeScale * size
  let bb = font.getPath(symbol, 0, 0, fontSize).getBoundingBox()
  let path = font.getPath(
    symbol,
    size / 2 - (bb.x2 + bb.x1) / 2,
    forceCenter.has(symbol)
      ? size / 2 - (bb.y2 + bb.y1) / 2
      : size / 2 + (font.tables.os2.sCapHeight) * fontSize / font.unitsPerEm / 2 - Math.max(bb.y2 / 2, 0),
    fontSize,
  )
  if(!path.commands.length) return []
  let newPath = new opentype.Path()
  let el = document.createElementNS("http://www.w3.org/2000/svg", "path")
  let polygons: Point[][] = [[]]
  let p = [0, 0] as Point
  for(let [i, command] of path.commands.entries()) {
    newPath.extend([command])
    el.setAttribute("d", newPath.toPathData(10))
    let point = el.getPointAtLength(el.getTotalLength())
    if(p + "" !== [point.x, point.y] + "")
      polygons[polygons.length - 1].push(p = [point.x, point.y])
    if(command.type === "Z") {
      let p = polygons[polygons.length - 1]
      if(p[0] + "" === p[p.length - 1] + "")p.pop()
      if(i !== path.commands.length - 1) {
        polygons.push([])
        newPath = new opentype.Path()
      }
    }
  }
  return polygons
}
