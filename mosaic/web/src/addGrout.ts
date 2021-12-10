
import { Point, getSide, v } from "./geo"

const angleRange = 1 / 3
const radiusMin = 2.5
const radiusMax = radiusMin + 1.5

export function addGrout(polygon: Point[], scale: number = 1, reverse = false){
  polygon = polygon.filter((x, i, a) => v.mag(v.sub(x, a[(i + 1) % a.length])) > 8)
  if(reverse) polygon.reverse()
  if(polygon.length < 3) return null
  let newPolygon: Point[] = []
  for(const [i, b] of polygon.entries()) {
    let a = polygon[(i + polygon.length - 1) % polygon.length]
    let c = polygon[(i + 1) % polygon.length]
    let concave = getSide([a, b], c) !== -1
    let minV = concave ? v.sub(b, a) : v.sub(c, b)
    let maxV = concave ? v.sub(b, c) : v.sub(a, b)
    let [minA, maxA] = [minV, maxV].map(x => Math.atan2(...v.rsz(x)))
    if(Math.abs(Math.abs(maxA - minA) - Math.PI) < Math.PI * 2 / 360) continue
    let diffA = (maxA < minA ? (Math.PI * 2 + maxA - minA) : maxA - minA)
    let angle = minA + (Math.random() * angleRange + (1 - angleRange) / 2) * diffA
    let amount = scale * (radiusMin + (radiusMax - radiusMin) * Math.random())
    newPolygon.push([b[0] + Math.sin(angle) * amount, b[1] + Math.cos(angle) * amount])
  }
  newPolygon = newPolygon.filter((x, i, a) => v.mag(v.sub(x, a[(i + 1) % a.length])) > 4)
  if(newPolygon.length < 3) return null
  if(reverse) newPolygon.reverse()
  return newPolygon
}
