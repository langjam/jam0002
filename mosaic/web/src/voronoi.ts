
import { Delaunay } from "d3-delaunay"
import { Point } from "./geo"

export function makeVoronoi(f: (x: [number, number]) => boolean, size: number, cellSize: number){
  let count = Math.ceil(size / cellSize)
  const points = [...Array(count)].flatMap((_, i) => [...Array(count)].map((_, j) => genPoint(i, j))).filter(f)
  const delaunay = Delaunay.from(points)
  const voronoi = delaunay.voronoi([0, 0, size, size])
  let polys = []
  for(const cell of voronoi.cellPolygons())
    polys.push((cell.slice(1).reverse() as never as Point[]))
  return polys
  function genPoint(cellX: number, cellY: number){
    return [
      (Math.random() + cellX) * cellSize,
      (Math.random() + cellY) * cellSize,
    ] as [number, number]
  }
}
