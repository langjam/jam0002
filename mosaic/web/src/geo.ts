
export type Point = readonly [number, number]

export const v = {
  add: (a: Point, b: Point) => [a[0] + b[0], a[1] + b[1]] as const,
  sub: (a: Point, b: Point) => [a[0] - b[0], a[1] - b[1]] as const,
  dot: (a: Point, b: Point) => a[0] * b[0] + a[1] * b[1],
  scl: (a: Point, b: number) => [a[0] * b, a[1] * b] as const,
  rsz: (a: Point, b: number = 1)  => v.scl(a, b / Math.sqrt(v.dot(a, a))),
  mag: (a:Point) => Math.sqrt(v.dot(a, a)),
}

export type Edge = [Point, Point]

export function intersectLineSegments([p, pr]: [Point, Point], [q, qs]: [Point, Point]){
  let cross = (a: Point, b:Point) => a[0] * b[1] - a[1] * b[0]
  let r = v.sub(pr, p)
  let s = v.sub(qs, q)
  let rXs = cross(r, s)
  let t = cross(v.sub(q, p), s) / rXs
  let u = cross(v.sub(q, p), r) / rXs
  return [t >= 0 && t <= 1 && u >= 0 && u <= 1, v.add(p, v.scl(r, t)), t, u]as const
}

export function getSide([p, q]: [Point, Point], r: Point): number{
  let s = v.sub(q, p)
  let t = v.sub(r, p)
  let x = s[0] * t[1] - s[1] * t[0]
  return Math.sign(x)
}

export function allPolyEdges(poly: Point[][]): [Point, Point][]{
  return poly.flatMap(x => x.map((y, i) => [y, x[(i + 1) % x.length]]))
}

export function reconstructPolygon(edges: Edge[]): Point[][]{
  let polys: Point[][] = []
  while(edges.length) {
    polys.push([...edges.pop()!])
    while(edges.length) {
      let i = edges.findIndex(x => v.mag(v.sub(polys[polys.length - 1].slice(-1)[0], x[0])) < 0.0001)
      if(i === -1) break
      polys[polys.length - 1].push(edges.splice(i, 1)[0][1])
    }
  }
  return polys.map(x => x.slice(0, -1))
}
