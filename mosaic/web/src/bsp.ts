
import { Point, Edge, getSide, allPolyEdges, intersectLineSegments, v, reconstructPolygon } from "./geo"

export type Bsp = {
  line: Edge,
  segments: Edge[],
  left: Bsp,
  right: Bsp,
} | null

export function* bspEdges(bsp: Bsp): IterableIterator<Edge>{
  if(!bsp) return
  yield* bsp.segments
  yield* bspEdges(bsp.left)
  yield* bspEdges(bsp.right)
}

export function* intersect(aBsp: Bsp,  bBsp: Bsp){
  yield* clipEdges(bBsp, [...bspEdges(aBsp)], true, false)
  yield* clipEdges(aBsp, [...bspEdges(bBsp)], true, true)
}

export function* diff(aBsp: Bsp, bBsp: Bsp){
  bBsp = invert(bBsp)
  yield* clipEdges(bBsp, [...bspEdges(aBsp)], true, false)
  yield* clipEdges(aBsp, [...bspEdges(bBsp)], true, true)
}

export function invert(bsp: Bsp):Bsp{
  if(bsp === null) return null
  return {
    line: bsp.line.slice().reverse() as never,
    segments: bsp.segments.map(x => x.slice().reverse()) as never,
    left: invert(bsp.right),
    right: invert(bsp.left),
  }
}

export function polygonToBsp(polygon: Point[][]){
  return addEdgesToBsp(null, allPolyEdges(polygon))
}

export function addEdgesToBsp(bsp: Bsp, edges: Edge[]): Bsp{
  if(!edges.length) return bsp
  if(bsp === null) bsp = { line: edges[0], segments: [edges.shift()!], left: null, right: null }
  let leftEdges: Edge[] = []
  let rightEdges: Edge[] = []
  splitEdges(bsp.line, edges, {
    left: leftEdges,
    right: rightEdges,
    co: bsp.segments,
    opp: [],
  })
  bsp.left = addEdgesToBsp(bsp.left, leftEdges)
  bsp.right = addEdgesToBsp(bsp.right, rightEdges)
  return bsp
}

export function splitEdges(line: [Point, Point], edges: [Point, Point][], groups: Record<"left" | "right" | "co" | "opp", Edge[]>){
  for(const edge of edges) {
    let pSide = getSide(line, edge[0])
    let qSide = getSide(line, edge[1])
    if((pSide || qSide) === (qSide || pSide)) {
      let side = pSide || qSide
      if(side === 0)
        if(v.dot(v.sub(...edge), v.sub(...line)) > 0)
          groups.co.push(edge)
        else
          groups.opp.push(edge)
      else if(side === -1)
        groups.left.push(edge)
      else if(side === 1)
        groups.right.push(edge)
      else throw new Error("invalid side " + side)
    }
    else {
      let [, middle] = intersectLineSegments(line, edge)
      let pEdge: Edge = [edge[0], middle]
      let qEdge: Edge = [middle, edge[1]]
      for(let [side, edge] of [[pSide, pEdge], [qSide, qEdge]] as const)
        if(side === -1)
          groups.left.push(edge)
        else if(side === 1)
          groups.right.push(edge)
        else throw new Error("invalid side " + [pSide, qSide, side, edge])
    }
  }
}

export function* clipEdges(bsp: Bsp, edges: [Point, Point][], inside: boolean, cplnb: boolean): IterableIterator<[Point, Point]>{
  if(bsp === null) return yield* edges
  let leftEdges: Edge[] = []
  let rightEdges: Edge[] = []
  splitEdges(bsp.line, edges, {
    left: leftEdges,
    right: rightEdges,
    co: !inside ? leftEdges : rightEdges,
    opp: cplnb === inside ? leftEdges : rightEdges,
  })
  if(bsp.left)
    yield* clipEdges(bsp.left, leftEdges, inside, cplnb)
  else if(inside)
    yield* leftEdges
  if(bsp.right)
    yield* clipEdges(bsp.right, rightEdges, inside, cplnb)
  else if(!inside)
    yield* rightEdges
}

export function pointInsideBsp(bsp: Bsp, point: Point): boolean | null{
  if(bsp === null) return null
  let side = getSide(bsp.line, point)
  if(side === 0) return null
  let sub = side === 1 ? bsp.right : bsp.left
  if(sub === null) return side === -1
  return pointInsideBsp(sub, point)
}

export function* bspToConvexPolygons(bsp: Bsp, edges: Edge[]): IterableIterator<Point[]>{
  if(bsp == null)
    return yield reconstructPolygon(edges)[0]
  let leftEdges: Edge[] = []
  let rightEdges: Edge[] = []
  let both: Edge[] = [[v.sub(bsp.line[0], v.rsz(v.sub(...bsp.line), 10000)), v.add(bsp.line[0], v.rsz(v.sub(...bsp.line), 10000))]]
  splitEdges(bsp.line, edges, {
    left: leftEdges,
    right: rightEdges,
    co: [],
    opp: [],
  })
  leftEdges = trimConvex([...leftEdges, ...both])
  rightEdges = trimConvex([...rightEdges, ...both.map(x => x.slice().reverse()) as Edge[]])
  yield* bspToConvexPolygons(bsp.left, leftEdges)
  if(bsp.right)
    yield* bspToConvexPolygons(bsp.right, rightEdges)
}

export function trimConvex(edges: Edge[]){
  for(let i = 0; i < edges.length; i++) {
    let newEdges: Edge[] = []
    splitEdges(edges[i], edges, {
      left: [],
      right: newEdges,
      co: newEdges,
      opp: [],
    })
    edges = newEdges
  }
  return edges
}
