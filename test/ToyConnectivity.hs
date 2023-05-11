module ToyConnectivity (
    VertexIndex (Vertex),
    EdgeIndex (Edge),
    CellIndex (Cell),
    vIdx,
    eIdx,
    cIdx,
    c2e,
    v2v,
    e2v,
    v2e
) where

newtype VertexIndex = Vertex { vIdx :: Int } deriving Show
newtype EdgeIndex = Edge { eIdx :: Int } deriving Show
newtype CellIndex = Cell { cIdx :: Int } deriving Show

c2eList = [
        [0, 10, 3, 9],
        [1, 11, 4, 10],
        [2, 9, 5, 11],
        [3, 13, 6, 12],
        [4, 14, 7, 13],
        [5, 12, 8, 14],
        [6, 16, 0, 15],
        [7, 17, 1, 16],
        [8, 15, 2, 17]
    ]

c2e :: CellIndex -> [EdgeIndex]
c2e (Cell c) = map Edge $ c2eList!!c

v2vList = [
        [1, 3, 2, 6],
        [2, 3, 0, 7],
        [0, 5, 1, 8],
        [4, 6, 5, 0],
        [5, 7, 3, 1],
        [3, 8, 4, 2],
        [7, 0, 8, 3],
        [8, 1, 6, 4],
        [6, 2, 7, 5]
    ]

v2v :: VertexIndex -> [VertexIndex]
v2v (Vertex v) = map Vertex $ v2vList!!v

e2vList = [
        [0, 1],
        [1, 2],
        [2, 0],
        [3, 4],
        [4, 5],
        [5, 3],
        [6, 7],
        [7, 8],
        [8, 6],
        [0, 3],
        [1, 4],
        [2, 5],
        [3, 6],
        [4, 7],
        [5, 8],
        [6, 0],
        [7, 1],
        [8, 2]
    ]

e2v :: EdgeIndex -> [VertexIndex]
e2v (Edge e) = map Vertex $ e2vList!!e

v2eList = [
        [0, 15, 2, 9],
        [1, 16, 0, 10],
        [2, 17, 1, 11],
        [3, 9, 5, 12],
        [4, 10, 3, 13],
        [5, 11, 4, 14],
        [6, 12, 8, 15],
        [7, 13, 6, 16],
        [8, 14, 7, 17]
    ]

v2e :: VertexIndex -> [EdgeIndex]
v2e (Vertex v) = map Edge $ v2eList!!v
