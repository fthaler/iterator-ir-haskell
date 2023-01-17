{-# LANGUAGE TypeFamilies #-}

module ToyConnectivity (
    VertexIndex (Vertex),
    EdgeIndex (Edge),
    CellIndex (Cell),
    localize,
    index,
    vIdx,
    eIdx,
    cIdx,
    c2e,
    v2v,
    e2v,
    v2e
) where

import IteratorIr

data VertexIndex = Vertex { vIdx :: Int }
data EdgeIndex = Edge { eIdx :: Int }
data CellIndex = Cell { cIdx :: Int }

c2e_list = [
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

data UnstructuredConnectivity a b = UC Int [[Int]]

class Location l where
    localize :: Int -> l
    index :: l -> Int

instance Location VertexIndex where
    localize = Vertex
    index = vIdx

instance Location EdgeIndex where
    localize = Edge
    index = eIdx

instance Location CellIndex where
    localize = Cell
    index = cIdx

instance (Location a, Location b) => Connectivity (UnstructuredConnectivity a b) where
    type Source (UnstructuredConnectivity a b) = a
    type Destination (UnstructuredConnectivity a b) = b
    neighbor (UC _ nbList) nb i = localize $ nbList!!(index i)!!nb
    maxNeighbors (UC maxNbs _ ) = maxNbs

c2e :: UnstructuredConnectivity CellIndex EdgeIndex
c2e = UC 4 c2e_list

v2v_list = [
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

v2v :: UnstructuredConnectivity VertexIndex VertexIndex
v2v = UC 4 v2v_list

e2v_list = [
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

e2v :: UnstructuredConnectivity EdgeIndex VertexIndex
e2v = UC 2 e2v_list

v2e_list = [
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

v2e :: UnstructuredConnectivity VertexIndex EdgeIndex
v2e = UC 4 v2e_list
