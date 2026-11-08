-- expect: A capture-many M must be the last path piece
module Test.Fail.MNotLast where

import Windrose.Router

type Api = M "m" Int :> P "x" :> V "v"

api = mkRoutable (RouteProxy :: RouteProxy Api)
