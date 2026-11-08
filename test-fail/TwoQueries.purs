-- expect: A query Q must come immediately before the view V
module Test.Fail.TwoQueries where

import Windrose.Router

type Api = Q (q :: String) :> Q (r :: String) :> V "v"

api = mkRoutable (RouteProxy :: RouteProxy Api)
