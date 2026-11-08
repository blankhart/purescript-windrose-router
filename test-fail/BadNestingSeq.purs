-- expect: Failure of type (a :<|> b) :> c
module Test.Fail.BadNestingSeq where

import Windrose.Router

type Api = (P "a" :<|> P "b") :> V "v"

api = mkRoutable (RouteProxy :: RouteProxy Api)
