-- expect: Failure of type (a :<|> b) :<|> c
module Test.Fail.BadNestingAlt where

import Windrose.Router

type Api = (V "a" :<|> V "b") :<|> V "c"

api = mkRoutable (RouteProxy :: RouteProxy Api)
