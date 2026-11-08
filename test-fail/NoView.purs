-- expect: Every endpoint must end in a view V
module Test.Fail.NoView where

import Windrose.Router

type Api = P "a" :> P "b"

api = mkRoutable (RouteProxy :: RouteProxy Api)
