open Primitives
open Vec3
open Matrix4
open Debug

module FVec3  = Vec3.Make(Float)

module Rbox3d = Rbox.Make(FVec3)
