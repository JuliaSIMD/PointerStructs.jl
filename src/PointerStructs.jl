module PointerStructs

using Static, UnPack, CloseOpenIntervals
using Static: Zero, One
using UnPack: unpack, pack!

export @pointer, @unpack, @pack!, static

include("ptrarrays.jl")
include("structmacro.jl")

end
