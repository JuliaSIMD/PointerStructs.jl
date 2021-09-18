module PointerStructs

using Static
using Static: Zero, One
using UnPack: unpack, pack!

export @pointer

include("ptrarrays.jl")
include("structmacro.jl")

end
