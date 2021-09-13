module PointerStructs

using Static
using UnPack: unpack, pack!

export @pointer

include("ptrarrays.jl")
include("structmacro.jl")

end
