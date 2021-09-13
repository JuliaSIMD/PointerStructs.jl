module PointerStructs

using Static
using UnPack: unpack, repack!

export @pointer

include("ptrarrays.jl")
include("structmacro.jl")

end
