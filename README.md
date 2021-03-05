# SymbolicTracing.jl

Experimental tracing for SymbolicUtils.jl types. SymbolicTracing.jl will allow you to effectively treat `Symbolic{T}` as being a subtype of `T` for the purposes of dispatch **if** `T` is an abstract type.

```julia
julia> using SymbolicTracing, Symbolics

julia> @syms x::Real;

julia> f(x::Number, y) = sin(x + 1) + (sin(3y) - 1);

julia> @trace f(x, 3.0)
sin(x + 1) - 0.5878815147582435

julia> typeof(ans)
SymbolicUtils.Term{Real, Nothing}
```

This works with *any* abstract type
```julia
julia> @syms s::AbstractString;

julia> g(s::AbstractString) = s * " hi"
g (generic function with 1 method)

julia> @trace g(s)
s* hi

julia> dump(ans) # the lack of quotation marks on " hi" is just a printing quirk from SymbolicUtils
SymbolicUtils.Term{Any, Nothing}
  f: * (function of type typeof(*))
  arguments: Array{Any}((2,))
    1: SymbolicUtils.Sym{AbstractString, Nothing}
      name: Symbol s
      metadata: Nothing nothing
    2: String " hi"
  metadata: Nothing nothing
  hash: Base.RefValue{UInt64}
    x: UInt64 0x0000000000000000
```

To register a certain function as being a natural stopping point for tracing (i.e. don't recurse inside it), use `@register`
```julia
julia> h(X::AbstractMatrix) = X .+ 1

h (generic function with 3 methods)

julia> @register h(X)

julia> @syms X::AbstractMatrix
(X,)

julia> @trace 2X + h(X)
h(X) + 2X
````
