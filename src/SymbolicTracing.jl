module SymbolicTracing 

using SymbolicUtils, IRTools
using SymbolicUtils: Symbolic, Term, Sym
using IRTools: @dynamo, argument!, IR, isexpr

Base.@pure sym_substitute(::Type{TSym}) where {T, TSym <: Symbolic{T}} = getfield(T, :abstract) ? T : TSym 
sym_substitute(::Type{T}) where {T} = T

@dynamo function sneakyinvoke(f, ::Type{T}, args...) where T<:Tuple
    ir = IR(f, T.parameters...)
    if ir === nothing
        sigs = " ::" * join(string.(args), " ::")
        error("Could not trace into $f($sigs). The method may not exist.\nRemember that trace will not recurse for Symbolic{T} for concretely typed T.")
    end
    argument!(ir, at = 2)
    return ir
end

@dynamo function trace(args...)
    ir = IR(args...)
    ir == nothing && return
    for (x, st) in ir
	isexpr(st.expr, :call) || continue
	ir[x] = Expr(:call, _trace, st.expr.args...)
    end
    return ir
end

_trace(f::Core.IntrinsicFunction, args...) = f(args...)
_trace(f) = trace(f)

@generated function _trace(f::F, args...) where {F}
    args′ = sym_substitute.(args)
    ex = if any(args .<: Symbolic)
	if isregistered(F)
	    :($Term(f, [args...]))
	elseif fieldcount(F) == 0
            fi = F.instance
            if hasmethod(fi, Tuple{args...})
                :(f(args...))
            elseif hasmethod(fi, Tuple{args′...})
	        :($trace($sneakyinvoke, f, Tuple{$(args′...)}, args...))
            else
                sigs = " ::" * join(string.(args), ", ::")
                error("Could not trace into $fi($sigs). The method may not exist.\nRemember that trace will not recurse for Symbolic{T} for concretely typed T.")
            end
        else
            :($trace($sneakyinvoke, f, $args′, args...))
	end
    else
	:(f(args...))
    end
    
    ci = expr_to_codeinfo(SymbolicTracing, [Symbol("#self#"), :f, :args], [:F], (F,), ex)
    ci.edges = []
    
    if fieldcount(F) == 0
        let T = Tuple{args...}, ftype = Tuple{f, args...}, ftype′ = Tuple{f, args′...} 
            method_insts = Core.Compiler.method_instances(f.instance, T, typemax(UInt))
            covering_method_insts = [mi for mi in method_insts if ftype <: mi.def.sig || ftype′ <: mi.def.sig]
            method_doesnot_exist = isempty(covering_method_insts)
            # Now we add the edges so if a method is defined this recompiles
            if method_doesnot_exist
                # No method so attach to method table
                mt = f.name.mt
                typ = Base.rewrap_unionall(Tuple{f, Base.unwrap_unionall(T).parameters...}, T)
                push!(ci.edges, Core.Compiler.vect(mt, typ)...)
            else  # method exists, attach edges to all instances
                push!(ci.edges, covering_method_insts...)
            end
        end
    end
    push!(ci.edges, Core.Compiler.method_instances(isregistered, Type{Type{F}})...)
    return ci
end

"""
    expr_to_codeinfo(m::Module, argnames, spnames, sp, e::Expr)

Take an expr (usually a generated function generator) and convert it into a CodeInfo object 
(Julia's internal, linear representation of code). 

`m` is the module that the CodeInfo should be generated from (used for name resolution)

`argnames` must be an iterable container of symbols describing the CodeInfo's input arguments. 
NOTE: the first argument should be given as `Symbol("#self#")`. So if the function is `f(x) = x + 1`,
then `argnames = [Symbol("#self#"), :x]`

`spnames` should be an iterable container of the names of the static parameters to the CodeInfo body
(e.g.) in `f(x::T) where {T <: Int} = ...`, `T` is a static parameter, so `spnames` should be `[:T]` 

`sp` should be an iterable container of the static parameters to the CodeInfo body themselves (as 
opposed to their names) (e.g.) in `f(x::T) where {T <: Int} = ...`, `T` is a static parameter, 
so `sp` should be `[T]` 

    `e` is the actual expression to lower to CodeInfo. This must be 'pure' in the same sense as generated
    function bodies.
    """
function expr_to_codeinfo(m::Module, argnames, spnames, sp, e::Expr)
    lam = Expr(:lambda, argnames,
               Expr(Symbol("scope-block"),
                    Expr(:block,
                         Expr(:return,
                              Expr(:block,
                                   e,
                                   )))))
    ex = if spnames === nothing || isempty(spnames)
        lam
    else
        Expr(Symbol("with-static-parameters"), lam, spnames...)
    end

    # Get the code-info for the generatorbody in order to use it for generating a dummy
    # code info object.
    ci = ccall(:jl_expand_and_resolve, Any, (Any, Any, Core.SimpleVector), ex, m, Core.svec(sp...))
    @assert ci isa Core.CodeInfo "Failed to create a CodeInfo from the given expression. This might mean it contains a closure or comprehension?\n Offending expression: $e"
    ci
end

macro trace(fcall)
    :($trace(() -> $fcall)) |> esc
end


macro register(fs...)
    ex = Expr(:block, __source__)
    for f in fs
        push!(ex.args, :($SymbolicTracing.isregistered(::Type{typeof($f)}) = true))
    end
    esc(ex)
end

# If isregistered(typeof(f)) == true, then inside a pass, we won't recurse into the insides of f. 
# Registered functions are stopping points for us

isregistered(::Type{T}) where {T} = false
@register((+), (-), (*), (/), (^), exp, log,
          sin, cos, tan, asin, acos, atan,
          sinh, cosh, tanh, asinh, acosh, atanh, adjoint)

end # module
