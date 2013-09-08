# Module inline parse transform

This parse transform is used to inline complete modules in other modules. 
To use it, then add the following compile directive in the moduled compiled

    -compile({parse_transform,inline}).

The inline parse transform looks for 'inline_module' directive to know what
modules to include like

    -inline_module([a,b,c]).
    -inline_module(d).

The modules included must priorly have been compiled to beam files and also be found in the code path. The inlined modules MUST also have been compiled with +debug_info enabled, this is how the inliner finds the code to inline.
If any of the above condition is not met, an error is generated.

I always declare 

    export ERL_COMPILER_OPTIONS=debug_info

in a .basrh or similar to always have the debugging info available at all times.

An alternative to give the parse_transform directive in the source file is to write it on the erlc command line

    erlc +'{parse_transform,inline}' foo.erl

The following module show a simple example on how to use module inline and
also an interesting property of inline in general.

```erlang
-module(example).

-export([run/0]).

-compile(inline).
-compile({inline_size,100}).

-inline_module(vec3f).

run() ->
    A = vec3f:new(1,2,3),
    B = vec3f:new(4,5,6),
    C = vec3f:new(7,8,9),
    vec3f:multiply(A,vec3f:add(B,C)).
```

The code above creates three 3D vectors A, B and C. Normally the above code would call vec3f:new three times then call vec3f:add and lastly call vec3f:multiply, but when compiled with the compile option

    -compile({inline_size,100}).

the code is simply reduced to the result of the calculation!

Checking the beam assembler output by setting the -S option to erlc we
can verify this

```erlang
{function, run, 0, 2}.
  {label,1}.
    {line,[{location,"example.erl",14}]}.
    {func_info,{atom,example},{atom,run},0}.
  {label,2}.
    {move,{literal,{11.0,26.0,45.0}},{x,0}}.
    return.
```

In otherwords to get constant propagation and other cool things to
happend in the erlang compiler/optimimizer the inline_size must be 
set quite high, 100 or above.
I have noticed that guards are not always evaluated for constant propagtion
purposes in the compiler leading to less interesting results.

When inlining modules the +native flag may lead to greate speed ups!
