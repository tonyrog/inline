%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Module/Function inlining + some cool features
%%% @end
%%% Created :  6 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(inline).

-export([load_code/1]).
-export([print_code/1, print_forms/1]).
-export([localize_code/1, localize_code/2]).
-export([localize_functions/2,localize_functions/3]).
-export([localize_calls/2]).

-export([format_error/1]).
-export([parse_transform/2]).

-record(mstate,
	{
	  mod,
	  functions = [], %% [{f,a}]
	  imports   = [], %% [{m,f,a}]
	  exports   = []  %% [{f,a}]
	}).

parse_transform(Forms, _Options) ->
    case extract_inline(Forms,[],[]) of
	{Forms1,[]} -> Forms1;
	{Forms1,Inline} ->
	    {Forms2,MXs} = inline_code(Inline,[],[]),
	    lists:map(
	      fun({function,Ln,Name,Arity,Body}) ->
		      {function,Ln,Name,Arity,
		       localize_calls(MXs,Body)};
		 (F) -> F
	      end, Forms1) ++ Forms2
    end.


format_error(Mesg) ->
    Mesg.

inline_code([Mod|Ms],FLs,MXs) ->
    Pfx = atom_to_list(Mod)++"_",
    case localize_code(Mod,Pfx) of
	{ok,Forms} ->
	    inline_code(Ms, [Forms|FLs], [{Mod,Pfx}|MXs]);
	{error,enoent} ->
	    Warn = [{warning,{?MODULE,["module ",atom_to_list(Mod),
				       " is not found"]}}],
	    inline_code(Ms, [Warn|FLs], MXs);
	{error,no_debug_info} ->
	    Warn = [{warning,{?MODULE,
			      ["module ",atom_to_list(Mod),
			       " was not compile with debug_info enabled"]}}],
	    inline_code(Ms, [Warn|FLs], MXs);
	{error,_Error} ->
	    Warn = [{warning,{?MODULE,["unable to inline ",
				       atom_to_list(Mod)]}}],
	    inline_code(Ms, [Warn|FLs], MXs)
    end;
inline_code([],FLs,MXs) ->
    {lists:append(FLs), MXs}.


extract_inline([{attribute,_Ln,inline_module,M}|Fs],Forms,Inline) ->
    if is_atom(M) ->
	    extract_inline(Fs,Forms,[M|Inline]);
       is_list(M) ->
	    extract_inline(Fs,Forms,[Mi || Mi <- M, is_atom(Mi)]++Inline);
       true ->
	    extract_inline(Fs,Forms,Inline)
    end;
extract_inline([F|Fs],Forms,Inline) ->
    extract_inline(Fs,[F|Forms],Inline);
extract_inline([],Forms,Inline) ->
    {lists:reverse(Forms),lists:usort(Inline)}.


load_code(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
	non_existing ->
	    {error,enoent};
	Path ->
	    case beam_lib:chunks(Path, [abstract_code]) of
		{ok,{Mod,[{abstract_code,{raw_abstract_v1, Forms}}]}} ->
		    {ok,Forms};
		{ok,{Mod,[{abstract_code,no_abstract_code}]}} ->
		    {error,no_debug_info};
		{ok,_} ->
		    {error,unrecognised_forms};
		Error ->
		    Error
	    end
    end.

localize_code(Mod) ->
    localize_code(Mod, atom_to_list(Mod)++"_").

localize_code(Mod, Pfx) ->
    case load_code(Mod) of
	{ok,Forms} ->
	    Forms1 = localize_functions(Mod,Pfx,Forms),
	    %% print_forms(Forms1),
	    {ok,Forms1};
	Error ->
	    Error
    end.

print_code(Mod) ->
    case load_code(Mod) of
	{ok,Forms} ->
	    print_forms(Forms);
	Error ->
	    Error
    end.

print_forms(Forms) ->
    lists:foreach(
      fun(F) ->
	      io:format("~s\n", [erl_pp:form(F)])
      end, Forms).

%%
%% localize function names and function calls in a module to be 
%% 
localize_functions(Mod, Forms) ->
    localize_functions(Mod, lists:concat([Mod,"_"]), Forms).

localize_functions(Mod,Pfx,Forms) ->
    localize_functions(Mod,Pfx,Forms,[],#mstate{}).

localize_functions(Mod,Pfx,[F|Fs],Acc,St) ->
    case F of
	{attribute,_Ln,module,Mod} ->  %% no module anymore
	    localize_functions(Mod,Pfx,Fs,Acc,St#mstate { mod=Mod} );

	{attribute,_Ln,export,Exports} -> %% no function are exported
	    St1 = St#mstate { exports = St#mstate.exports++Exports },
	    localize_functions(Mod,Pfx,Fs,Acc,St1);

	{attribute,_Ln,import,{M,FAs}} -> %% must expand imports here
	    MFAs = [{{Fn,A},M} || {Fn,A} <- FAs],
	    St1 = St#mstate { imports = St#mstate.imports++MFAs },
	    localize_functions(Mod,Pfx,Fs,Acc,St1);

	{attribute,_Ln,export_type,_Exports} -> %% no types are exported
	    localize_functions(Mod,Pfx,Fs,Acc,St);

	{attribute,_Ln,spec,_Spec} -> %% specs are not needed !?
	    localize_functions(Mod,Pfx,Fs,Acc,St);

	{attribute,_Ln,type,_Spec} -> %% types are not needed !?
	    localize_functions(Mod,Pfx,Fs,Acc,St);

	{function,Ln,Name,Arity,Body} ->
	    NewName = concat_atom([Pfx,Name]),
	    Body1 = localize_body(Mod,Pfx,Body,St),
	    F1={function,Ln,NewName,Arity,Body1},
	    St1 = St#mstate { functions=[{NewName,Arity}|St#mstate.functions]},
	    localize_functions(Mod,Pfx,Fs,[F1|Acc],St1);
	F ->
	    localize_functions(Mod,Pfx,Fs,[F|Acc],St)
    end;
localize_functions(_Mod,_Pfx,[],Acc,St) ->
    %% declare all functions as potentially unused
    [{attribute,1,compile,[{nowarn_unused_function,St#mstate.functions}]} | 
     lists:reverse(Acc)].


%% localize the body of an inlined module function
localize_body(Mod,Pfx,Body,St) ->
    transform(
      fun(expr,A={call,Ln1,{atom,Ln2,F},As}) ->
	      Arity = length(As),
	      case lists:keyfind({F,Arity},1,St#mstate.imports) of
		  false ->
		      case erl_internal:bif(F,Arity) of
			  true -> A;
			  false -> 
			      {call,Ln1,{atom,Ln2,concat_atom([Pfx,F])},As}
		      end;
		  {_,IM} ->
		      {call,Ln1,{remote,Ln2,{atom,Ln2,IM},{atom,Ln2,F}},As}
	      end;
	 (expr,A={call,Ln1,{remote,_Ln2,{atom,_Ln3,M},{atom,Ln4,F}},As}) ->
	      %% We could try to translate this call into a remote call
	      %% in the new module ? but we postpone that for a while
	      if M =:= Mod ->
		      io:format("Warning:~w: ~w:~w transform into local call\n",
				[Ln1,M,F]),
		      {call,Ln1,{atom,Ln4,concat_atom([Pfx,F])},As};
		 true -> A
	      end;
	 (expr,A={function,{atom,Ln1,M},{atom,_Ln2,F},{integer,_Ln3,Arity}})->
	      %% remote call to inline moduled? we may allow this if we export
	      %% the function from the module that inline this function and
	      %% the function was exported in the first place?
	      %% Right now I warn and translate to a local call!
	      if M =:= Mod ->
		      io:format("Warning:~w: ~w:~w transform into local call\n",
				[Ln1,M,F]),
		      {function,concat_atom([Pfx,F]),Arity};
		 true ->
		      A
	      end;
	 (expr,A={function,F,Arity}) ->
	      case lists:keyfind({F,Arity},1,St#mstate.imports) of
		  false ->
		      case erl_internal:bif(F,Arity) of
			  true -> A;
			  false -> {function,concat_atom([Pfx,F]),Arity}
		      end;
		  {_,IM} ->
		      Ln1 = 1,
		      {function,{atom,Ln1,IM},{atom,Ln1,F},{integer,Ln1,Arity}}
	      end;
	 (_W,Form) -> Form
      end, Body).

%% localize calls from the module to the inlined modules
%% Ms=[{Mod,Pfx}]
localize_calls(MXs, Body) ->
    %% FIXME: handle imports!!!
    transform(
      fun(expr,A={call,Ln1,{remote,_Ln2,{atom,_Ln3,Mod},{atom,Ln4,Fun}},As}) ->
	      case lists:keyfind(Mod,1,MXs) of
		  false -> A;
		  {_,Pfx} ->
		      {call,Ln1,{atom,Ln4, concat_atom([Pfx,Fun])},As}
	      end;
	 (expr,A={function,{atom,_Ln1,M},{atom,_Ln2,F},{integer,_Ln3,Arity}}) ->
	      case lists:keyfind(M,1,MXs) of
		  false -> A;
		  {_,Pfx} ->
		      {function,concat_atom([Pfx,F]),Arity}
	      end;
	 (_W,Form) -> Form
      end, Body).
    

transform(Fun,Form) ->
    tf_(Fun,form,Form).

tf_(Fun,W,Form) ->
    Form1 =
	case Form of
	    {T,Ln} -> 
		{T,Ln};
	    {'fun',Ln,{clauses,Cs}} ->
		{'fun',Ln,{clauses,tf_(Fun,clauses,Cs)}};
	    {T,Ln,A1} ->
		{T,Ln,tf_(Fun,W,A1)};
	    {T,Ln,A1,A2} ->
		{T,Ln,tf_(Fun,W,A1),tf_(Fun,W,A2)};
	    {clause,Ln,A1,A2,A3} ->
		{clause,Ln,
		 tf_(Fun,head,A1),tf_(Fun,guard,A2),tf_(Fun,expr,A3)};
	    {T,Ln,A1,A2,A3} ->
		{T,Ln,tf_(Fun,W,A1),tf_(Fun,W,A2),tf_(Fun,W,A3)};
	    {'try',Ln,A1,A2,A3,A4} ->
		{'try',Ln,tf_(Fun,expr,A1),tf_(Fun,clauses,A2),
		 tf_(Fun,clauses,A3),tf_(Fun,expr,A4)};
	    [H|T] ->
		[tf_(Fun,W,H) | tf_(Fun,W,T)];
	    [] ->
		[];
	    _ when is_number(Form) -> Form;
	    _ when is_atom(Form) -> Form
	end,
    Fun(W,Form1).

concat_atom(List) ->
    list_to_atom(lists:concat(List)).
