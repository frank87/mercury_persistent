:- module serialize.

:- interface.

:- import_module univ.
:- import_module construct.
:- import_module list.
:- import_module gdbm.

%-------------------------------------------------------------------------------
%  Using mercury construct library to generalize record
%-------------------------------------------------------------------------------
:- type general_record ---> general_record( functor_number_lex, list(univ) ).

:- func construct_record( general_record ) = X is semidet.

:- func deconstruct_record( X ) = general_record is semidet.

:- type record( T ) ---> record(T).

:- instance data(record(T)).

:- implementation.

:- import_module deconstruct.
:- import_module int.
:- import_module string.
:- import_module type_desc.
:- import_module exception.

% packing and unpacking records.
construct_record( general_record( I, Args ) ) = X :-
	univ( X ) = construct( type_of(X), I, Args ).

deconstruct_record( X ) = general_record( Functor, Args ) :-
	deconstruct_du( X, do_not_allow, Functor, _, Args ).

:- pred add_args( list(univ), int, int, gdbmDatum, gdbmDatum ).
:- mode add_args( in, in, out, gdi, guo ) is det.
add_args( [], !Cursor, !Target ).
add_args( [U|Rest], !Cursor, !Target ) :-
		(
		if U = univ(X:int) then
			data_set( X, !Cursor, !Target )
		else (
			if U = univ(X:string) then
				data_set( X, !Cursor, !Target )
			else
				throw( U )
			)
		),
		add_args( Rest, !Cursor, !Target ).

:- pred data_get_args( gdbmDatum::gui, int::in, int::out, list( pseudo_type_desc )::in, list( univ )::out ) is semidet.

data_get_args( _, !Cursor, [], [] ).

data_get_args( Src, !Cursor, [Next|Rest], Target ) :-
	Type= det_ground_pseudo_type_desc_to_type_desc(Next),
	(
		if type_name( Type ) = "string" then 
			data_get( Src, !Cursor, S:string ),
			data_get_args( Src, !Cursor, Rest, Tail ),
			Target = [ univ(S:string) | Tail ]
		else
			type_name( Type ) = "int",
			data_get( Src, !Cursor, I:int ),
			data_get_args( Src, !Cursor, Rest, Tail ),
			Target = [ univ(I:int)| Tail ]
	).

:- instance data( record(T) ) where [
	(
	data_set( record(Src), !Cursor, !Target ) :-
		if general_record( I, Args ) = deconstruct_record( Src ) then
			data_set( I, !Cursor, !Target ),
			add_args( Args, !Cursor, !Target )
		else
			throw("Deconstruction record failed")
	),
	(
	data_get( Src, !Cursor, record(Target) ) :-
		data_get(Src, !Cursor, I:int ),
		get_functor(type_of(Target), I, _, _, Types ),
		data_get_args(Src, !Cursor, Types, ArgList ),
		Target = construct_record( general_record( I, ArgList ) )
	)
].
