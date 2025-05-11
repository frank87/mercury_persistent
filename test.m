:- module test.
:- interface.

:- import_module io.
:- import_module list.
:- import_module string.
:- import_module serialize.

:- pred main(io::di, io::uo) is cc_multi.

:- type myData ---> myData( string );
	xxxx( int, int ).

:- implementation.

:- import_module gdbm.

:- import_module int.

main(!IO) :-
	gdbmDatum_new( Rec1 ), 
	data_set( record(xxxx(1,2)), 0, _, Rec1, Rec2 ), 
	(
	if data_get( Rec2, 0, _, record(String:int)) then
		print_line( String, !IO )
	else
		(
		if data_get(Rec2, 4, _, record( String:myData ) ) then
			print_line( "failed", !IO ),
			print_line( String, !IO )
		else
			print_line( "FAIL", !IO )
		)
	),
	gdbmDatum_free( Rec2 ).

