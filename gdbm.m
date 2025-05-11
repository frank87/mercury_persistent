:- module gdbm.
:- interface.
:- import_module string.
:- import_module int.

:- type gdbmTable.

%-------------------------------------------------------------------------------
% Opening and closing
%   open( FileDescriptor, Filename ).
%   close( FileDescriptor ).
%-------------------------------------------------------------------------------
:- pred open( gdbmTable, string ).
:- mode open( uo, in ) is semidet.

:- pred close( gdbmTable ).
:- mode close( di ) is det.

%-------------------------------------------------------------------------------
%  fetching records doesn't change "world state"
%    fetch( FileDescriptor, SearchKey, Result ).
%-------------------------------------------------------------------------------
:- pred fetch( gdbmTable, gdbmDatum, gdbmDatum ).
:- mode fetch( ui, in, out ) is semidet.

%-------------------------------------------------------------------------------
%  updating records (Changes world state: use file descriptor like !IO
%    store( !FileDescriptor, SearchKey, Result ).
%-------------------------------------------------------------------------------
:- pred store( gdbmTable, gdbmTable, gdbmDatum, gdbmDatum ).
:- mode store( di, uo, in, in ) is det.


% Type holds internal link to the stored data, making it an unique type.
:- type gdbmDatum.
:- inst allocated == ground.
:- mode guo == out(allocated).
:- mode gui == in(allocated).
:- mode gdi == allocated >> dead.
%-------------------------------------------------------------------------------
%  handling gdbm-datum type used in gdbm-calls.
%-------------------------------------------------------------------------------

% allocate using malloc
:- pred gdbmDatum_new( gdbmDatum::guo ) is det.

% free malloc...
:- pred gdbmDatum_free( gdbmDatum::gdi ) is det.

%-------------------------------------------------------------------------------
%  Pack and unpack Mercury-data...
%-------------------------------------------------------------------------------
:- typeclass data(T) where [
	% data_set( Src, Start, Next, !Target )
	pred data_set( T::in, int::in, int::out, gdbmDatum::gdi, gdbmDatum::guo ) is det,
	% data_get( Src, Start, Next, Target )
	pred data_get( gdbmDatum::gui, int::in, int::out, T::out ) is semidet
].

:- instance data(int).
:- instance data(string).

:- implementation.

:- pragma foreign_decl("C", "#include \"gdbm.h\"").

:- pragma foreign_type( "C", gdbmTable, "GDBM_FILE " ).
:- pragma foreign_type( "C", gdbmDatum, "datum" ).

:- pred toMercury( string::in, int::in, string::out ) is det.
:- pragma foreign_export("C", toMercury( in, in, out ), "to_mercury" ).
toMercury( Src, Length, Result ) :-
	left(Src, Length - 1, Result). % gdbm-text includes \0 

:- pragma foreign_proc("C",
    open( GDBM::uo, Str::in ),
    [will_not_call_mercury, promise_pure],
"
     GDBM = gdbm_open(Str, 0, GDBM_WRCREAT|GDBM_XVERIFY, 511, NULL );
     if ( GDBM == NULL ) {
		     printf( ""%s\\n"", gdbm_strerror( gdbm_errno ) );
		     };
     SUCCESS_INDICATOR = (GDBM != NULL);
").

:- pragma foreign_proc("C",
    fetch( GDBM::ui, Key::in, Data::out ),
    [promise_pure],
    "
    Data = gdbm_fetch( GDBM, Key );
    SUCCESS_INDICATOR = (Data.dptr != NULL);
    "
).

:- pred store( gdbmTable::di, gdbmTable::uo, gdbmDatum::in, gdbmDatum::in, int::out, string::out ) is det.
:- pragma foreign_proc("C",
    store( GDBM1::di, GDBM2::uo, Key::in, Data::in, Result::out, ErrMsg::out ),
    [promise_pure],
"
    int result;
    char * msg;

    GDBM2 = GDBM1;

    printf( ""[%s][%s] \\n"", Key.dptr, Data.dptr );
    Result = gdbm_store( GDBM1, Key, Data, GDBM_REPLACE );

    msg = (char *) gdbm_strerror( gdbm_errno );
    to_mercury( msg, 80, &ErrMsg );

").

:- import_module exception.
store( FDin, FDout, Key, Data ) :-
	store( FDin, FDout, Key, Data, Result, ErrMsg ),
	(
	if Result = 0 then
		true
	else
		throw( ErrMsg )
	).

:- pragma foreign_proc("C",
    close( GDBM::di ),
    [will_not_call_mercury, promise_pure],
"
     gdbm_close(GDBM);
").

%-------------------------------------------------------------------------------
%	building datum
%-------------------------------------------------------------------------------

:- pragma foreign_proc("C",
    gdbmDatum_new( Data::guo ),
    [will_not_call_mercury, promise_pure ],
    "
    Data.dsize = 0;
    Data.dptr = NULL;
    ").

:- pragma foreign_proc("C",
    gdbmDatum_free( Rec::gdi ),
    [will_not_call_mercury, promise_pure],
    "
    Rec.dsize = 0;
    if ( Rec.dptr ) free( Rec.dptr );
    ").

%-------------------------------------------------------------------------------
% type conversion
%-------------------------------------------------------------------------------

% int
:- pred int_data_set( int::in, int::in, int::out, gdbmDatum::gdi, gdbmDatum::guo ) is det.
:- pragma foreign_proc("C",
   int_data_set( Src::in, Start::in, Next::out, Dst0::gdi, Dst::guo ),
   [will_not_call_mercury, promise_pure],
   "
	char * cursor;
	Next = Start + sizeof(int);
	Dst = Dst0;
	if ( Dst.dsize <= Next ){
		Dst.dptr = realloc( Dst.dptr, Next );
		Dst.dsize = Next;
	}
	
	cursor = Dst.dptr + Start;
	*((int *)cursor) = Src;
   " ).

:- pred int_data_get( gdbmDatum::gui, int::in, int::out, int::out ) is semidet.
:- pragma foreign_proc("C",
   int_data_get( Src::gui, Start::in, Next::out, Dst::out ),
   [will_not_call_mercury, promise_pure],
   "
   	char * cursor;
	Next = Start + sizeof(int);
        SUCCESS_INDICATOR = ( Src.dsize >= Next );
        if ( SUCCESS_INDICATOR ) {
		cursor = Src.dptr + Start;
		Dst = *((int *)cursor);
	};
   " ).

:- instance data(int) where [
	pred(data_set/5) is int_data_set,
	pred(data_get/4) is int_data_get
].

% string

:- pred string_data_set( string::in, int::in, int::out, gdbmDatum::gdi, gdbmDatum::guo ) is det.
:- pragma foreign_proc("C",
   string_data_set( Src::in, Start::in, Next::out, Dst0::gdi, Dst::guo ),
   [will_not_call_mercury, promise_pure],
   "
	char * cursor;
	Next = Start + strlen(Src) + 1;
	Dst = Dst0;
	if ( Dst.dsize <= Next ){
		Dst.dptr = realloc( Dst.dptr, Next );
		Dst.dsize = Next;
	}
	
	cursor = Dst.dptr + Start;
	strcpy( cursor, Src );
   " ).

:- pred string_data_get( gdbmDatum::gui, int::in, int::out, string::out ) is semidet.
:- pragma foreign_proc("C",
   string_data_get( Src::gui, Start::in, Next::out, Dst::out ),
   [will_not_call_mercury, promise_pure],
   "
   	char * cursor;
        SUCCESS_INDICATOR = ( Src.dsize > Start );
        if ( SUCCESS_INDICATOR ) {
		cursor = Src.dptr + Start;
		to_mercury( cursor, Src.dsize - Start, &Dst );
		Next = Start + strlen(cursor) + 1; /* zero terminatir */
	};
   " ).

:- instance data(string) where [
	pred(data_set/5) is string_data_set,
	pred(data_get/4) is string_data_get
].

% string

