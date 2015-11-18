-module(tictactoe).
-export([start/0, init/0]).

-include_lib("eunit/include/eunit.hrl").

-define(TICTACTOE_INSTANCE_NAME, tictactoe_server).
-define(TICTACTOE_NODE_NAME, 'server@vagrant-ubuntu-trusty-64').

%% NOTE: You must run the tictactoe server process in a node named as defined by TICTACTOE_NODE_NAME.
start() ->
	Pid = spawn(?MODULE, init, []),
	register(?TICTACTOE_INSTANCE_NAME, Pid). 

init() ->
	io:format("init()~n", []),
	InitialState = create_empty_board(),
	loop(InitialState).

loop(State) ->
	loop(State).

-spec create_empty_board() -> tuple().
create_empty_board() ->
	{empty, empty, empty,
	 empty, empty, empty,
	 empty, empty, empty}.

-spec is_board_filled(tuple()) -> boolean().
is_board_filled(Board) ->
	Marks = tuple_to_list(Board),
	not lists:any(fun (Mark) -> Mark == empty end, Marks).

-spec check_board(tuple()) -> {victory, x} | {victory, o} | draw | undefined.
check_board(Board) ->
	case Board of

		{Mark, Mark, Mark,
		 _, _, _,
		 _, _, _} -> {victory, Mark};

		{_, _, _,
		 Mark, Mark, Mark,
		 _, _, _} -> {victory, Mark};

		{_, _, _,
		 _, _, _,
		 Mark, Mark, Mark} -> {victory, Mark};

		{Mark, _, _,
		 Mark, _, _,
		 Mark, _, _} -> {victory, Mark};

		{_, Mark, _,
		 _, Mark, _,
		 _, Mark, _} -> {victory, Mark};

		{_, _, Mark,
		 _, _, Mark,
		 _, _, Mark} -> {victory, Mark};

		{Mark, _, _,
		 _, Mark, _,
		 _, _, Mark} -> {victory, Mark};

		{_, _, Mark,
		 _, Mark, _,
		 Mark, _, _} -> {victory, Mark};

		_ -> %% It can be a draw or undefined, that's it, the board is not complete.
			case is_board_filled(Board) of
				false -> undefined;
				true -> draw
			end
	end.
	
create_empty_board_test() ->
	?assert(create_empty_board() =:= {empty, empty, empty, empty, empty, empty, empty, empty, empty}).

check_board_for_victory_in_row_test() ->
	Board1 = {x, x, x,
		 o, o, empty,
		 empty, empty, empty},
	?assert(check_board(Board1) =:= {victory, x}),

	Board2 = {o, o, o,
		  empty, x, empty,
		  empty, empty, x},
	?assert(check_board(Board2) =:= {victory, o}).

check_board_for_victory_in_column_test() ->
	Board1 = {x, o, o,
		  x, empty, empty,
		  x, empty, empty},
	?assert(check_board(Board1) =:= {victory, x}).

check_board_for_victory_in_diagonal_test() ->
	Board1 = {o, x, empty,
		  empty, o, x,
		  empty,x, o},
	?assert(check_board(Board1) =:= {victory, o}).

check_board_for_draw_test() ->
	Board = {o, x, o,
		  x, o, x,
		  x, o, x},
	?assert(check_board(Board) =:= draw).

check_board_for_undefined_test() ->
	Board = {x, x, o,
		  x, x, o,
		  o, o, empty},
	?assert(check_board(Board) =:= undefined).

is_board_filled_test() ->
	Board1 = {o, x, empty,
	          o, x, x,
	  	  x, o, x},	  
	?assert(is_board_filled(Board1) =:= false),

	Board2 = {o, x, o,
		  x, o, x,
		  x, o, x},
	?assert(is_board_filled(Board2)).

