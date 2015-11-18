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

-spec check_board(tuple()) -> {victory, x} | {victory, o} | draw.
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
		 Mark, _, _} -> {victory, Mark}
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

