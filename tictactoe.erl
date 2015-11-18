-module(tictactoe).
-export([start/0, init/0]).

-include_lib("eunit/include/eunit.hrl").

-define(TICTACTOE_INSTANCE_NAME, tictactoe_server).
-define(TICTACTOE_NODE_NAME, 'server@vagrant-ubuntu-trusty-64').

-record(server_state, { players = [], board, current_state}). 

%% NOTE: You must run the tictactoe server process in a node named as defined by TICTACTOE_NODE_NAME.
start() ->
	Pid = spawn(?MODULE, init, []),
	register(?TICTACTOE_INSTANCE_NAME, Pid). 

init() ->
	InitialState = create_server_state(),
	loop(InitialState).

loop(State) ->
	receive
		{From, connect}	-> UpdatedState = server_handle_connect(From, State);
		Unrecognized -> UpdatedState = server_handle_unrecognized_message(Unrecognized, State)
	end,
	loop(UpdatedState).

player_id_to_name(player_1) -> 
	"Player 1 (X) - Crosses";
player_id_to_name(player_2) ->
	"Player 2 (O) - Circles".

add_player(Player, Players) ->
	[Player | Players].

get_free_player(State) ->
	Players = server_state_get_players(State),
	case lists:keymember(player_1, 1, Players) of
		false -> player_1;
	        true -> case lists:keymember(player_2, 1, Players) of  	
				false -> player_2;
				true -> undefined 
			end
	end.

may_connect_player(From, State) ->
	case get_free_player(State) of
		undefined ->
			{error, no_more_players_allowed};
		FreePlayer ->
			Players = server_state_get_players(State),
			UpdatedPlayers = add_player({FreePlayer, From}, Players),
			UpdatedState = server_state_set_players(UpdatedPlayers, State),
			{ok, FreePlayer, UpdatedState}
	end.

server_handle_connect(From, State) ->
	case may_connect_player(From, State) of
		{ok, Player, UpdatedState} ->
			%% TODO: Inform the player what mark he got.		       
			UpdatedState;
		{error, no_more_players_allowed} ->
			%% TODO: Inform the player that we can not connect because there isn't any free place.
			State
	end.

server_handle_unrecognized_message(Message, ServerState) ->
	error_logger:info_msg("Unrecognized message ~p~n", [Message]),
	ServerState.

-spec create_server_state() -> #server_state{}.
create_server_state() ->
	Board = create_empty_board(),
	#server_state{board = Board}.

-spec server_state_get_players(#server_state{}) -> list({pid()}).
server_state_get_players(#server_state{players = Players}) -> Players.

-spec server_state_set_players(list(), #server_state{}) -> #server_state{}.
server_state_set_players(Players, ServerState = #server_state{}) -> ServerState#server_state{players=Players}.

-spec create_empty_board() -> tuple().
create_empty_board() ->
	{empty, empty, empty,
	 empty, empty, empty,
	 empty, empty, empty}.

-spec is_board_filled(tuple()) -> boolean().
is_board_filled(Board) ->
	Marks = tuple_to_list(Board),
	not lists:any(fun (Mark) -> Mark == empty end, Marks).

-spec check_board_for_victory(tuple()) -> {victory, x} | {victory, o} | undefined.
check_board_for_victory(Board) ->
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

		_ -> undefined
	end.

-spec check_board(tuple()) -> {victory, x} | {victory, o} | draw | undefined.
check_board(Board) ->
	case check_board_for_victory(Board) of
		{victory, Mark} -> {victory, Mark};

		undefined -> %% It can be a draw or undefined (that's it, the board is not yet complete).
			case is_board_filled(Board) of
				false -> undefined;
				true -> draw
			end
	end.

-spec to_linear_index(pos_integer(), pos_integer()) -> pos_integer().
to_linear_index(X, Y) ->
	(Y - 1) * 3 + X.

-spec is_position_empty(pos_integer(), pos_integer(), tuple()) -> boolean().
is_position_empty(X, Y, Board) ->
	Index = to_linear_index(X, Y),
	element(Index, Board) =:= empty.

-spec board_set_position(atom(), pos_integer(), pos_integer(), tuple()) -> tuple().
board_set_position(Mark, X, Y, Board) ->
	Index = to_linear_index(X, Y),
	setelement(Index, Board, Mark).

-spec play(atom(), pos_integer(), pos_integer(), tuple()) -> {ok, tuple()} | {error, position_already_taken} | {error, out_of_range_position}.
play(Mark, X, Y, Board) ->
	case is_position_empty(X, Y, Board) of
		true -> board_set_position(Mark, X, Y, Board);
		false -> {error, position_already_taken}
	end.

create_empty_board_test() ->
	?assertEqual(create_empty_board(), {empty, empty, empty, empty, empty, empty, empty, empty, empty}).

check_board_for_victory_in_row_test() ->
	Board1 = {x, x, x,
		 o, o, empty,
		 empty, empty, empty},
	?assertEqual(check_board(Board1), {victory, x}),

	Board2 = {o, o, o,
		  empty, x, empty,
		  empty, empty, x},
	?assertEqual(check_board(Board2), {victory, o}).

check_board_for_victory_in_column_test() ->
	Board1 = {x, o, o,
		  x, empty, empty,
		  x, empty, empty},
	?assertEqual(check_board(Board1), {victory, x}).

check_board_for_victory_in_diagonal_test() ->
	Board1 = {o, x, empty,
		  empty, o, x,
		  empty,x, o},
	?assertEqual(check_board(Board1), {victory, o}).

check_board_for_draw_test() ->
	Board = {o, x, o,
		  x, o, x,
		  x, o, x},
	?assertEqual(check_board(Board), draw).

check_board_for_undefined_test() ->
	Board = {x, x, o,
		  x, x, o,
		  o, o, empty},
	?assertEqual(check_board(Board), undefined).

is_board_filled_test() ->
	Board1 = {o, x, empty,
	          o, x, x,
	  	  x, o, x},	  
	?assertNot(is_board_filled(Board1)),

	Board2 = {o, x, o,
		  x, o, x,
		  x, o, x},
	?assert(is_board_filled(Board2)).

play_on_empty_position_test() ->
	Board = {empty, empty, empty,
	 	x, o, x,
       		o, x, o},
	ExpectedBoard = {x, empty, empty,
			x, o, x,
		       o, x, o},	
	?assertEqual(play(x, 1,1, Board), ExpectedBoard).   	

play_on_non_empty_position_test() ->
	Board = {empty, empty, x,
		 x, o, x,
		 empty, empty, empty},
	?assertEqual(play(o, 1,2, Board), {error, position_already_taken}).

may_connect_player_test() ->
	State = create_server_state(),
	?assertEqual(server_state_get_players(State), []),
	{ok, player_1, UpdatedState1} = may_connect_player(some_pid, State), 
	?assertEqual(server_state_get_players(UpdatedState1), [{player_1, some_pid}]),
	{ok, player_2, UpdatedState2} = may_connect_player(another_pid, UpdatedState1),
	?assertEqual(server_state_get_players(UpdatedState2), [{player_2, another_pid}, {player_1, some_pid}]).

get_free_player_test() ->
	State = create_server_state(),
	?assertEqual(get_free_player(State), player_1),
	UpdatedState1 = server_state_set_players([{player_1, some_pid}], State),
	?assertEqual(get_free_player(UpdatedState1), player_2),
	UpdatedState2 = server_state_set_players([{player_2, another_pid}, {player_1, some_pid}], UpdatedState1),
	?assertEqual(get_free_player(UpdatedState2), undefined).

