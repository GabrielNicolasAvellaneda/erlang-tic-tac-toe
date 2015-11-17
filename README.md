# erlang-tic-tac-toe
A distributed Tic-Tac-Toe game

## Requirements

- Create a client-server application of the tic-tac-toe game that acts as a state machine, that can be played by two players. It can also be played by 1 player and the computer.
- Enforce player turns by informing the user when his/her turn takes places and when he is not allowed to so some action.
- Detect game over for a win of one of the player or for a draw.
- Assign the player 1 to the first player that connects to the server. Then player 2 to the second one.
- Create a simple shell interface for this game.

## Client to Server Commands/Messages
- connect
	Connects a user to the game server
- disconnect
	Disconnect a user on the game server. If a user was disconnected, allow to connect again and continue the game. If the user does not connect in an specified time period, the game will end and the winner will be the other player.
- place(X,Y)
	Allow the current user to place a mark at the specified position. If the position is already taken or invalid (outsite the board) it will get an error message. If this is not his/her turn it will get an error message.
- stats
	Gets the number of games played and the number of win/losses for each player.
- new game
	Start a new game after a game-over.
- abandon
	Abandon the game. The other player wins.

## Server to Client Commands/Messages
- your_turn
	Informs the user that is his turn.
- connect_response(Player)
	Informs the user that has succesfully connected and the player that has been assinged to him.
- gameover(Reason)
	Informs the user that the game is over. A Reson of you_won, you_loose, game_draw, the_other_player_abandoned.

