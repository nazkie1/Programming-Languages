
% Set bounds to 5 x 5. A for rows and B for columns.
inside_bounds(A, B) :- A > 0, A =< 5, B > 0, B =< 5.

% Set 4 possible directions to move and their actions. Does the move as long as the next point is inside bounds.
go_right((A, B), (A, B_NEXT)) :- B_NEXT is B + 1. 
go_left((A, B), (A, B_NEXT)) :- B_NEXT is B - 1.
go_up((A, B), (A_NEXT, B)) :- A_NEXT is A - 1.
go_down((A, B), (A_NEXT, B)) :- A_NEXT is A + 1.

% Conditions for "the move to be made" to be accepted.
accepted_move((A, B),(A_NEXT, B_NEXT), _ , Obstacles, Visited) :-
	inside_bounds(A, B), inside_bounds(A_NEXT, B_NEXT),        % Starting point & next point must be within bounds
    \+ member((A, B), Obstacles),\+ member((A_NEXT, B_NEXT), Obstacles),    % Starting point & next point has to be non-obstacle
    \+ member((A_NEXT, B_NEXT), Visited).  	% Next point must be non-visited

% The rule which will run form_path. We'll give our inputs with path.
path(Start, Finish, Obstacles, Path) :- 
    form_path(Start, Finish, Obstacles, [Start], Path).

% Base case.
form_path(Finish, Finish, _ , Visited, Path) :-
    reverse(Visited, Path). % Reverse the order of visited to see the road clearly.

% Recursive case.
form_path((A, B), Finish, Obstacles, Visited, Path) :-
    
    (go_right((A, B), Next) ;  %Tries to go in one of this directions each time...
     go_down((A, B), Next);
     go_left((A, B), Next) ;
     go_up((A, B), Next)), %AND
     
    
     accepted_move((A,B), Next, Finish, Obstacles, Visited), % Goes in choosen direction if the move is accepted.
    															 % After going in that direction, calls form_path 
     form_path(Next, Finish, Obstacles, [Next | Visited], Path). % with: start point as that gone (next) node and
																 % visited list updated as Visited + gone (next) node. Recursively.

      
