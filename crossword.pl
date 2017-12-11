
:- use_module(library(clpfd), [transpose/2]).

%% setup
  crossword([
    [_,_,_,_],
    [8,8,_,8],
    [8,8,_,8],
    [_,_,_,_]
  ]).

  words([
    'hurt',
    'rent',
    'auth'
  ]).

%% report(Content, Title) :-
  report(Content, Title) :-
    print(Title), nl,
    report(Content).

  report([]).
  report([H|T]) :-
    print(H), nl,
    report(T).

%% main function
  solve(Crossword) :-
    words(Words),
    crossword(Crossword),
    transpose(Crossword, Cols),
    append(Crossword, Cols, AllSlots),
    slots(AllSlots, Slots),
    report(Slots, 'Slots'),
    solve(Words, Slots),
    report(Slots, 'Matched Words'),
    report(Crossword, 'Crossword').

%% slots(AllSlots, Slots) - true when all valid slots are added to the words list
  slots(AllSlots, Slots) :-
    exclude(zeros, AllSlots, Slots).

%% zeros(Slot) - true when the slot doesn't contain 0s
  zeros(Slot) :-
    not(member([1,1,1,1], [Slot])).

%% solve(Words, Crossword) - true all a word fits an all the slots
  solve(_, []).
  solve(Words, [Slot|Slots]) :-
    solveWords(Words, Slot),
    solve(Words, Slots).

%% solveWords(Words, Slot) - true when word fits the slot
  solveWords([Word|_], Slot) :-
    atom_chars(Word, Codes),
    member(Codes, [Slot]).
  solveWords([_|Words], Slot) :-
    solveWords(Words, Slot).
