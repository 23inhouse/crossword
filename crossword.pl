
:- use_module(library(clpfd)).

%% setup
  crossword([
    [_,_,_,8],
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
    solve(Words, Slots),
    report(Slots, 'Matched Words'),
    report(Crossword, 'Crossword').

%% slots(AllSlots, Slots) - true when all valid slots are added to the words list
  slots(AllSlots, Slots) :-
    slot_groups(AllSlots, [], RandomSlots),
    sort(RandomSlots, Slots).

  slot_groups([], Acc, Acc).
  slot_groups([H|T], Acc, Slots) :-
    slot_group(H, Groups),
    include(emptySlot(), Groups, EmptySlots),
    append(Acc, EmptySlots, NewAcc),
    slot_groups(T, NewAcc, Slots).

%% slot_group([_,_,8,_,8,_], Groups).
  slot_group(Slots, Groups) :-
    foldl(with_vars(), Slots, Pairs, _, _),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, KeyGroups),
    pairs_values(KeyGroups, Groups).

  with_vars(L, R-I-L, I0, I) :-
    ( var(L) -> ( var(I0) -> R #= I, I0 #= I; R #= I ) ; R #= I, 0 #= I ).

%% emptySlot(Slot) -
  emptySlot([H|T]) :-
    var(H),
    not(length(T, 0)).

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
