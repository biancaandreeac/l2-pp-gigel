:- ensure_loaded('chat.pl').
%['/Users/macbookair/Desktop/gigel/checker.pl'].

%% match_rule(+Tokens, +UserMemory, +rule(_Expr, _ReplyList, _Actions, _Emotion, _Tag))
%  Returns true if the rule matches the given sentence.
%  Uses User Memory to match Emotions and Tags.
match_rule(Tokens, UserMemory, rule(Expr, _, _, Emotions, Tags)) :-
    length(Tokens, TokLen),
    length(Expr, ExprLen),
    TokLen == ExprLen,
    get_emotion(UserMemory, Emotion),
    get_tag(UserMemory, Tag),
    (
        % Rule with Emotion
        Emotions \= [],
        [Emotion] == Emotions;
        % Rule with Tags
        Tags \= [],
        [Tag] == Tags;
        % No Emotion or Tags
        Emotions == [], Emotion == 'neutru',
        Tags == [], Tag == 'none'
    )
.

%% find_matching_rules(+Tokens, +Rules, +UserMemory, -MatchingRules) :- fail.
%  returns all the matching rules

% checked all rules
find_matching_rules(_, [], _, []).

% check if current Rule is ok
find_matching_rules(Tokens, [Rule | Rules], UserMemory, [RuleFinal | MatchingRules]) :-
    match_rule(Tokens, UserMemory,  Rule),
    Rule = rule(Tokens, Reply, Actions, Emotions, Tags),
    RuleFinal = rule(Tokens, Reply, Actions, Emotions, Tags),
    find_matching_rules(Tokens, Rules, UserMemory, MatchingRules),
    ! % stop, since next definition will be the defined for the "else" case
.

% if current Rule isn't ok
find_matching_rules(Tokens, [_ | Rules], UserMemory, MatchingRules) :-
    find_matching_rules(Tokens, Rules, UserMemory, MatchingRules)
.


%% select_answer(+Tokens, +UserMemory, +BotMemory, -Answer, -Actions)
%  selects a set of rules based on key words, finds the rules that 
%  match with the input, create a list with all elements, make
%  a list of pairs and get the best answer using min_element.
select_answer(Tokens, UserMemory, BotMemory, Answer, Actions) :-
    select_rules(Tokens, Rules),
    find_matching_rules(Tokens, Rules, UserMemory, MatchingRules),
    concat_answers(MatchingRules, AllAnswers, Actions),
    pairList(AllAnswers, BotMemory, List),
    min_element(List, AnswerSentence),
    words(AnswerSentence, Answer)
.

%% select_rules(+Tokens, -Rules).
%  select rules based on key words.
select_rules(Tokens, Rules) :-
    rules(Keys, Rules),
    ord_subset(Keys, Tokens)
.

%% concat_answers(+[Rules], -Answ).
%  concatenate all answers
concat_answers([rule(_, Answ, Action, _, _)], Answ, Action).
concat_answers([rule(_, Answ, _, _, _) | Rest], Res, Action) :-
    concat_answers(Rest, Partial_Res, Action),
    append(Answ, Partial_Res, Res), !
.

%% append(+L1, +L2, -(L1 ++ L2)).
%  append 2 lists and add the score to each element
append([], L, L).
append([H | T], L, [H | R]) :- append(T, L, R).

% create list of pairs <expression, usage>
pairList([], _, []) :- !.
pairList([Answer | Rest], Memory, [(Sentence, Value) | Res]) :-
    unwords(Answer, Sentence),
    get_value(Memory, Sentence, Value),
    pairList(Rest, Memory, Res)
.

%% handle_actions(+Actions).
%  fails only if there is an 'exit' in the list action.
handle_actions(Actions) :- \+ member(exit, Actions).

%% find_occurrences(+UserMemory, -Result).
%  finds each word's # of occurrences.
%  ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
%  Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
find_occurrences(UserMemory, Result) :-
    dict_keys(UserMemory, Sentences),
    create_occurrences_dict(UserMemory, Sentences, memory{}, Result)
.

%% create_occurrences_dict(+UserMemory, +Sentences, +Memory, -Memory).
%  recursive function to create the memory for every sentece
create_occurrences_dict(_, [], Memory, Memory).
create_occurrences_dict(UserMemory, [Sentence | Rest], Memory, FinMemory) :-
    words(Sentence, Tokens),
    get_value(UserMemory, Sentence, Val),
    create_sentence_dict(Tokens, Val, Memory, NewMemory),
    create_occurrences_dict(UserMemory, Rest, NewMemory, FinMemory)
.

%% create_sentence_dict(+Words, +Value +Memory, -Memory).
%  recursive function to create the memory for every word in a sentence.
%  value is # of occurences of the sentence.
create_sentence_dict([], _, Memory, Memory).
create_sentence_dict([Word | Rest], Value, Memory, FinMemory) :-
    (Val = Memory.get(Word), !; Val = 0),
	NewVal is Val + Value,
	NewMemory = Memory.put(Word, NewVal),
    create_sentence_dict(Rest, Value, NewMemory, FinMemory)
.

%% get_happy_score(+UserMemory, -Score).
%  adds the number of occurences of every happy word.
get_happy_score(UserMemory, Score) :- 
    find_occurrences(UserMemory, WordMemory),
    dict_keys(WordMemory, Words),
    happy_score(WordMemory, Words, Score)
.

happy_score(_, [], 0).
% current word is a happy word
happy_score(WordMemory, [Word | Rest], Score) :-
    happy(Word),
    WordScore = WordMemory.get(Word),
    happy_score(WordMemory, Rest, RestScore),
    Score is WordScore + RestScore,
    !
.
% current word isn't a happy word
happy_score(WordMemory, [_ | Rest], Score) :-
    happy_score(WordMemory, Rest, Score)
.

%% get_sad_score(+UserMemory, -Score).
%  adds the number of occurences of every sad word.
get_sad_score(_UserMemory, _Score) :- fail.
get_sad_score(UserMemory, Score) :- 
    find_occurrences(UserMemory, WordMemory),
    dict_keys(WordMemory, Words),
    sad_score(WordMemory, Words, Score)
.

sad_score(_, [], 0).
% current word is a sad word
sad_score(WordMemory, [Word | Rest], Score) :-
    sad(Word),
    WordScore = WordMemory.get(Word),
    sad_score(WordMemory, Rest, RestScore),
    Score is WordScore + RestScore,
    !
.
% current word is not a sad word
sad_score(WordMemory, [_ | Rest], Score) :-
    sad_score(WordMemory, Rest, Score)
.

%% get_emotion(+UserMemory, -Emotion).
% based on the computed scores, give a general emotion.
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
get_emotion(UserMemory, Emotion) :-
    get_happy_score(UserMemory, HappyScore),
    get_sad_score(UserMemory, SadScore),
    (
        (SadScore == HappyScore, Emotion = 'neutru');
        (SadScore >  HappyScore, Emotion = 'trist');
        (SadScore <  HappyScore, Emotion = 'fericit')
    ),
    !
.

%% get_tag_score(+Tag, +UserMemory, -Score).
%  adds scores of words within a specific tag.
get_tag_score(Tag, UserMemory, Score) :-
    find_occurrences(UserMemory, WordMemory),
    dict_keys(WordMemory, Words),
    tag(Tag, TagList),
    tag_score(TagList, WordMemory, Words, Score)
.

tag_score(_, _, [], 0).
% current word can be tagged with the tag
tag_score(TagL, WordMemory, [Word | Rest], Score) :-
    member(Word, TagL),
    WordScore = WordMemory.get(Word),
    tag_score(TagL, WordMemory, Rest, RestScore),
    Score is WordScore + RestScore
.
% current word can't be tagged with the tag
tag_score(TagL, WordMemory, [Word | Rest], Score) :-
    \+ member(Word, TagL),
    tag_score(TagL, WordMemory, Rest, Score)
.

%% get_tag(-UserMemory, +Tag).
%  general tag of the conversation
get_tag(UserMemory, Tag) :-
    findall(X, tag(X, _), Tags),
    get_max_tag(UserMemory, Tags, 0, _, none, Tag)
.

get_max_tag(_, [], Max, Max, MaxTag, MaxTag).
get_max_tag(UserMemory, [Tag | Tags], CurMax, Max, CurMaxTag, MaxTag) :-
    get_tag_score(Tag, UserMemory, Score),
    (
        (Score > CurMax, NextMax = Score, NextTag = Tag) ;
        (Score =< CurMax, NextMax = CurMax, NextTag = CurMaxTag)
    ),
    get_max_tag(UserMemory, Tags, NextMax, Max, NextTag, MaxTag)
.
