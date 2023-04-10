:- include('revamp.pl'). % convert to proper name later

:- dynamic symptoms_list/1.

symptoms_list([]).
diagnoses_list([]).

% finds all probable diseases based on patient's symptoms
find_diseases(SymptomList, Diseases) :-
    findall(Disease, disease(Disease, SymptomList), Diseases).

add_symptom(Symptom) :-
    symptoms_list(OldList), append(OldList, [Symptom], NewList),
    retract(symptoms_list(_)), assertz(symptoms_list(NewList)).

print_symptoms([]).
print_symptoms([H|T]) :-
    string_upper(H, Symptom),
    write('- '), write(Symptom), nl,
    print_symptoms(T).

print_diagnoses([]).
print_diagnoses([H|T]) :-
    string_upper(H, Disease),
    write('- '), write(Disease), nl,
    print_symptoms(T).