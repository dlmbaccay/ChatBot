:- dynamic symptom/1.
:- dynamic symptom_list/1.

symptom_list([]).

% Append an item to the list
append_symptom(Symptom) :-
    symptom_list(List),
    assertz(symptom_list([Symptom|List])),
    retract(symptom_list(List)),
    true.

remove_symptom(Symptom) :-
    symptom_list(List),
    delete(List, Symptom, NewList),
    retract(symptom_list(List)),
    assertz(symptom_list(NewList)),
    true.

chief_complaint(headache).
chief_complaint(nausea_vomiting).

symptom(sleepiness).
symptom(salivation).
symptom(hyperactivity).
symptom(seizure).
symptom(stiff_neck).
symptom(paralysis).


diagnosis(rabies, [headache, nausea_vomiting, salivation, paralysis, hyperactivity]).
diagnosis(meningitis, [headache, nausea_vomiting, stiff_neck, seizure, sleepiness]).


consult :-
    write('MEDICAL DIAGNOSTIC CHATBOT'), nl, nl,
    write('SELECT YOUR CHIEF COMPLAINTS'), nl,
    read(ChiefComplaint),
    (
        ChiefComplaint = 3 ->
            append_symptom(headache), 
            append_symptom(nausea_vomiting), 

            write('Does the patient profusely salivate? (y/n)'), nl,
            read(Salivation), 
            (
                Salivation = 'y' -> append_symptom(salivation);
                Salivation = 'n' -> remove_symptom(salivation)
            ),

            write('Does the patient have paralysis? (y/n)'), nl,
            read(Paralysis), 
            (
                Paralysis = 'y' -> append_symptom(paralysis);
                Paralysis = 'n' -> remove_symptom(paralysis)
            ),

            write('Does the patient have hyperactivity? (y/n)'), nl,
            read(Hyperactivity),
            (
                Hyperactivity = 'y' -> append_symptom(hyperactivity);
                Hyperactivity = 'n' -> remove_symptom(hyperactivity)
            ),        

            write('Does the patient have stiff neck? (y/n)'), nl,
            read(StiffNeck),
            (
                StiffNeck = 'y' -> append_symptom(stiff_neck);
                StiffNeck = 'n' -> remove_symptom(stiff_neck)
            ),

            write('Does the patient have seizure? (y/n)'), nl,
            read(Seizure),
            (
                Seizure = 'y' -> append_symptom(seizure);
                Seizure = 'n' -> remove_symptom(seizure)
            ),

            write('Does the patient have sleepiness? (y/n)'), nl,
            read(Sleepiness),
            (
                Sleepiness = 'y' -> append_symptom(sleepiness);
                Sleepiness = 'n' -> remove_symptom(sleepiness)
            ),

            % Check for possible diagnoses and list them
            symptom_list(SymptomList),
            (
                diagnosis(Disease, RequiredSymptoms),
                subset(RequiredSymptoms, SymptomList) ->
                    write('Possible diagnosis: '), write(Disease), nl
                ;
                write('No probable diagnosis. Refer to a larger medical facility'), nl
            ),
            
            retractall(symptom_list(_)),
            assertz(symptom_list([]))
        ;
        write('Invalid input. Please try again.'), nl, consult
    ).
