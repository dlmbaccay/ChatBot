% working prototype, for revision

:- dynamic symptom/1.
:- dynamic patient_symptom_list/1.

patient_symptom_list([]).

% Append an item to the list
append_symptom(Symptom) :-
    patient_symptom_list(List),
    assertz(patient_symptom_list([Symptom|List])),
    retract(patient_symptom_list(List)),
    true.

% Remove an item from the list
remove_symptom(Symptom) :-
    patient_symptom_list(List),
    delete(List, Symptom, NewList),
    retract(patient_symptom_list(List)),
    assertz(patient_symptom_list(NewList)),
    true.

% fever & headache pairing ~ dengue, malaria, typhoid fever
disease(dengue, SymptomList) :-
    member(fever, SymptomList), % is fever part of SymptomList
    member(headache, SymptomList),
    member(joint_muscle_pain, SymptomList),
    member(dehydration, SymptomList),
    member(skin_rash, SymptomList).

disease(malaria, SymptomList) :-
    member(fever, SymptomList),
    member(headache, SymptomList),
    member(pale_skin, SymptomList),
    member(rapid_heart_rate, SymptomList),
    member(breath_shortness, SymptomList).

disease(typhoid_fever, SymptomList) :-
    member(fever, SymptomList),
    member(headache, SymptomList),
    member(stomach_pain, SymptomList),
    member(diarrhea, SymptomList),
    member(skin_rash, SymptomList).


find_diseases(SymptomList, Diseases) :-
    findall(Disease, disease(Disease, SymptomList), Diseases).

% Helper function to print list elements
write_list([]).
write_list([H|T]) :-
    write('- '), write(H), nl,
    write_list(T).

% main program
consult :- 
    nl, write('MEDICAL DIAGNOSTIC CHATBOT'), nl, nl,
    write('SELECT YOUR CHIEF COMPLAINTS'), nl,
    write('1. Fever & Headache'), nl,
    write('2. Fever & Cough'), nl,
    write('3. Headache & Nausea/Vomiting'), nl,
    write('4. Diarrhea & Nausea/Vomiting'), nl,
    write('5. Exit ChatBot'), nl,
    read(ChiefComplaint),
    (
        ChiefComplaint = 1 ->
            nl, write('Chief Complaints: Fever & Headache'), nl, nl,
            append_symptom(fever),
            append_symptom(headache),
            
            write('Does the patient have joint and muscle pain? (y/n)'), nl,
            read(JointMusclePain),
            (
                JointMusclePain = 'y' -> append_symptom(joint_muscle_pain);
                JointMusclePain = 'n' -> remove_symptom(joint_muscle_pain)
            ),

            write('Does the patient experience dehydration? (y/n)'), nl,
            read(Dehydration),
            (
                Dehydration = 'y' -> append_symptom(dehydration);
                Dehydration = 'n' -> remove_symptom(dehydration)
            ),

            write('Does the patient have skin rash? (y/n)'), nl,
            read(SkinRash),
            (
                SkinRash = 'y' -> append_symptom(skin_rash);
                SkinRash = 'n' -> remove_symptom(skin_rash)
            ),

            write('Does the patient have pale skin? (y/n)'), nl,
            read(PaleSkin),
            (
                PaleSkin = 'y' -> append_symptom(pale_skin);
                PaleSkin = 'n' -> remove_symptom(pale_skin)
            ),

            write('Does the patient have a rapid heart rate? (y/n)'), nl,
            read(RapidHeartRate),
            (
                RapidHeartRate = 'y' -> append_symptom(rapid_heart_rate);
                RapidHeartRate = 'n' -> remove_symptom(rapid_heart_rate)
            ),

            write('Does the patient experience shortness of breath? (y/n)'), nl,
            read(BreathShortness),
            (
                BreathShortness = 'y' -> append_symptom(breath_shortness);
                BreathShortness = 'n' -> remove_symptom(breath_shortness)
            ),

            write('Does the patient have stomach pain? (y/n)'), nl,
            read(StomachPain),
            (
                StomachPain = 'y' -> append_symptom(stomach_pain);
                StomachPain = 'n' -> remove_symptom(stomach_pain)
            ),

            write('Does the patient have diarrhea? (y/n)'), nl,
            read(Diarrhea),
            (
                Diarrhea = 'y' -> append_symptom(diarrhea);
                Diarrhea = 'n' -> remove_symptom(diarrhea)
            ),

            % Check for possible diagnoses and list them
            patient_symptom_list(SymptomList),
            find_diseases(SymptomList, Diagnoses),
            (
                Diagnoses \= [] -> % if Diagnoses[] is not empty
                nl, write('--------------------------------------------------------'), nl,
                nl, write('Based on your symptoms, you may have:'), nl,
                nl, write_list(Diagnoses),
                nl, write('--------------------------------------------------------'), nl
                ;
                nl, write('-----------------------------------------------------------'), nl,
                nl, write('No probable diagnosis. Refer to a larger medical facility.'), nl,                
                nl, write('-----------------------------------------------------------'), nl
            ), 
            
            retractall(patient_symptom_list(_)),
            retractall(diagnoses(_)),
            assertz(patient_symptom_list([])), consult
        ;
        ChiefComplaint = 5 -> halt
        ;
        write('Invalid input. Please try again.'), nl, consult
    ).

:- consult.