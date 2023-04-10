:- consult('knowledge.pl').

:- dynamic symptoms_list/1. % allows symptoms to be added on list during runtime

symptoms_list([]).

ask_symptom(Question, Symptom) :-
    write(Question), nl,
    read(Response),
    (
        Response = 'y' -> add_symptom(Symptom);
        Response = 'n' -> true
    ).

print_symptoms([]). % print patient's symptoms
print_symptoms([H|T]) :-
    string_upper(H, Symptom),
    write('- '), write(Symptom), nl,
    print_symptoms(T).

print_diagnoses([]). % print probable diagnoses
print_diagnoses([H|T]) :-
    string_upper(H, Disease),
    write('- '), write(Disease), nl,
    print_symptoms(T).

add_symptom(Symptom) :- % add symptom to symptom_list
    symptoms_list(OldList), 
    append(OldList, [Symptom], NewList),
    retract(symptoms_list(_)), 
    assertz(symptoms_list(NewList)).

find_diseases(SymptomList, Diseases) :- % finds all probable diseases based on patient's symptoms
    findall(Disease, disease(Disease, SymptomList), Diseases).

check_for_diagnoses :- % checks if there are probable diagnoses, and outputs such
    symptoms_list(SymptomList),
    find_diseases(SymptomList, Diagnoses),
    (
        Diagnoses \= [] -> % if Diagnoses[] is not empty
        nl, write('--------------------------------------------------------'), nl,
        nl, write('Based on these symptoms:'), nl,
        nl, print_symptoms(SymptomList),
        nl, write('You may have:'), nl,
        nl, print_diagnoses(Diagnoses),
        nl, write('--------------------------------------------------------'), nl;
        nl, write('-----------------------------------------------------------'), nl,
        nl, write('No probable diagnosis. Refer to a larger medical facility.'), nl,                
        nl, write('-----------------------------------------------------------'), nl
    ).

ask_chief_complaint :- % asks for chief complaint
    write('SELECT YOUR CHIEF COMPLAINTS'), nl,
    write('1. Fever & Headache'), nl,
    write('2. Fever & Cough'), nl,
    write('3. Headache & Nausea/Vomiting'), nl,
    write('4. Diarrhea & Nausea/Vomiting'), nl,
    write('5. Exit ChatBot'), nl.

ask_symptoms_CC1 :- % fever & headache pairing
    nl, write('-----------------------------------------------------------'), nl,
    nl, write('Chief Complaints: Fever & Headache'), nl, nl,
    add_symptom(fever), 
    add_symptom(headache),
    ask_symptom('Does the patient have joint and muscle pain? (y/n)', joint_muscle_pain),
    ask_symptom('Does the patient experience dehydration? (y/n)', dehydration),
    ask_symptom('Does the patient have skin rash? (y/n)', skin_rash),
    ask_symptom('Does the patient have pale skin? (y/n)', pale_skin),
    ask_symptom('Does the patient have a rapid heart rate? (y/n)', rapid_heart_rate),
    ask_symptom('Does the patient experience shortness of breath? (y/n)', breath_shortness),
    ask_symptom('Does the patient have stomach pain? (y/n)', stomach_pain),
    ask_symptom('Does the patient have diarrhea? (y/n)', diarrhea).

ask_symptoms_CC2 :- % fever & cough pairing
    nl, write('-----------------------------------------------------------'), nl,
    nl, write('Chief Complaints: Fever & Cough'), nl, nl,
    add_symptom(fever), 
    add_symptom(cough), 
    ask_symptom('Does the patient have chest pain? (y/n)', chest_pain),
    ask_symptom('Does the patient cough up blood? (y/n)', blood_cough),
    ask_symptom('Does the patient experience shortness of breath? (y/n)', breath_shortness),
    ask_symptom('Does the patient cough up phlegm? (y/n)', phlegm_cough),
    ask_symptom('Does the patient experience chronic fatigue? (y/n)', fatigue),
    ask_symptom('Does the patient have a sore throat? (y/n)', sore_throat),
    ask_symptom('Does the patient have skin rash? (y/n)', skin_rash),
    ask_symptom('Does the patient have a dry cough? (y/n)', dry_cough).

ask_symptoms_CC3 :- % headache & nausea/vomiting pairing
    nl, write('-----------------------------------------------------------'), nl,
    nl, write('Chief Complaints: Headache & Nausea/Vomiting'), nl, nl,
    add_symptom(headache), 
    add_symptom(nausea_vomiting), 
    ask_symptom('Does the patient profusely salivate? (y/n)', salivation),
    ask_symptom('Does the patient have paralysis? (y/n)', paralysis),
    ask_symptom('Does the patient have hyperactivity? (y/n)', hyperactivity),
    ask_symptom('Does the patient have stiff neck? (y/n)', stiff_neck),
    ask_symptom('Does the patient have seizure? (y/n)', seizure),
    ask_symptom('Does the patient experience prominent sleepiness? (y/n)', sleepiness).

ask_symptoms_CC4 :- % diarrhea & nausea/vomiting pairing
    nl, write('-----------------------------------------------------------'), nl,
    nl, write('Chief Complaints: Diarrhea & Nausea/Vomiting'), nl, nl,
    add_symptom(diarrhea), 
    add_symptom(nausea_vomiting), 
    ask_symptom('Does the patient experience dehydration? (y/n)', dehydration),
    ask_symptom('Does the patient experience restlessness? (y/n)', restlessness),
    ask_symptom('Does the patient experience muscle cramps? (y/n)', muscle_cramps),
    ask_symptom('Does the patient have yellow-ish eyes and/or skin? (y/n)', jaundice),
    ask_symptom('Does the patient pee dark urine?', dark_urine),
    ask_symptom('Does the patient experience intense itching? (y/n)', intense_itching).
    
reset_chatbot :- % resets symptom list and diagnoses, for reuse
    retractall(symptoms_list(_)),
    retractall(diagnoses(_)),
    assertz(symptoms_list([])).