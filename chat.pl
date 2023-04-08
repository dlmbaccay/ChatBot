% working prototype, for revision

:- dynamic symptom/1.
:- dynamic symptom_list/1.

symptom_list([]).

% Append an item to the list
append_symptom(Symptom) :-
    symptom_list(List),
    assertz(symptom_list([Symptom|List])),
    retract(symptom_list(List)),
    true.

% Remove an item from the list
remove_symptom(Symptom) :-
    symptom_list(List),
    delete(List, Symptom, NewList),
    retract(symptom_list(List)),
    assertz(symptom_list(NewList)),
    true.

% declare all chief complaints
chief_complaint(cough).
chief_complaint(diarrhea).
chief_complaint(fever).
chief_complaint(headache).
chief_complaint(nausea_vomiting).

% declare all possible symptoms mentioned as HPIs
symptom(blood_cough).
symptom(breath_shortness).
symptom(chest_pain).
symptom(constipation).
symptom(dark_urine).
symptom(dehydration).
symptom(diarrhea).
symptom(dry_cough).
symptom(fatigue).
symptom(hyperactivity).
symptom(intense_itching).
symptom(jaundice).
symptom(joint_muscle_pain).
symptom(muscle_cramps).
symptom(pale_skin).
symptom(paralysis).
symptom(phlegm_cough).
symptom(rapid_heart_rate).
symptom(restlessness).
symptom(salivation).
symptom(seizure).
symptom(skin_rash).
symptom(sleepiness).
symptom(sore_throat).
symptom(stiff_neck).
symptom(stomach_pain).

% rules for probable diagnoses based on patient's symptoms

% dengue
dengue(SymptomList) :-
    length(SymptomList, 5), % SymptomList must have EXACTLY 5 symptoms 
    member(fever, SymptomList), % is fever part of SymptomList
    member(headache, SymptomList),
    member(joint_muscle_pain, SymptomList),
    member(dehydration, SymptomList),
    member(skin_rash, SymptomList).

malaria(SymptomList) :-
    length(SymptomList, 5),
    member(fever, SymptomList),
    member(headache, SymptomList),
    member(pale_skin, SymptomList),
    member(rapid_heart_rate, SymptomList),
    member(breath_shortness, SymptomList).

typhoid_fever(SymptomList) :-
    length(SymptomList, 5),
    member(fever, SymptomList),
    member(headache, SymptomList),
    member(stomach_pain, SymptomList),
    member(diarrhea, SymptomList),
    member(skin_rash, SymptomList).

tuberculosis(SymptomList) :-
    length(SymptomList, 5),
    member(fever, SymptomList), 
    member(cough, SymptomList),
    member(chest_pain, SymptomList),
    member(blood_cough, SymptomList),
    member(breath_shortness, SymptomList).

pneumonia(SymptomList) :-
    length(SymptomList, 5),
    member(fever, SymptomList), 
    member(cough, SymptomList),
    member(chest_pain, SymptomList),
    member(phlegm_cough, SymptomList),
    member(fatigue, SymptomList).

measles(SymptomList) :-
    length(SymptomList, 5),
    member(fever, SymptomList),
    member(cough, SymptomList),
    member(sore_throat, SymptomList),
    member(skin_rash, SymptomList),
    member(dry_cough, SymptomList).

rabies :-
    length(SymptomList, 5),
    member(headache, SymptomList),
    member(nausea_vomiting, SymptomList),
    member(salivation, SymptomList),
    member(paralysis, SymptomList),
    member(hyperactivity, SymptomList).

meningitis :-
    length(SymptomList, 5),
    member(headache, SymptomList),
    member(nausea_vomiting, SymptomList),
    member(stiff_neck, SymptomList),
    member(seizure, SymptomList),
    member(sleepiness, SymptomList).

hepatitis_A :-
    length(SymptomList, 5),
    member(diarrhea, SymptomList),
    member(nausea_vomiting, SymptomList),
    member(jaundice, SymptomList),
    member(dark_urine, SymptomList),
    member(intense_itching, SymptomList).

cholera :-
    length(SymptomList, 5),
    member(diarrhea, SymptomList),
    member(nausea_vomiting, SymptomList),
    member(dehydration, SymptomList),
    member(restlessness, SymptomList),
    member(muscle_cramps, SymptomList).

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

            % joint muscle pain
            % dehydration
            % skin rash
            % pale skin
            % rapid heart rate
            % breath shortness
            % stomach pain
            % diarrhea
            
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
            symptom_list(SymptomList),
            (
                dengue(SymptomList) -> 
                    nl, write('--------------------------------------------------------'), nl,
                    nl, write('Based on your symptoms, you may have: Dengue.'), nl,
                    nl, write('--------------------------------------------------------'), nl
                ;
                malaria(SymptomList) ->
                    nl, write('--------------------------------------------------------'), nl,
                    nl, write('Based on your symptoms, you may have: Malaria.'), nl,
                    nl, write('--------------------------------------------------------'), nl
                ;
                typhoid_fever(SymptomList) ->
                    nl, write('--------------------------------------------------------'), nl,
                    nl, write('Based on your symptoms, you may have: Typhoid Fever.'), nl,
                    nl, write('--------------------------------------------------------'), nl
                ;
                nl, write('--------------------------------------------------------'), nl,
                nl, write('No probable diagnosis. Refer to a larger medical facility.'), nl,                
                nl, write('--------------------------------------------------------'), nl
            ), 
            
            retractall(symptom_list(_)),
            assertz(symptom_list([])), consult
        ;
        ChiefComplaint = 5 -> halt
        ;
        write('Invalid input. Please try again.'), nl, consult
    ).

:- consult.