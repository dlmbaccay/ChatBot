:- include('helper.pl').

% fever & headache pairing ~ dengue, malaria, typhoid fever
disease(dengue, SymptomList) :-
    member(fever, SymptomList), member(headache, SymptomList),
    member(joint_muscle_pain, SymptomList), member(dehydration, SymptomList), member(skin_rash, SymptomList).

disease(malaria, SymptomList) :-
    member(fever, SymptomList), member(headache, SymptomList),
    member(pale_skin, SymptomList), member(rapid_heart_rate, SymptomList), member(breath_shortness, SymptomList).

disease(typhoid_fever, SymptomList) :-
    member(fever, SymptomList), member(headache, SymptomList),
    member(stomach_pain, SymptomList), member(diarrhea, SymptomList), member(skin_rash, SymptomList).

% main program
consult :- 
    nl, write('---------------------------'),
    nl, write('MEDICAL DIAGNOSTIC CHATBOT'),
    nl, write('---------------------------'), nl, nl,

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

            % joint muscle pain, dehydration
            % skin rash, pale skin
            % rapid heart rate, breath shortness
            % stomach pain, diarrhea
            
            write('Does the patient have joint and muscle pain? (y/n)'), nl,
            read(JointMusclePain),
            (
                JointMusclePain = 'y' -> add_symptom(joint_muscle_pain);
            ),

            write('Does the patient experience dehydration? (y/n)'), nl,
            read(Dehydration),
            (
                Dehydration = 'y' -> add_symptom(dehydration);
            ),

            write('Does the patient have skin rash? (y/n)'), nl,
            read(SkinRash),
            (
                SkinRash = 'y' -> add_symptom(skin_rash);
            ),

            write('Does the patient have pale skin? (y/n)'), nl,
            read(PaleSkin),
            (
                PaleSkin = 'y' -> add_symptom(pale_skin);
            ),

            write('Does the patient have a rapid heart rate? (y/n)'), nl,
            read(RapidHeartRate),
            (
                RapidHeartRate = 'y' -> add_symptom(rapid_heart_rate);
            ),

            write('Does the patient experience shortness of breath? (y/n)'), nl,
            read(BreathShortness),
            (
                BreathShortness = 'y' -> add_symptom(breath_shortness);
            ),

            write('Does the patient have stomach pain? (y/n)'), nl,
            read(StomachPain),
            (
                StomachPain = 'y' -> add_symptom(stomach_pain);
            ),

            write('Does the patient have diarrhea? (y/n)'), nl,
            read(Diarrhea),
            (
                Diarrhea = 'y' -> add_symptom(diarrhea);
            ),

            % Check for possible diagnoses and list them
            symptom_list(SymptomList),
            find_diseases(SymptomList, Diagnoses),
            (
                Diagnoses \= [] -> % if Diagnoses[] is not empty
                nl, write('--------------------------------------------------------'), nl,
                nl, write('Based on your symptoms, you may have:'), nl,
                nl, diagnoses_list(Diagnoses),
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
        
        ChiefComplaint = 5 -> 
            nl, write('--------------------------------------------------------'), nl,
            nl, write('Thank you for consulting with ChatBot!'), nl,
            nl, write('--------------------------------------------------------'), halt
        ;

        nl, write('--------------------------------------------------------'), nl,
        nl, write('Invalid input. Please try again.'), nl,
        nl, write('--------------------------------------------------------'), nl, consult
    ).

:- consult.
