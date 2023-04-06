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
chief_complaint(fever).
chief_complaint(cough).
chief_complaint(headache).
chief_complaint(nausea_vomiting).
chief_complaint(diarrhea).

% declare all symptoms
symptom(skin_rash).
symptom(pale_skin).
symptom(rapid_heart_rate).
symptom(breath_shortness).
symptom(sleepiness).
symptom(fatigue).

symptom(stomach_pain).
symptom(diarrhea).
symptom(constipation).
symptom(chest_pain).

symptom(dehydration).
symptom(salivation).
symptom(blood_cough).
symptom(phlegm_cough).
symptom(dry_cough).
symptom(sore_throat).

symptom(joint_muscle_pain).
symptom(muscle_cramps).
symptom(stiff_neck).
symptom(hyperactivity).
symptom(seizure).

symptom(restlessness).
symptom(paralysis).
symptom(jaundice).
symptom(dark_urine).
symptom(intense_itching).

% fever & headache pairing ~ dengue, malaria, typhoid fever
diagnosis(dengue, [fever, headache, joint_muscle_pain, dehydration, skin_rash]).
diagnosis(malaria, [fever, headache, pale_skin, rapid_heart_rate, breath_shortness]).
diagnosis(typhoid_fever, [fever, headache, stomach_pain, diarrhea, skin_rash]).

% fever & cough pairing ~ tuberculosis, pnuemonia, measles
diagnosis(tuberculosis, [fever, cough, chest_pain, blood_cough, breath_shortness]).
diagnosis(pnuemonia, [fever, cough, chest_pain, phlegm_cough, fatigue]).
diagnosis(measles, [fever, cough, sore_throat, skin_rash, dry_cough]).

% headache & nausea_vomiting pairing ~ rabies & meningitis
diagnosis(rabies, [headache, nausea_vomiting, salivation, paralysis, hyperactivity]).
diagnosis(meningitis, [headache, nausea_vomiting, stiff_neck, seizure, sleepiness]).

% diarrhea & nausea_vomiting pairing ~ hepatitis & cholera
diagnosis(hepatitis_A, [diarrhea, nausea_vomiting, jaundice, dark_urine, intense_itching]).
diagnosis(cholera, [diarrhea, nausea_vomiting, dehydration, restlessness, muscle_cramps]).

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
                diagnosis(Disease, RequiredSymptoms),
                subset(RequiredSymptoms, SymptomList) ->
                    nl, write('--------------------------------------------------------'), nl,
                    nl, write('Based on your symptoms, you may have: '), write(Disease), nl,
                    nl, write('--------------------------------------------------------'), nl
                ;
                nl, write('--------------------------------------------------------'), nl,
                nl, write('No probable diagnosis. Refer to a larger medical facility.'), nl,                
                nl, write('--------------------------------------------------------'), nl
            ),
            
            retractall(symptom_list(_)),
            assertz(symptom_list([])), consult
        ;

        ChiefComplaint = 2 -> 
            nl, write('Chief Complaints: Fever & Cough'), nl, nl,
            append_symptom(fever), 
            append_symptom(cough), 

            % chest pain
            % coughing blood
            % shortness of breath
            % coughing phlegm
            % chronic fatigue
            % sore throat
            % skin rash
            % dry cough

            write('Does the patient have chest pain? (y/n)'), nl,
            read(ChestPain),
            (
                ChestPain = 'y' -> append_symptom(chest_pain);
                ChestPain = 'n' -> remove_symptom(chest_pain)
            ),

            write('Does the patient cough up blood? (y/n)'), nl,
            read(CoughingBlood),
            (
                CoughingBlood = 'y' -> append_symptom(blood_cough);
                CoughingBlood = 'n' -> remove_symptom(blood_cough)
            ),

            write('Does the patient experience shortness of breath? (y/n)'), nl,
            read(ShortnessOfBreath),
            (
                ShortnessOfBreath = 'y' -> append_symptom(breath_shortness);
                ShortnessOfBreath = 'n' -> remove_symptom(breath_shortness)
            ),

            write('Does the patient cough up phlegm? (y/n)'), nl,
            read(CoughingPhlegm),
            (
                CoughingPhlegm = 'y' -> append_symptom(phlegm_cough);
                CoughingPhlegm = 'n' -> remove_symptom(phlegm_cough)
            ),

            write('Does the patient experience chronic fatigue? (y/n)'), nl,
            read(ChronicFatigue),
            (
                ChronicFatigue = 'y' -> append_symptom(fatigue);
                ChronicFatigue = 'n' -> remove_symptom(fatigue)
            ),

            write('Does the patient have a sore throat? (y/n)'), nl,
            read(SoreThroat),
            (
                SoreThroat = 'y' -> append_symptom(sore_throat);
                SoreThroat = 'n' -> remove_symptom(sore_throat)
            ),

            write('Does the patient have skin rash? (y/n)'), nl,
            read(SkinRash),
            (
                SkinRash = 'y' -> append_symptom(skin_rash);
                SkinRash = 'n' -> remove_symptom(skin_rash)
            ),

            write('Does the patient have a dry cough? (y/n)'), nl,
            read(DryCough),
            (
                DryCough = 'y' -> append_symptom(dry_cough);
                DryCough = 'n' -> remove_symptom(dry_cough)
            ),

            % Check for possible diagnoses and list them
            symptom_list(SymptomList),
            (
                diagnosis(Disease, RequiredSymptoms),
                subset(RequiredSymptoms, SymptomList) ->
                    nl, write('--------------------------------------------------------'), nl,
                    nl, write('Based on your symptoms, you may have: '), write(Disease), nl,
                    nl, write('--------------------------------------------------------'), nl
                ;
                nl, write('--------------------------------------------------------'), nl,
                nl, write('No probable diagnosis. Refer to a larger medical facility.'), nl,                
                nl, write('--------------------------------------------------------'), nl
            ),
            
            retractall(symptom_list(_)),
            assertz(symptom_list([])), consult
        ;

        ChiefComplaint = 3 ->
            nl, write('Chief Complaints: Headache & Nausea/Vomiting'), nl, nl,
            append_symptom(headache), 
            append_symptom(nausea_vomiting), 

            % salivation
            % paralysis
            % hyperactivity
            % stiff_neck
            % seizure
            % sleepiness

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

            write('Does the patient experience prominent sleepiness? (y/n)'), nl,
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
                    nl, write('--------------------------------------------------------'), nl,
                    nl, write('Based on your symptoms, you may have: '), write(Disease), nl,
                    nl, write('--------------------------------------------------------'), nl
                ;
                nl, write('--------------------------------------------------------'), nl,
                nl, write('No probable diagnosis. Refer to a larger medical facility.'), nl,                
                nl, write('--------------------------------------------------------'), nl
            ),
            
            retractall(symptom_list(_)),
            assertz(symptom_list([])), consult
        ;
        
        ChiefComplaint = 4 ->
            nl, write('Chief Complaints: Diarrhea & Nausea/Vomiting'), nl, nl,
            append_symptom(diarrhea), 
            append_symptom(nausea_vomiting), 

            % dehydration
            % restlessness
            % muscle_cramps
            % jaundice
            % dark_urine
            % intense_itching

            write('Does the patient experience dehydration? (y/n)'), nl,
            read(Dehydration), 
            (
                Dehydration = 'y' -> append_symptom(dehydration);
                Dehydration = 'n' -> remove_symptom(dehydration)
            ),

            write('Does the patient experience restlessness? (y/n)'), nl,
            read(Restlessness), 
            (
                Restlessness = 'y' -> append_symptom(restlessness);
                Restlessness = 'n' -> remove_symptom(restlessness)
            ),

            write('Does the patient experience muscle cramps? (y/n)'), nl,
            read(MuscleCramps),
            (
                MuscleCramps = 'y' -> append_symptom(muscle_cramps);
                MuscleCramps = 'n' -> remove_symptom(muscle_cramps)
            ),

            write('Does the patient have yellow-ish eyes and/or skin? (y/n)'), nl,
            read(Jaundice),
            (
                Jaundice = 'y' -> append_symptom(jaundice);
                Jaundice = 'n' -> remove_symptom(jaundice)
            ),

            write('Does the patient pee dark urine? (y/n)'), nl,
            read(DarkUrine),
            (
                DarkUrine = 'y' -> append_symptom(dark_urine);
                DarkUrine = 'n' -> remove_symptom(dark_urine)
            ),

            write('Does the patient experience intense itching? (y/n)'), nl,
            read(IntenseItching),
            (
                IntenseItching = 'y' -> append_symptom(intense_itching);
                IntenseItching = 'n' -> remove_symptom(intense_itching)
            ),

            % Check for possible diagnoses and list them
            symptom_list(SymptomList),
            (
                diagnosis(Disease, RequiredSymptoms),
                subset(RequiredSymptoms, SymptomList) ->
                    nl, write('--------------------------------------------------------'), nl,
                    nl, write('Based on your symptoms, you may have: '), write(Disease), nl,
                    nl, write('--------------------------------------------------------'), nl
                ;
                nl, write('--------------------------------------------------------'), nl,
                nl, write('No probable diagnosis. Refer to a larger medical facility.'), nl,                
                nl, write('--------------------------------------------------------'), nl
            ),
            
            retractall(symptom_list(_)),
            assertz(symptom_list([])), consult
        ;
        
        ChiefComplaint = 5 -> 
            nl, write('--------------------------------------------------------'), nl,
            nl, write('Thank you for consulting with ChatBot!'), nl,
            nl, write('--------------------------------------------------------'), halt
        ;
        write('Invalid input. Please try again.'), nl, consult
    ).

:- consult.
% to run program in terminal, download SWI-Prolog and integrate it to your terminal
% to run program in SWISH, comment out ':- consult.', and type 'consult.' to prompt instead :)