:- dynamic symptom/1. % to make predicates dynamic, subject to assertion ~ true or false

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
symptom(phleghm_cough).
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

% fever & headache pairing

% dengue 
% malaria
% typhoid fever

diagnosis(dengue) :- 
    chief_complaint(fever), chief_complaint(headache), % chief complaint
    symptom(joint_muscle_pain), symptom(dehydration), symptom(skin_rash). % HPIs

diagnosis(malaria) :-
    chief_complaint(fever), chief_complaint(headache), % chief complaint
    symptom(pale_skin), symptom(rapid_heart_rate), symptom(breath_shortness). % HPIs

diagnosis(typhoid_fever) :-
    chief_complaint(fever), chief_complaint(headache), % chief complaint
    symptom(stomach_pain), (symptom(diarrhea) ; symptom(constipation)), symptom(skin_rash). % HPIs

% joint muscle pain
% dehydration
% skin rash
% pale skin
% rapid heart rate
% breath shortness
% stomach pain
% diarrhea or constipation

% fever & cough pairing

% tuberculosis
% pneumonia
% measles

diagnosis(tuberculosis) :-
    chief_complaint(fever), chief_complaint(cough), % chief complaint
    symptom(chest_pain), symptom(blood_cough), symptom(breath_shortness). % HPIs

diagnosis(pneumonia) :-
    chief_complaint(fever), chief_complaint(cough), % chief complaint
    symptom(chest_pain), symptom(phleghm_cough), symptom(fatigue). % HPIs

diagnosis(measles) :-
    chief_complaint(fever), chief_complaint(cough), % chief complaint
    symptom(sore_throat), symptom(skin_rash), symptom(dry_cough). % HPIs

% chest pain
% coughing blood
% shortness of breath
% coughing phlegm
% chronic fatigue
% sore throat
% skin rash
% dry cough


% headache & nausea_vomiting pairing

% rabies
% meningitis

diagnosis(rabies) :-
    chief_complaint(headache), chief_complaint(nausea_vomiting), % chief complaint
    symptom(salivation), symptom(paralysis), symptom(hyperactivity). % HPIs

diagnosis(meningitis) :-
    chief_complaint(headache), chief_complaint(nausea_vomiting), % chief complaint
    symptom(stiff_neck), symptom(seizure), symptom(sleepiness). % HPIs

% salivation
% paralysis
% hyperactivity
% stiff_neck
% seizure
% sleepiness

% diarrhea & nausea_vomiting pairing

% cholera
% hepatitis_A

diagnosis(cholera) :-
    chief_complaint(diarrhea), chief_complaint(nausea_vomiting), % chief complaint
    symptom(dehydration), symptom(restlessness), symptom(muscle_cramps). % HPIs

diagnosis(hepatitis_A) :-
    chief_complaint(diarrhea), chief_complaint(nausea_vomiting), % chief complaint
    symptom(jaundice), symptom(dark_urine), symptom(intense_itching). % HPIs

% dehydration
% restlessness
% muscle_cramps
% jaundice
% dark_urine
% intense_itching

consult :-
    write('MEDICAL DIAGNOSTIC CHATBOT'), nl, nl,
    write('SELECT YOUR CHIEF COMPLAINTS'), nl,
    write('1. Fever & Headache'), nl,
    write('2. Fever & Cough'), nl,
    write('3. Headache & Nausea/Vomiting'), nl,
    write('4. Diarrhea & Nausea/Vomiting'), nl,
    read(ChiefComplaint),
    (
        ChiefComplaint = 1 -> write('Fever & Headache'), nl;
        ChiefComplaint = 2 -> write('Fever & Cough'), nl;

        ChiefComplaint = 3 ->
        write('Chief Complaints: Headache & Nausea/Vomiting'), nl, nl,
        
        write('Does the patient profusely salivate?'), nl,
        read(Salivation),
        (
            Salivation = yes -> assert(symptom(salivation); true);
            Salivation = no -> assert(symptom(salivation); false)),
        ),

        write('Does the patient have paralysis?'), nl,
        read(Paralysis),
        (
            Paralysis = yes -> assert(symptom(paralysis); true);
            Paralysis = no -> assert(symptom(paralysis); false)),
        ),

        write('Does the patient have hyperactivity?'), nl,
        read(Hyperactivity),
        (
            Hyperactivity = yes -> assert(symptom(hyperactivity); true);
            Hyperactivity = no -> assert(symptom(hyperactivity); false)),
        ),

        write('Does the patient have stiff neck?'), nl,
        read(StiffNeck),
        (
            StiffNeck = yes -> assert(symptom(stiff_neck); true);
            StiffNeck = no -> assert(symptom(stiff_neck); false)),
        ),

        write('Does the patient have seizure?'), nl,
        read(Seizure),
        (
            Seizure = yes -> assert(symptom(seizure); true);
            Seizure = no -> assert(symptom(seizure); false)),
        ),

        write('Does the patient have sleepiness?'), nl,
        read(Sleepiness),
        (
            Sleepiness = yes -> assert(symptom(sleepiness); true);
            Sleepiness = no -> assert(symptom(sleepiness); false)),
        ),
                    
        diagnosis(rabies) -> write('Diagnosis: Rabies'), nl;
        diagnosis(meningitis) -> write('Diagnosis: Meningitis'), nl;
        diagnosis(_) -> write('No Diagnosis'), nl, !;

        ChiefComplaint = 4 -> write('Diarrhea & Nausea/Vomiting'), nl;

        write('Invalid input. Please try again.'), nl, consult, ! % backtracking
    ).

    
:- consult.