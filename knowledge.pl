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

% fever & cough pairing ~ tuberculosis, pneumonia, measles
disease(tuberculosis, SymptomList) :-
    member(fever, SymptomList), 
    member(cough, SymptomList),
    member(chest_pain, SymptomList),
    member(blood_cough, SymptomList),
    member(breath_shortness, SymptomList).

disease(pneumonia, SymptomList) :-
    member(fever, SymptomList), 
    member(cough, SymptomList),
    member(chest_pain, SymptomList),
    member(phlegm_cough, SymptomList),
    member(fatigue, SymptomList).

disease(measles, SymptomList) :-
    member(fever, SymptomList),
    member(cough, SymptomList),
    member(sore_throat, SymptomList),
    member(skin_rash, SymptomList),
    member(dry_cough, SymptomList).

% headache & nausea_vomiting pairing ~ rabies & meningitis
disease(rabies, SymptomList) :-
    member(headache, SymptomList),
    member(nausea_vomiting, SymptomList),
    member(salivation, SymptomList),
    member(paralysis, SymptomList),
    member(hyperactivity, SymptomList).

disease(meningitis, SymptomList) :-
    member(headache, SymptomList),
    member(nausea_vomiting, SymptomList),
    member(stiff_neck, SymptomList),
    member(seizure, SymptomList),
    member(sleepiness, SymptomList).

% diarrhea & nausea_vomiting pairing ~ hepatitis & cholera
disease(hepatitis_A, SymptomList) :-
    member(diarrhea, SymptomList),
    member(nausea_vomiting, SymptomList),
    member(jaundice, SymptomList),
    member(dark_urine, SymptomList),
    member(intense_itching, SymptomList).

disease(cholera, SymptomList) :-
    member(diarrhea, SymptomList),
    member(nausea_vomiting, SymptomList),
    member(dehydration, SymptomList),
    member(restlessness, SymptomList),
    member(muscle_cramps, SymptomList).