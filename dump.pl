:- dynamic symptom/1. % to make predicates dynamic, subject to assertion ~ true or false
:- dynamic symptom_list/1.

symptom_list([]).

add_symptom(Symptom) :-
    symptom_list(Symptoms), % Retrieve current symptom list
    retract(symptom_list(Symptoms)), % Remove current symptom list from database
    assertz(symptom_list([Symptom|Symptoms])). % Add new symptom to the list and assert it

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
symptom(pleghm_cough).
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

% diagnosis(dengue) :- 
%     chief_complaint(fever), chief_complaint(headache), % chief complaint
%     symptom(joint_muscle_pain), symptom(dehydration), symptom(skin_rash). % HPIs

% diagnosis(malaria) :-
%     chief_complaint(fever), chief_complaint(headache), % chief complaint
%     symptom(pale_skin), symptom(rapid_heart_rate), symptom(breath_shortness). % HPIs

% diagnosis(typhoid_fever) :-
%     chief_complaint(fever), chief_complaint(headache), % chief complaint
%     symptom(stomach_pain), (symptom(diarrhea) ; symptom(constipation)), symptom(skin_rash). % HPIs

% fever & cough pairing

% tuberculosis
% pneumonia
% measles

% diagnosis(tuberculosis) :-
%     chief_complaint(fever), chief_complaint(cough), % chief complaint
%     symptom(chest_pain), symptom(blood_cough), symptom(breath_shortness). % HPIs

% diagnosis(pneumonia) :-
%     chief_complaint(fever), chief_complaint(cough), % chief complaint
%     symptom(chest_pain), symptom(pleghm_cough), symptom(fatigue). % HPIs

% diagnosis(measles) :-
%     chief_complaint(fever), chief_complaint(cough), % chief complaint
%     symptom(sore_throat), symptom(skin_rash), symptom(dry_cough). % HPIs

% headache & nausea_vomiting pairing

% rabies
% meningitis

% diagnosis(rabies) :-
%     chief_complaint(headache), chief_complaint(nausea_vomiting), % chief complaint
%     symptom(salivation), symptom(paralysis), symptom(hyperactivity). % HPIs

diagnosis(rabies, [headache, nausea_vomiting, salivation, paralysis, hyperactivity]).

% diagnosis(meningitis) :-
%     chief_complaint(headache), chief_complaint(nausea_vomiting), % chief complaint
%     symptom(stiff_neck), symptom(seizure), symptom(sleepiness). % HPIs

% salivation
% paralysis
% hyperactivity
% stiff_neck
% seizure
% sleepiness

% diarrhea & nausea_vomiting pairing

% cholera
% hepatitis_A

% diagnosis(cholera) :-
%     chief_complaint(diarrhea), chief_complaint(nausea_vomiting), % chief complaint
%     symptom(dehydration), symptom(restlessness), symptom(muscle_cramps). % HPIs

% diagnosis(hepatitis_A) :-
%     chief_complaint(diarrhea), chief_complaint(nausea_vomiting), % chief complaint
%     symptom(jaundice), symptom(dark_urine), symptom(intense_itching). % HPIs

consult :-
    write('MEDICAL DIAGNOSTIC CHATBOT'), nl, nl,
    write('SELECT YOUR CHIEF COMPLAINTS'), nl,
    % write('1. Fever & Headache'), nl,
    % write('2. Fever & Cough'), nl,
    write('3. Headache & Nausea/Vomiting'), nl,
    % write('4. Diarrhea & Nausea/Vomiting'), nl.
    read(ChiefComplaint),
    (
        ChiefComplaint = 3 ->
            
        write('Invalid input. Please try again.'), nl, consult, !
    ).