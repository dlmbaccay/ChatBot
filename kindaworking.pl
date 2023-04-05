:- dynamic symptom/1.
:- dynamic symptom_list/1.

symptom_list([]).

% Append an item to the list
append_symptom(Symptom) :-
    assertz(symptom_list([Symptom|T])),
    retract(symptom_list(T)).

chief_complaint(fever).
chief_complaint(cough).
chief_complaint(headache).
chief_complaint(nausea_vomiting).
chief_complaint(diarrhea).

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

diagnosis(rabies, [headache, nausea_vomiting, salivation, paralysis, hyperactivity]).

consult :-
    write('MEDICAL DIAGNOSTIC CHATBOT'), nl, nl,
    write('SELECT YOUR CHIEF COMPLAINTS'), nl,
    read(ChiefComplaint),
    (
        ChiefComplaint = 3 ->
            write('Do you have salivation, paralysis, hyperactivity, stiff neck, seizure, and sleepiness (y/n)?'), nl,
            read(Response),
            (
                Response = 'y' ->
                    append_symptom(salivation),
                    append_symptom(paralysis),
                    append_symptom(hyperactivity),
        
                    diagnosis(rabies, Symptoms) ->
                        write('Diagnose rabies.'), nl, !;
                    write('No diagnosis.'), nl, !
            )
        ;
        write('Invalid input. Please try again.'), nl, consult, !
    ).
