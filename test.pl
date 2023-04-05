:- dynamic symptom/2.

% dengue and tuberculosis symptoms
symptom(dengue, fever).
symptom(dengue, headache).
symptom(dengue, muscle_and_joint_pain).
symptom(dengue, nausea).

symptom(tuberculosis, fever).
symptom(tuberculosis, cough).
symptom(tuberculosis, weight_loss).
symptom(tuberculosis, chest_pain).

diagnosis(dengue) :-
    symptom(dengue, fever), % chief complaint
    symptom(dengue, headache),
    symptom(dengue, muscle_and_joint_pain),
    symptom(dengue, nausea).

diagnosis(tuberculosis) :-
    symptom(tuberculosis, cough), % chief complaint
    symptom(tuberculosis, fever),
    symptom(tuberculosis, weight_loss),
    symptom(tuberculosis, chest_pain).

consult :- 
    write('SELECT A CHIEF COMPLAINT'), nl,
    write('1 - Fever'), nl,
    write('2 - Cough'), nl,
    read(Answer),
    (
        Answer = 1 -> 
            write('Do you have severe headache?'),
            read(Input), (Input = yes, assert(symptom(dengue, headache)); true),
            write('Do you have muscle and joint pain?'),
            read(Input), (Input = yes, assert(symptom(dengue, muscle_and_joint_pain)); true),
            write('Do you have nausea?'),
            read(Input), (Input = yes, assert(symptom(dengue, nausea)); true),
            diagnosis(dengue) -> write('You have dengue.'), halt; % wrong

        Answer = 2 -> 
            write('Do you have fever?'),
            read(Input), (Input = yes, assert(symptom(tuberculosis, fever)); true),
            write('Do you have chest pain?'),
            read(Input), (Input = yes, assert(symptom(tuberculosis, chest_pain)); true),
            write('Do you have weight loss?'),
            read(Input), (Input = yes, assert(symptom(tuberculosis, weight_loss)); true),
            diagnosis(tuberculosis) -> write('You have tuberculosis.'), halt; % wrong

        write('Invalid input. Please try again.'), nl, consult, !
    ).

% start the chatbot
:- consult.