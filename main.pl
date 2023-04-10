:- consult('helper.pl').

% main program
consult :- 
    nl, write('---------------------------'),
    nl, write('MEDICAL DIAGNOSTIC CHATBOT'),
    nl, write('---------------------------'), nl, nl,

    ask_chief_complaint, 
    read(ChiefComplaint),
    (
        ChiefComplaint = 1 ->
            ask_symptoms_CC1,
            check_for_diagnoses,
            reset_chatbot, consult;
        
        ChiefComplaint = 2 ->
            ask_symptoms_CC2,
            check_for_diagnoses,
            reset_chatbot, consult;
            
        ChiefComplaint = 3 ->
            ask_symptoms_CC3,
            check_for_diagnoses,
            reset_chatbot, consult;

        ChiefComplaint = 4 ->
            ask_symptoms_CC4,
            check_for_diagnoses,
            reset_chatbot, consult;

        ChiefComplaint = 5 -> 
            nl, write('--------------------------------------------------------'), nl,
            nl, write('Thank you for consulting with ChatBot!'), nl,
            nl, write('--------------------------------------------------------'), halt;

        nl, write('--------------------------------------------------------'), nl,
        nl, write('Invalid input. Please try again.'), nl,
        nl, write('--------------------------------------------------------'), nl, consult
    ).

:- consult.
