:- consult('helper.pl').

% main program
main :- 
    nl, write('---------------------------'),
    nl, write('MEDICAL DIAGNOSTIC CHATBOT'),
    nl, write('---------------------------'), nl, nl,

    ask_chief_complaint, 
    read(ChiefComplaint),
    (
        ChiefComplaint = 1 ->
            ask_symptoms_CC1,
            check_for_diagnoses,
            reset_chatbot, main;
        
        ChiefComplaint = 2 ->
            ask_symptoms_CC2,
            check_for_diagnoses,
            reset_chatbot, main;
            
        ChiefComplaint = 3 ->
            ask_symptoms_CC3,
            check_for_diagnoses,
            reset_chatbot, main;

        ChiefComplaint = 4 ->
            ask_symptoms_CC4,
            check_for_diagnoses,
            reset_chatbot, main;

        ChiefComplaint = 5 -> 
            nl, write('--------------------------------------------------------'), nl,
            nl, write('Thank you for consulting with ChatBot!'), nl,
            nl, write('--------------------------------------------------------'), halt;

        nl, write('--------------------------------------------------------'), nl,
        nl, write('Invalid input. Please try again.'), nl,
        nl, write('--------------------------------------------------------'), nl, main
    ).