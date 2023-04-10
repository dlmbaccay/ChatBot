# **MEDICAL DIAGNOSTIC CHATBOT**
---

The **Medical Diagnostic Chatbot** is a backward-chaining expert system Prolog program that aims to diagnose the user's medical condition based on the symptoms they are experiencing. The program utilizes a decision tree approach to narrow down the possible diseases that the user may have. Backward chaining is a type of inference mechanism used in the program, where the system starts with a goal and works backward through a set of rules to determine which symptoms are present and which diseases are likely causing those symptoms.

The program is divided into three .pl files: main.pl, helper.pl, and knowledge.pl. main.pl contains the main program, helper.pl contains helper functions, and knowledge.pl contains the knowledge base of the program.

The program is designed to provide a quick and easy preliminary assessment of a user's medical condition. However, it should not be used as a substitute for professional medical advice. The user is encouraged to seek medical attention from a qualified healthcare professional for a proper diagnosis and treatment plan.

---

### HOW TO RUN
1. Download and install SWI Prolog from the official website: https://www.swi-prolog.org/Download.html

2. Clone or download your Prolog program files (main.pl, helper.pl, and knowledge.pl) to your local machine.

3. Open your terminal application (Command Prompt on Windows, Terminal on macOS/Linux).

4. Navigate to the directory where your Prolog program files are located using the cd command. For example, if your files are in a folder called my_program, you can navigate to it by typing: cd /path/to/my_program

5. Once you are in the correct directory, start SWI Prolog by typing swipl in the terminal and pressing Enter.

6. Load your program files into SWI Prolog by typing [main,helper,knowledge]. and pressing Enter. This will load all three files into the interpreter.

7. Once your program is loaded, you can start using it. Follow the instructions or prompts in your program to interact with it.

---

### HOW TO USE
1. Choose the chief complaint that best matches your symptoms from the list of four provided.

2. You will be asked a series of questions about your symptoms. Answer with '**y.**' for yes and '**n.**' for no.

3. After answering all the questions, the Medical Diagnostic ChatBot will assess your symptoms and provide you with a list of possible diseases that match your symptoms.
---

### In partial fulfillment of the course CSINTSY - BS Computer Science - De La Salle University.
### Developers:
* BACCAY, Dominic Luis M.
* DIGNO, Kenneth Clark M.
* GUTIERREZ, Mark Daniel C.

---