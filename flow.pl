setup1(N, Pref, P) :-
% Normal setup process
    Temp is 6000 + P,
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,Temp,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(Pref, Temp, N).

setup11 :-
    roundInitialiser,
    roundWork.

/*setup2(N) :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6002,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(250, 6002, N),

setup21 :-
    roundInitialiser,
    roundWork.

setup3(N) :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6003,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(300, 6003, N),
    roundInitialiser.

setup31 :- 
    roundInitialiser,
    roundWork.

setup4(N) :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6004,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(350, 6004, N),
    roundInitialiser.

setup41 :- 
    roundInitialiser,
    roundWork.
*/