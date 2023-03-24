setup1(N) :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6001,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(200, 6001, N),
    roundInitialiser.

setup11 :-
    roundWork.

setup2(N) :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6002,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(250, 6002, N),
    roundInitialiser.

setup21 :-
    roundWork.

setup3(N) :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6003,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(300, 6003, N),
    roundInitialiser.

setup31 :- 
    roundWork.

setup4(N) :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6004,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(350, 6004, N),
    roundInitialiser.

setup41 :- 
    roundWork.
