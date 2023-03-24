setup1 :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6001,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(200, 6001, 3),
    roundInitialiser.

setup11 :-
    roundWork.
    % sendMessage(6001,1,2345,1,6000).
    % sendAck(6000,6001).

setup2 :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6002,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(250, 6002, 3),
    roundInitialiser.

setup21 :-
    roundWork.
    % sendMessage(6001,1,2345,1,6000).
    % sendAck(6000,6001).

setup3 :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6003,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(300, 6003, 3),
    roundInitialiser.

setup31 :- 
    roundWork.
    % sendMessage(6001,1,2345,1,6000).
    % sendAck(6000,6001).

