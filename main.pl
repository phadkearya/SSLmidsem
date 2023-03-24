setup1 :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6000,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(20, 6000, 3),
    roundInitialiser.
    sendMessage(6001,1,2345,1,6000).
    % sendAck(6000,6001).
    
setup2 :-
% Normal setup process
% this is the coordinator for this round
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6001,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(25, 6001, 3),
    roundInitialiser.
    % broadcastMessage(1,1000,2,6001).

setup3 :-
% Normal setup process
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,6002,1),
    consult('Desktop/sem6/SSL/MidSem/agent.pl'),
    startSession(30, 6002, 3),
    roundInitialiser.
    sendMessage(6001,1,2345,1,6002).
    % writeln("message done"),
    % sendNAck(6002,6001),
    % writeln("Nack done").





