% N is the number of processes, Pref is the starting preference of the process and P is the processID of the process.

setup(N, Pref, P) :-
% Normal setup process
    Temp is 6000 + P,
    consult('Desktop/sem6/SSL/Assignment4/platform-ubuntu.pl'),
    start_tartarus(localhost,Temp,1),
    consult('Desktop/sem6/SSL/MidSem/run.pl'),
    startSession(Pref, Temp, N).

% setupNext simply executed the predicates defined in run.pl
setupNext :-
    roundInitialiser,
    roundWork.

