% DECLARATIONS OF DIFFERENT PREDICATES

:- dynamic pID/1.   
:- dynamic preference/1.
:- dynamic round/1.
:- dynamic timestamp/1.
:- dynamic coordinator/1.
:- dynamic n/1.
:- dynamic countTotalAcks/1.
:- dynamic countMessage/1.
:- dynamic countAck/1.
:- dynamic rNew/1.
:- dynamic xNew/1.
:- dynamic tNew/1.
:- dynamic receivedPref/1.
:- dynamic receivedBroadcast/1.
:- dynamic receivedNextBroadcast/1.
:- dynamic receivedDecidePref/1.

:- dynamic startSession/3.
:- dynamic roundInitialiser/0.
:- dynamic roundWork/0.

:- dynamic payloadToC/5.
:- dynamic agentM_handler/3.
:- dynamic handlerFunction/4.
:- dynamic sendMessage/5.

:- dynamic waitForCount/2.
:- dynamic loop/2.

:- dynamic payloadFromC/3.
:- dynamic agentR_handler/3.
:- dynamic broadcastMessage/4.

:- dynamic agentA_handler/3.
:- dynamic payloadAck/1.
:- dynamic sendAck/2.

:- dynamic agentN_handler/3.
:- dynamic payloadNAck/1.
:- dynamic sendNack/2.

:- dynamic broadcastDecidePref/4.
:- dynamic payloadNewPref/2.
:- dynamic agentPR_handler/3.
:- dynamic loopNew/5.

% ---------------------------------------------------------------------------------------------------

% THIS IS THE INITIALISATION PREDICATE, WHICH INSTANTIATES CERTAIN VALUES OF THE PLATFORM
% The main things any process has are preference, processID and timestamp. All other things are just for implementation of the algorithm.

% input: preference, pid, number of processes. 
% output: nothing
% global change : sets all parameters of the process.
startSession(Preference, PID, N) :-
    assertz(preference(Preference)),
    assertz(pID(PID)),
    assertz(round(0)),
    assertz(timestamp(0)),
    assertz(coordinator(0)),
    assertz(countMessage(0)),
    assertz(countTotalAcks(0)),
    assertz(countAck(0)),
    assertz(n(N)),
    assertz(rNew(0)),
    assertz(tNew(0)),
    assertz(xNew(Preference)),
    assertz(receivedPref(0)),
    assertz(receivedBroadcast(0)),
    assertz(receivedDecidePref(0)),
    assertz(receivedNextBroadcast(0)).

% ---------------------------------------------------------------------------------------------------

% roundInitialiser SETS SOME VALUES FOR THAT PARTICULAR ROUND

% input : 
% output:
% global changes: sets the round number and the coordinator and resets the counts to 0.
roundInitialiser :-
    round(X),
    coordinator(Co),
    n(P),
    Y is X+1,
    retract(round(_)),
    assertz(round(Y)),
    C1 is Y,
    C is C1+6000,
    retract(coordinator(Co)),
    assertz(coordinator(C)),
    retract(countMessage(_)),
    assertz(countMessage(0)),
    retract(countTotalAcks(_)),
    assertz(countTotalAcks(0)),
    retract(countAck(_)),
    assertz(countAck(0)),

    preference(Temporary),
    retract(xNew(_)),
    assertz(xNew(Temporary)),

    retract(receivedPref(_)),
    assertz(receivedPref(0)),
    retract(receivedBroadcast(_)),
    assertz(receivedBroadcast(0)),
    retract(receivedDecidePref(_)),
    assertz(receivedDecidePref(0)),

    % receivedNextBroadcast is a predicate which is used in synchronization of the rounds.

    receivedNextBroadcast(Nb),
    (Nb =:= 1 ->
    retract(receivedNextBroadcast(_)),
    assertz(receivedNextBroadcast(0)),
    retract(receivedBroadcast(_)),
    assertz(receivedBroadcast(1));
    true
    ),

    writeln("round is "),
    writeln(Y),
    writeln("coordinator is "),
    writeln(C).

% ---------------------------------------------------------------------------------------------------

% THIS IS THE MAIN DRIVER FUNCTION OF THE ALGORITHM.

roundWork :- 

    round(R),
    n(N),
    coordinator(C2),
    pID(P),
    timestamp(T),
    preference(X),
    countMessage(Count),
    countTotalAcks(CountTotalAcks),
    countAck(CountAcks),
    rNew(Rnew),
    tNew(Tnew),
    xNew(Xnew),
    receivedPref(Rp),
    /*writeln("platform number "),
    writeln(P),
    writeln("coordinator"),
    writeln(C2),*/

    % sending initial message of preference to coordinator

    sendMessage(C2,R,X,T,P),

    (P =\= C2 -> 
    % intitial value of Timeout is False,

    % waiting for broadcast message and sending ACK or NACK appropriately

    sleep(10),
    receivedBroadcast(B),
    (B =:= 1 ->
    % it has received broadcast
    sendAck(P,C2),
    writeln("broadcast received");
    % coordinator has crashed
    sendNAck(P,C2),
    writeln("coordinator crash")
    );
    true
    ),

    (P =\= C2 -> 
    % intitial value of Timeout is False,
    % waiting to receive deicde preference value, if received, broadcasting and terminating or else entering next round.
    sleep(20),
    receivedDecidePref(Dp),
    preference(Pfinal),
    (Dp =:= 1 ->
    % it has received decide preference
    writeln("decide preference received"),
    broadcastDecidePref(R,Pfinal,N,P),
    writeln("Final broadcast of round done"),
    writeln("Final preference found"),
    writeln("Done");
    % it has not received decide preference
    writeln("no decide preference received"),
    writeln("Continuing to next round"),
    setup11
    );
    true
    ),
    writeln("Round done").

% ----------------------------------------------------------------------------------------------------

% THESE PREDICATES BELOW HANDLE THE SENDING OF THE INITIAL MESSAGE TO THE COORDINATOR FROM EVERY PROCESS.

% input : 
% output:
% desciption : just the handler, it increases count of received messages and compares all received messages for most recent timestamp and sets that as the preference.
agentM_handler(guid,(IP,Port),main):-
    payloadToC(guid,X1,Y,Z,PO),
    handlerFunction(X1,Y,Z,PO).

% This handler function gets executed when message with preference and timestamp goes to coordinator.

handlerFunction(X1,Y,Z,PO):-
    countMessage(M),
    V is M+1,
    % writeln("current V is "),
    % writeln(V),
    retract(countMessage(_)),
    assert(countMessage(V)),
    /*writeln(X1),
    writeln(Y),
    writeln(Z),*/
    tNew(Tnew),
    preference(Xe),
    % writeln("current preference is "),
    % writeln(Xe),

    % deciding new preference on the basis of timestamp values

    (Z > Tnew -> 
    retract(rNew(_)),
    assertz(rNew(X1)),
    retract(tNew(_)),
    assertz(tNew(Z)),
    retract(xNew(_)),
    assertz(xNew(Y)),
    retract(preference(_)),
    assertz(preference(Y));
    true
    ),
    xNew(X11),
    % writeln("changed preference is "),
    % writeln(X11),
    coordinator(C3),
    pID(P3),
    n(N),
    round(R),
    K1 is N - R +1,
    K is K1/2,
    % now if V is more than N/2 call another function to continue execution, or dont do anything.

    % broadcasting decided preference after N/2 preference values have reached the coordinator.

    (V > K ->
    xNew(X11),
    retract(preference(_)),
    assertz(preference(X11)),
    % writeln("New preference is "),
    % writeln(X11),
    retract(countMessage(_)),
    assertz(countMessage(-10)),
    % writeln("calling broadcast"),
    (R =:= 1 ->
    true;
    broadcastMessage(R,X11,N,P3)
    );
    true
    ).

% Sending message to coordinator

% input :
% output: 
% description: sends a message with r, preference and timestamp to the coordinator using mobile agent and payloads.
sendMessage(C,R,X,T,P) :- 
    (P =\= C ->
    atom_concat("round", R, Temp1),
    atom_concat(Temp1, "port", Temp2),
    atom_concat(Temp2, P, FinalName),
    create_mobile_agent(FinalName,(localhost,P),agentM_handler,[1]),
    writeln("sending preference "),
    writeln(X),
    assert(payloadToC(guid,R,X,T,P)),
    add_payload(FinalName,[(payloadToC,5)]),
    move_agent(FinalName,(localhost,C)),
    writeln("sent message to coordinator");
    handlerFunction(R,X,T,P),
    writeln("am the coordinator itself")
    ).


% -----------------------------------------------------------------------------------------------------

% THESE PREDICATES BELOW ARE TO SEND THE BROADCAST FROM THE COORDINATOR TO ALL THE PROCESSES


% input :
% output: 
% description: handler of the payload which is the message which is sent by coordinator to all other processes.
agentR_handler(guid,(IP,Port),main):-
    writeln("Response has reached process "),
    writeln(Port),
    payloadFromC(guid,Rincoming,Xincoming),
    writeln("incoming preference is"),
    writeln(Xincoming),
    retract(preference(_)),
    assertz(preference(Xincoming)),
    round(R),
    % write("Rincoming: "),writeln(Rincoming),
    % write("R: "), writeln(R),
    (Rincoming =:= R ->
    retract(receivedBroadcast(_)),
    assertz(receivedBroadcast(1)),
    % writeln("same round");
    retract(receivedNextBroadcast(_)),
    assertz(receivedNextBroadcast(1)),
    % writeln("next round")
    ),
    % writeln("why").
    

% input : 
% output : 
% desciption : loops through all the available processes and sends them a payload containing new value of preference.
:- dynamic loop/5.
loop(I,N,R,X,P):-
    I > N;
    Tt is I+6000,   
    (Tt =:= P ->
    true;
    atom_concat("round", R, Temp1),
    atom_concat(Temp1, "port", Temp2),
    atom_concat(Temp2, P, FinalName),
    atom_concat(FinalName, "Broadcast", FinalName2),
    create_mobile_agent(FinalName2,(localhost,P),agentR_handler,[1]),
    assert(payloadFromC(guid,R,X)),
    add_payload(FinalName2,[(payloadFromC,3)]),
    move_agent(FinalName2,(localhost,Tt)),
    writeln("broadcasted to process "),
    writeln(Tt)
    ),
    Ii is I+1,
    loop(Ii,N,R,X,P).

% Broadcasting message to all processes

% input :
% output: 
% description: sends a message with r and preference to all processes using mobile agent and payloads.
broadcastMessage(R,X,N,P) :-
    writeln("beginning broadcast"),
    % for i in all platforms: 
        % make a dynamic agent with payload (r,x) and send it to platform i.
    Cur is P - 6000,
    I is Cur,
    loop(I,N,R,X,P),
    writeln("broadcast done").


% -------------------------------------------------------------------------------------------------------

% THESE PREDICATES BELOW ARE USED TO SEND THE ACKS FROM DIFFERENT PROCESSES TO THE COORDINATOR

% input :
% output: 
% description: handler of payload payloadAcks which increments countTotalAcks and countAck-number of positive acks received by coordinator.
agentA_handler(guid,(IP,Port),main):-
    countTotalAcks(Ta),
    V is Ta+1,
    retract(countTotalAcks(_)),
    assert(countTotalAcks(V)),
    countAck(Tb),
    W is Tb+1,
    retract(countAck(_)),
    assert(countAck(W)),
    writeln("total acks"),
    writeln(V),
    writeln("positive acks"),
    writeln(W),
    n(N),
    round(R),
    pID(P),
    preference(X),
    T1 is N - R + 1,
    ( W >= T1/2 ->
    % call decide pref broadcast function
    broadcastDecidePref(R,X,N,P);
    true
    ).

% input :
% output: 
% description: makes a mobile agent which sends positive ack to coordinator.
sendAck(P,C) :- 
    % make a dynamic agent with payload (+1) and send to C and its handler will keep countTotalAcks and countAck and increment it.
    round(R),
    atom_concat("round", R, Temp1),
    atom_concat(Temp1, "port", Temp2),
    atom_concat(Temp2, P, FinalName),
    atom_concat(FinalName, "Ack", FinalName2),
    create_mobile_agent(FinalName2,(localhost,P),agentA_handler,[1]),
    assert(payloadAck(guid)),
    add_payload(FinalName2,[(payloadAck,1)]),
    move_agent(FinalName2,(localhost,C)).

% -----------------------------------------------------------------------------------------------------------------

% THESE PREDICATES BELOW ARE USED TO SEND NACKS FROM DIFFERENT PROCESSES TO THE COORDINATOR


% input :
% output: 
% description: handler of payload payloadAcks which increments countTotalAcks and countAck-number of positive acks received by coordinator.
agentN_handler(guid,(IP,Port),main):-
    countTotalAcks(Ta),
    V is Ta+1,
    retract(countTotalAcks(_)),
    assert(countTotalAcks(V)),
    writeln("total acks"),
    writeln(V).

sendNAck(P,C) :-
    % make a dynamic agent with payload (-1) and send to C and its handler will keep countTotal and increment it.
    round(R),
    atom_concat("round", R, Temp1),
    atom_concat(Temp1, "port", Temp2),
    atom_concat(Temp2, P, FinalName),
    atom_concat(FinalName, "Nack", FinalName2),
    create_mobile_agent(FinalName2,(localhost,P),agentN_handler,[1]),
    assert(payloadNAck(guid)),
    add_payload(FinalName2,[(payloadNAck,1)]),
    move_agent(FinalName2,(localhost,C)).

% -------------------------------------------------------------------------------------------------

% THESE PREDICATES BELOW ARE USED TO BROADCAST DECIDE PREFERENCE DIFFERENT PROCESSES BY THE COORDINATOR

% input :
% output: 
% description: handler of the payload which is the new preference which is broadcast.
agentPR_handler(guid,(IP,Port),main):-
    writeln("Boadcast decide preference has reached process "),
    writeln(Port),
    payloadNewPref(guid,X),
    retract(receivedDecidePref(_)),
    assertz(receivedDecidePref(1)),
    retract(preference(_)),
    assertz(preference(X)),
    writeln("new preference is "),
    writeln(X).

% input : 
% output : 
% desciption : loops through all the available processes and sends them a payload containing new value of preference.
loopNew(I,N,R,X,P):-
    I > N;
    Tt is I+6000,
    (Tt =:= P ->
    true;
    atom_concat("round", R, Temp1),
    atom_concat(Temp1, "port", Temp2),
    atom_concat(Temp2, P, FinalName),
    atom_concat(FinalName, "DecideBroadcast", FinalName2),
    create_mobile_agent(FinalName2,(localhost,P),agentPR_handler,[1]),
    assert(payloadNewPref(guid,X)),
    add_payload(FinalName2,[(payloadNewPref,2)]),
    move_agent(FinalName2,(localhost,Tt)),
    writeln("broadcasted new decide preference to process "),
    writeln(Tt)
    ),
    Ii is I+1,
    loopNew(Ii,N,R,X,P).

% input :
% output: 
% description: sends a message with r and preference to all processes using mobile agent and payloads.
broadcastDecidePref(R,X,N,P) :-
    writeln("beginning broadcast of new preference"),
    % for i in all platforms: 
        % make a dynamic agent with payload (x) and send it to platform i.
    Cur is P - 6000,
    I is Cur,
    loopNew(I,N,R,X,P),
    writeln("broadcast of new decide preference done").