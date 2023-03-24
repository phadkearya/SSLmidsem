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
    assertz(receivedBroadcast(0)).

% ---------------------------------------------------------------------------------------------------

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
    C1 is Y mod P,
    C is C1+6000,
    retract(coordinator(Co)),
    assertz(coordinator(C)),
    retract(countMessage(_)),
    assertz(countMessage(0)),
    retract(countTotalAcks(_)),
    assertz(countTotalAcks(0)),
    retract(countAck(_)),
    assertz(countAck(0)),

    % create_mutex(mutexForCount),

    writeln("round is "),
    writeln(Y),
    writeln("coordinator is "),
    writeln(C).

% ---------------------------------------------------------------------------------------------------



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
    writeln("platform number "),
    writeln(P),
    writeln("coordinator"),
    writeln(C2),
    sendMessage(C2,R,X,T,P),

    (P =\= C2 -> 
    % intitial value of Timeout is False,
    sleep(10),
    receivedBroadcast(B),
    (B =:= 1 ->
    % it has received broadcast
    sendAck(P,C2),
    writeln("broadcasr refceived");
    % coordinator has crashed
    sendNAck(P,C2),
    writeln("coordinator crash")
    );
    true
    ).

    /*

    (P =:= C -> 
    waitForAcks(CountTotalAcks, CountAcks, Temp),
    (CountAcks > CountTotalAcks/2 -> 
    decidePrefB;
    % terminate round? 
    );
    waitForDecidePref(),

    (Rp =/= X1 ->
    retract(preference),
    assertz(preference(Rp)),
    broadcastPref(Rp),
    % terminate round.
    ;
    true
    )
    ).

    */


% ----------------------------------------------------------------------------------------------------

% input : 
% output:
% desciption : just the handler, it increases count of received messages and compares all received messages for most recent timestamp and sets that as the preference.
agentM_handler(guid,(IP,Port),main):-
    payloadToC(guid,X1,Y,Z,PO),
    handlerFunction(X1,Y,Z,PO).

handlerFunction(X1,Y,Z,PO):-
    countMessage(M),
    V is M+1,
    writeln("current V is "),
    writeln(V),
    retract(countMessage(_)),
    assert(countMessage(V)),
    writeln(X1),
    writeln(Y),
    writeln(Z),
    tNew(Tnew),
    preference(Xe),
    writeln("current preference is "),
    writeln(Xe),
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
    writeln("changed preference is "),
    writeln(X11),
    writeln("almost there"),
    coordinator(C3),
    pID(P3),
    writeln("current V is "),
    writeln(V),
    agent_list,
    n(N),
    writeln("V is "),
    writeln(V),
    writeln("N is "),
    writeln(N),
    K is N/2,
    % now if V is more than N/2 call another function to continue execution, or dont do anything.
    (V > K ->
    xNew(X11),
    writeln("New preference is "),
    writeln(X11),
    retract(countMessage(_)),
    assertz(countMessage(-10)),
    writeln("calling broadcast"),
    broadcastMessage(R,X11,N,P3);
    true
    ).

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
    /*writeln("creating static agent"),
    create_static_agent(agentM,(localhost,P),agentM_handler,[1]),
    writeln("executing static agent"),
    % execute agent handler
    % HERE EXECUTE AGENT IS NOT WORKING!!!!
    execute_agent(agentM,(localhost,P),agentM_handler),*/
    handlerFunction(R,X,T,P),
    writeln("am the coordinator itself")
    ).


% ----------------------------------------------------------------------------------------------------

% waits until coordinator has received n/2 messages
waitForCount(Count,N) :- 
    loop(Count,N).

% loop till count exceeds n/2
loop(Count,N):-
    (Count > N/2 ->
    writeln("done"),
    true;
    writeln(Count),
    loop(Count1,N)
    ).

% -----------------------------------------------------------------------------------------------------


% input :
% output: 
% description: handler of the payload which is the message which is sent by coordinator to all other processes.
agentR_handler(guid,(IP,Port),main):-
    writeln("Response has reached process "),
    writeln(Port),
    payloadFromC(guid,Rincoming,Xincoming),
    writeln("incoming preference is"),
    writeln(Xincoming),
    retract(receivedBroadcast(_)),
    assertz(receivedBroadcast(1)).
    

% input : 
% output : 
% desciption : loops through all the available processes and sends them a payload containing new value of preference.
:- dynamic loop/5.
loop(I,N,R,X,P):-
    I > N;
    Tt is I+6000,
    (Tt =:= P ->
    true;
    create_mobile_agent(agentR,(localhost,P),agentR_handler,[1]),
    assert(payloadFromC(guid,R,X)),
    add_payload(agentR,[(payloadFromC,3)]),
    move_agent(agentR,(localhost,Tt)),
    writeln("broadcasted to process "),
    writeln(Tt)
    ),
    Ii is I+1,
    loop(Ii,N,R,X,P).

% input :
% output: 
% description: sends a message with r and preference to all processes using mobile agent and payloads.
broadcastMessage(R,X,N,P) :-
    writeln("beginning broadcast"),
    % for i in all platforms: 
        % make a dynamic agent with payload (r,x) and send it to platform i.
    I is 1,
    loop(I,N,R,X,P),
    writeln("broadcast done").


% -------------------------------------------------------------------------------------------------------

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
    payloadAck(guid),
    remove_payload(agentA,[(payloadAck,1)]),
    writeln("total acks"),
    writeln(V),
    writeln("positive acks"),
    writeln(W),
    n(N),
    round(R),
    pID(P),
    preference(X),
    ( W > N/2 ->
    % call decide pref broadcast function
    broadcastDecidePref(R,X,N,P);
    true
    ).

% input :
% output: 
% description: makes a mobile agent which sends positive ack to coordinator.
sendAck(P,C) :- 
    % make a dynamic agent with payload (+1) and send to C and its handler will keep countTotalAcks and countAck and increment it.
    create_mobile_agent(agentA,(localhost,P),agentA_handler,[1]),
    assert(payloadAck(guid)),
    add_payload(agentA,[(payloadAck,1)]),
    move_agent(agentA,(localhost,C)).

% -----------------------------------------------------------------------------------------------------------------


% input :
% output: 
% description: handler of payload payloadAcks which increments countTotalAcks and countAck-number of positive acks received by coordinator.
agentN_handler(guid,(IP,Port),main):-
    countTotalAcks(Ta),
    V is Ta+1,
    retract(countTotalAcks(_)),
    assert(countTotalAcks(V)),
    payloadAck(guid),
    remove_payload(agentA,[(payloadAck,1)]),
    writeln("total acks"),
    writeln(V).

sendNAck(P,C) :-
    % make a dynamic agent with payload (-1) and send to C and its handler will keep countTotal and increment it.
    create_mobile_agent(agentN,(localhost,P),agentN_handler,[1]),
    assert(payloadNAck(guid)),
    add_payload(agentN,[(payloadNAck,1)]),
    move_agent(agentN,(localhost,C)).

% ------------------------------------------------------------------------------------------------------------------

/*
:- dynamic broadcastPref/3.
:- dynamic payloadNewPref/2.
:- dynamic agentPR_handler/3.

% input :
% output: 
% description: handler of the payload which is the new preference which is broadcast.
agentPR_handler(guid,(IP,Port),main):-
    writeln("Boadcast preference has reached process "),
    writeln(Port).

% input : 
% output : 
% desciption : loops through all the available processes and sends them a payload containing new value of preference.
:- dynamic loopNew/5.
loopNew(I,N,R,Rp,P):-
    I > N;
    Tt is I+6000,
    (Tt =:= P ->
    true;
    create_mobile_agent(agentPR,(localhost,P),agentPR_handler,[1]),
    assert(payloadNewPref(guid,Rp)),
    add_payload(agentPR,[(payloadNewPref,2)]),
    move_agent(agentPR,(localhost,Tt)),
    writeln("broadcasted to process "),
    writeln(Tt)
    ),
    Ii is I+1,
    loopNew(Ii,N,R,Rp,P).

% input :
% output: 
% description: sends a message with r and preference to all processes using mobile agent and payloads.
broadcastPref(R,Rp,N,P) :-
    writeln("beginning broadcast"),
    % for i in all platforms: 
        % make a dynamic agent with payload (rp) and send it to platform i.
    I is 0,
    loopNew(I,N,R,Rp,P),
    writeln("broadcast done").
*/

% -------------------------------------------------------------------------------------------------


% input :
% output: 
% description: handler of the payload which is the new preference which is broadcast.
agentPR_handler(guid,(IP,Port),main):-
    writeln("Boadcast preference has reached process "),
    writeln(Port).

% input : 
% output : 
% desciption : loops through all the available processes and sends them a payload containing new value of preference.
loopNew(I,N,R,X,P):-
    I > N;
    Tt is I+6000,
    (Tt =:= P ->
    true;
    create_mobile_agent(agentPR,(localhost,P),agentPR_handler,[1]),
    assert(payloadNewPref(guid,X)),
    add_payload(agentPR,[(payloadNewPref,2)]),
    move_agent(agentPR,(localhost,Tt)),
    writeln("broadcasted new preference to process "),
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
    I is 0,
    loopNew(I,N,R,X,P),
    writeln("broadcast of new preference done").