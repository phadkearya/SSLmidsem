:- dynamic waitForTimeout/2.
waitForTimeout :-
    // wait for timeout, that is, no message received from coordinator. Y contains the new preference as received from the coordinator.




:- dynamic waitForAcks/3.
waitForAcks :- 

:- dynamic waitForDecidePref/3.


:- terminateRound.

:- roundWork/0.
roundWork :- 

    round(R),
    n(N),
    coordinator(C),
    pID(P),
    timestamp(T),
    preference(X),
    countMessage(Count),
    countTotal(CountTotalAcks),
    countAck(CountAcks),
    rNew(Rnew),
    tNew(Tnew),
    xNew(Xnew),
    receivedPref(Rp).
    Temp is 0,

    sendMessage(C,R,X,T,P),

    (P =:= C -> 
    waitForCount(Count,N),
    broadcastMessage(R,X,N);
    ),  

    // intitial value of Timeout is False,
    waitForTimeout(Timeout,Y),

    (Timeout =:= False ->
    ( X =\= Y -> 
    retract(timestamp(_)),
    assert(timestamp(R)),
    T1 is R;
    true
    ),
    retract(preference(_)),
    assert(preference(Y)),
    X1 is Y,
    sendAck(C,R);
    sendNAck(C,R)
    ),

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










