    /* // mechanism List, ListCell */
    /* = "$datumMessage" Param     -- datumMessage */
    /* | "$datumpComponent" Param     -- datumpComponent */
    /* | "$datumpConnection" Param     -- datumpConnection */
    /* | "$newListCellComponent" Param  -- newListCellComponent */
    /* | "$append" TwoParams  -- append */
extern List* listNewCellComponent (Component*);
extern List* append (List*, List*);

    /* // mechanism Component */
    /* | "$newComponent" TwoParams -- newComponent */
    /* | "$isReady" Param -- isReady */
    /* | "$popInput" Param -- popInput */
    /* | "$callReaction" TwoParams    -- callReaction */
    /* | "$hasInputs" Param        -- hasInputs */
    /* | "$hasOutputs" Param        -- hasOutputs */
    /* | "$enqueueMessage" TwoParams  -- enqueueMessage */
    /* | "$enqueueOutput" TwoParams  -- enqueueOutput */
extern Component componentNew (InitializationFunction, ReactionFunction);
extern void componentCallReaction (Component*, Message);
extern void componentAppendInput (Component*, Message);
extern void componentAppendOutput (Component*, Message);
extern List* componentPopInput (Component*);

    /* // Component List iterator */
    /* | "$beginWalkingComponentList" Param -- beginWalkingComponentList */
    /* | "$endWalkingComponentList" Param -- endWalkingComponentList */
    /* | "$walkMoreComponents" Param -- walkingMoreComponents */
    /* | "$advance" Param -- advance */
    /* | "$nextComponentInList" Param -- nextComponentInList */

    /* // Component's output List iterator */
    /* | "$beginWalkingOutputsAsSent" TwoParams -- beginWalkingOutputsAsSent */
    /* | "$endWalkingOutputsAsSent" TwoParams -- endWalkingOutputsAsSent */
    /* | "$walkMoreOutputs" Param -- walkMoreOutputs */
    /* | "$nextOutput" Param -- nextOutput */
    /* | "$advanceOutputsAndGC" Param -- advanceOutputs */
extern List* componentGetOutputsAsSent (Component*);
extern void outputListAdvanceAndGC (List*);

// ConnectionTable
  /* $connectedTo (Component) -> Component; */
  /* $connect (Component, Component); */
extern Component connectionsConnectedTo (Component*);
extern void connectionsConnect (Component*, Component*);

// Kernel
  /* $withLock (v) $block; */
  /* $sendc (Component, Message); */
  /* $panic (string); */
  /* $quit (); */
extern void kernelSendc (Component*, char);
extern void kernelPanic (char*);
extern void kernelStart ();
extern void kernelStop ();

// Counter
/* $initializeCounter */
/* $decCounter */
/* $counterIsGreaterThanZero */

