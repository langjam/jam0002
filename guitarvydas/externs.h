// List
  /* $first (List) -> Datum; */
  /* $datum (List) -> Datum; */
  /* $reverse (List) -> List; */
  /* $rest (List) -> List; */
  /* $isEmpty (List) -> Boolean; */
  /* $newListCell (Datum) -> List; */
  /* $append (List, Cell) -> List; */
extern List listReverse (List);
extern List listNewCell (Datum);
extern List listAppend1 (List, Message);

// Component
  /* $newComponent (FunctionInit, FunctionReact) */
  /* $field (Component) -> any; */
  /* $appendInput (Component, Message); */
  /* $popInput (Component) -> Message; */
  /* $callReaction (Component, Message); */
  /* $hasInputs (Component) -> Boolean; */
  /* $isReady (Component) -> Boolean; */
extern Component componentNew (InitializationFunction, ReactionFunction);
extern void componentAppendInput (Component, Message*);
extern ListCell componentPopInput (Component);
extern void componentCallReaction (Component, Message*);
extern List componentGetOutputsAsSent (Component);

// Message
  /* $newMessagec (Datum) -> Message; */
  /* $dataComponent (Message) -> Component; */
  /* $dataConnection (Message) -> Component; */
extern Message messageNewc (char);

// ConnectionTable
  /* $connectedTo (Component) -> Component; */
  /* $connect (Component, Component); */
extern Component connectionsConnectedTo (Component);
extern void connect (Component, Component);

// Kernel
  /* $withLock (v) $block; */
  /* $sendc (Component, Message); */
  /* $panic (string); */
  /* $quit (); */
extern void kernelSendc (Component, unsigned char);
extern void kernelPanic (char*);
extern void kernelStart ();
extern void kernelStop ();

// Counter
/* $initializeCounter */
/* $decCounter */
/* $counterIsGreaterThanZero */

// Iterator Components
/* $beginWalkingComponentList */
/* $endWalkingComponentList */
/* $walkMoreComponents */
/* $nextComponentInList */
/* $advance */

// Iterator Outputs
/* $beginWalkingOutputsAsSent */
/* $endWalkingOutputsAsSent */
/* $walkMoreOutputs */
/* $nextOutput */
/* $advanceOutputs */
