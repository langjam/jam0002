// List
  /* $first (List) -> Datum; */
  /* $reverse (List) -> List; */
  /* $rest (List) -> List; */
  /* $isEmpty (List) -> Boolean; */
  /* $newListCell (Datum) -> List; */
  /* $append (List, Cell) -> List; */
extern List listReverse (List);
extern List listNewCell (Datum);
extern List listAppend1 (List, List);

// Component
  /* $newComponent (FunctionInit, FunctionReact) */
  /* $field (Component) -> any; */
  /* $appendInput (Component, Message); */
  /* $popInput (Component) -> Message; */
  /* $callReaction (Component, Message); */
  /* $hasInputs (Component) -> Boolean; */
extern Component componentNew (InitializationFunction, ReactionFunction);
extern void componentAppendInput (Component, Message);
extern Message componentPopInput (Component);
extern void componentCallReaction (Component, Message);

// Message
  /* $newMessage (Datum) -> Message; */
  /* $data (Message) -> Datum; */
extern Message messageNew (Datum);

// ConnectionTable
  /* $connectedTo (Component) -> Component; */
  /* $connect (Component, Component); */
extern Component connectionsConnectedTo (Component);
extern void connect (Component, Component);

// Kernel
  /* $withLock (v) $block; */
  /* $send (Component, Message); */
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

