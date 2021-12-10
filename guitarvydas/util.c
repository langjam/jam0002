#include <stdio.h>
#include <stdlib.h>
#include "mpos.h"

ListCell allocCell ();
void freeCell (ListCell);
Message* allocMessage ();
void freeMessage (Message*);
Datum* allocDatum ();
void freeDatum (Datum*);

//////////

/* 
   list cell allocation performance could be improved by keeping a cache 
     of unused cells (instead of calling malloc/free every time) 
   (even in a GC'ed language) 
   If warranted by profiling.
*/

ListCell allocCell () {
  ListCell cell = (ListCell) malloc (sizeof (ListCell));
  return cell;
}

void freeCell (ListCell cell) {
  freeMessage (cell->data);
  cell->data = NULL;
  cell->next = NULL;
  free (cell);
}


/* we could be smarter about this, if warranted by profiling */
Message* allocMessage () {
  Message* result = (Message*) malloc (sizeof (Message));
  return result;
}

void freeMessage (Message* msg) {
  free (msg);
}

/* we could be smarter about this, if warranted by profiling */
Datum* allocDatum () {
  Datum* result = (Datum*) malloc (sizeof (Datum));
  return result;
}

void freeDatum (Datum* d) {
  free (d);
}



// List
List listNewCell (Datum d) {
  ListCell cell = allocCell ();
  Datum* newd = allocDatum ();
  cell->d = newd;
  cell->data.c = d.c;
  cell->data.component = d.component;
  cell->data.connection = d.connection;
  cell->next = NULL;
  return cell;
}

List listAppend1 (List l, Message m) {
  /* append Message to end of list */
  /* dumb version - CDR() down list until end (using recursive function calls) */
  if (l->next == NULL) {
    l->next = m;
    return l;
  } else {
    return listAppend1 (l->next, m);
  }
}

extern List listAppend (List l1, List l2) {
  // smarten this up if warranted
  // (fold listAppend1 into listAppend if warranted)
  if (l1->next == NULL) {
    l1->next = l2;
    return l1;
  } else {
    return listAppend (l1->next, l2);
}

List listNewCellComponent (Component c) {
  ListCell cell = allocCell ();
  cell->data.component = c;
  cell->next = NULL;
  return cell;
}

// Component
Component componentNew (InitializationFunction initfn, ReactionFunction reactfn) {
  Component component = (Component) malloc (sizeof (Component));
  component->react = reactfn;
  component->initialize = initfn;
  component->inputQueue = NULL;
  component->outputQueue = NULL;
  return component;
}

// no need to free Components - they are initialized at the beginning and never die */

void componentAppendInput (Component component, Message* msg) {
  ListCell cell = listNewCell ();
  cell->data = msg;
  cell->next = NULL;
  component->inputQueue = listAppend1 (component->inputQueue, cell);
}

ListCell componentPopInput (Component component) {
  if (NULL == component->inputQueue) {
    panic ("empty input queue being popped");
    return NULL; // should never reach this point
  } else {
    ListCell result = component->inputQueue;
    component->inputQueue = result->next;
    result->next = NULL;
    return result;
  }
}

void componentCallReaction (Component component, Message* msg) {
  (*component->reactFunction) (component, msg);
}

List componentGetOutputsAsSent (Component component) {
  // reverse the output list to get outputs in the order they were sent
  List reversed = listReverse (component->outputQueue);
  return reversed;
}

// Message
Message* messageNewc (char c) {
  Message* msg = (Message*) allocMessage ();
  msg->c = c;
  return msg;
}

// ConnectionTable
// for purposes of this example, we statically allocate the connection table
// in general, though, connections should belong to the parent container
// in general, we could allocate connection tables in an OO manner, but,
//  the compiler can be smart enough to figure out how many connections there
//  are in a system an statically allocate them all 
//  (if profiling shows a need to optimize this portion)
Connection mtable; 

Component connectionsConnectedTo (Component component) {
  Component receiver = mtable [0].receiver; // not generalized
  return receiver;
}

void connectionsConnect (Component sender, Component receiver) {
  // not generalized
  mtable [0].sender = sender;
  mtable [0].receiver = receiver;
}
  
// Kernel
void kernelSendc (Component component, char c) {
  Message msg = messageNewc (c);
  ListCell cell = listNewCell ();
  cell->data = msg;
  cell->next = NULL;
  component->outputQueue = listAppend1 (component->outputQueue, cell);
}

void kernelPanic (char* str) {
  fprintf (stderr, str);
  exit (1);
}

int systemRunning = 1;
void kernelStart () { systemRunning = 1; }
void kernelStop () { systemRunning = 0; }

ListCell outputListFreeAndAdvance (List l) { /* $advanceOutputs */
  ListCell garbage = l;
  ListCell result = l->next;
  freeMessage (garbage->data);
  freeCell (garbage);
  return result;
}
