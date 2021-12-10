#include <stdio.h>
#include <stdlib.h>
#include "mpos.h"
#include "kernel.h"

void DispatchTransferOutputs ();
List* runQueue;
int systemRunning = 1;

void kernelStart () { systemRunning = 1; }
void kernelStop () { systemRunning = 0; }

void Dispatch ();
void DistributeOutputsToReceivers ();
void DispatchMaybe (Component*);
void DistributeOutputsFrom (Component*);
void DeliverMessageToReceiver (Component*, Message);
void RunComponentOnce (Component*);
void DistributeMaybe (Component*);
void DistributeOutputsFrom (Component*);

void Dispatcher () {
  while (systemRunning) {
    Dispatch ();
    DistributeOutputsToReceivers ();
  }
}

void Dispatch () {
  List* componentList;
componentList = runQueue;
  while ((componentList != NULL)) {
      Component* c = componentList->datum.pcomponent;
      DispatchMaybe (c);
componentList = (componentList != NULL) ? componentList->next : (List*)NULL;
    }
;
}

void DispatchMaybe (Component* c) {
  if (c->inputQueue) {
    RunComponentOnce (c);
  }
}

void RunComponentOnce (Component* c) {
  List* cell = componentPopInput (c);
  Message msg = cell->datum.message;
componentCallReaction (c, msg);
}

void DistributeOutputsToReceivers () {
  List* componentList;
componentList = runQueue;
  while ((componentList != NULL)) {
      Component* c = componentList->datum.pcomponent;
      DistributeMaybe (c);
componentList = (componentList != NULL) ? componentList->next : (List*)NULL;
    }
;
}

void DistributeMaybe (Component* c) {
  if (c->outputQueue) {
    DistributeOutputsFrom (c);
  }
}
			    
void DistributeOutputsFrom (Component* c) {
  List* outputs;
outputs = componentGetOutputsAsSent (c);
  while ((outputs != NULL)) {
    Message m = outputs->datum.message;
    DeliverMessageToReceiver (c, m);
outputs = (outputs != NULL) ? outputListAdvanceAndGC (outputs) : (List*)NULL;
  }
;
}

void DeliverMessageToReceiver (Component* c, Message m) {
  Component* receiver = connectionsConnectedTo (c);
componentAppendInput (receiver, m);
}


void panic (char* panicMessage) {
  printf ("%s\n", panicMessage);
  exit (1);
}

void kernelSendc (Component* self, char c) {
  Message m = $newMessagec (c);
componentAppendOutput (self->outputQueue, m);
}
 


