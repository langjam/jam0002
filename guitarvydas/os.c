#include <stdio.h>
#include <stdlib.h>
#include "mpos.h"

void DispatchTransferOutputs ();
List runQueue;

void kernelStart () { systemRunning = 1; }
void kernelStop () { systemRunning = 0; }

void Dispatch ();
void DistributeOutputsToReceivers ();
void DispatchMaybe (Component);
void DistributeOutputsFrom (Component);
void DeliverMessageToReceiver (Component, Message*);
void RunComponentOnce (Component);
void DistributeMaybe (Component);
void DistributeOutputsFrom (Component);

void Dispatcher () {
  while (systemRunning) {
    Dispatch ();
    DistributeOutputsToReceivers ();
  }
}

void Dispatch () {
  List componentList;
componentList = runQueue;
  while ((componentList != NULL)) {
      Component c = componentList->data->component;
      DispatchMaybe (c);
componentList = (componentList != NULL) ? componentList->next : NULL;
    }
;
}

void DispatchMaybe (Component c) {
  if (c->inputQueue) {
    RunComponentOnce (c);
  }
}

void RunComponentOnce (Component c) {
  ListCell cell = componentPopInput (c);
  Datum* data = cell->data;
componentCallReaction (c, data);
}

void DistributeOutputsToReceivers () {
  List componentList;
componentList = runQueue;
  while ((componentList != NULL)) {
      Component c = componentList->data->component;
      DistributeMaybe (c);
componentList = (componentList != NULL) ? componentList->next : NULL;
    }
;
}

void DistributeMaybe (Component c) {
  if (c->outputQueue) {
    DistributeOutputsFrom (c);
  }
}

void DistributeOutputsFrom (Component c) {
  List outputs;
outputs = componentGetOutputsAsSent (c);
  while ((outputs != NULL)) {
    Message* m = outputs->data;
    DeliverMessageToReceiver (c, m);
outputs = (outputs != NULL) ? outputListFreeAndAdvance (outputs) : NULL;
  }
;
}

void DeliverMessageToReceiver (Component c, Message* m) {
  Component receiver = connectionsConnectedTo (c);
componentAppendInput (receiver, m);
}


void panic (char* panicMessage) {
  printf ("%s\n", panicMessage);
  exit (1);
}

void kernelSendc (Component self, unsigned char c) {
  Message m = messageNewc (c);
self->outputQueue = listAppend (self->outputQueue, m);
}
 


