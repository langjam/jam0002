void DispatchTransferOutputsForOneComponent (Component c) {
  List outputs = c->outputQueue;
  outputs = listReverse (outputs);
  while (!NULL == outputs) {
    Component receiver = connectionsConnectedTo (c);
    Datum data = outputs->data;
    Message m = messageNew (data);
componentAppendInput (receiver, m);
outputs->next;
  }
}

void DispatchOnce () {
  List all = runQueue;
  while (!NULL == all{
      component = all->data;
      if (component->inputQueue) {
	Message m = componentPopInput (component);
componentCallReaction (component, m);
      }
all->next;
    }
}
  
void Dispatcher () {
  while (systemRunning) {
    DispatchOnce ();
  }
  DispatchTransferOutputs ();
}

void DispatchTransferOutputs () {
  List all = runQueue;
  c = all->data;
  while (!NULL == all) {
    DispatchTransferOutputsForOneComponent (componentList);
all->next;
  }
}

