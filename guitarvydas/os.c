void DispatchTransferOutputsForOneComponent (Component c) {
  List outputs = $field;
  outputs = listReverse (outputs);
  while (!NULL == outputs) {
    Component receiver = connectionsConnectedTo (c);
    Datum data = outputs->data;
    Message m = $newMessage (data, NULL);
$appendInput;
outputs->next;
  }
}

void DispatchOnce () {
  List all = runQueue;
  while (!NULL == all{
      component = all->data;
      if (component->inputQueue) {
	Message m = componentPopInput (component);
$callReaction;
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

