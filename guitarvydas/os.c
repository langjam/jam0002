void DispatchTransferOutputsForOneComponent (Component c) {
  List outputs = $field (c, outputQueue);
  outputs =listReverse (outputs);
  while (!NULL == outputs) {
    Component receiver =$connectedToc;
    Datum data =outputs->data;
    Message m = $newMessage (data, NULL);
$appendInput;
outputs->next;
  }
}

void DispatchOnce () {
  List all = runQueue;
  while (!NULL == all{
      component =all->data;
      if (component->inputQueue) {
	Message m =$popInputcomponent;
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
  c =all->data;
  while (!NULL == all) {
    DispatchTransferOutputsForOneComponent (componentList);
all->next;
  }
}

