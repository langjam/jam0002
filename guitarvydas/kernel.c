Message* kernelNewMessage (Datum* data) {
  Message* m = (Message*) malloc (sizeof (Message));
  m->datum = data;
  return m;
}

Message* kernelPopInputQueue (Runnable* self) {
  if (self->inputQueue) {
    Message* m = self->inputQueue;
    self->inputQueue = m->next;
    m->next = NULL;
    return m;
  } else {
    panic ("in kernelPopInputQueue");
  }
}

void kernelSend (Runnable* self, Datum data) {
  Message* outMessage = kernelNewMessage (data);
  outMessage->next = self->outputQueue;
  self->outputQueue = outMessage;
}

ListCell* kernelNewListCell (Datum data, ListCell* next) {
  ListCell* c = (ListCell*) malloc (sizeof (ListCell));
  if (c == NULL) {
    panic ("allocation error in kernelNewListCell");
  }
  return c;
}


