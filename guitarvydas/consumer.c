#include <stdio.h>
#include "cos.h"
#include "consumer.h"

// consumer
void initConsumer (Component self) {
  // nothing to do here
}

void reactConsumer (Component self, Message m) {
  printf ("consumer received: /%c/\n", m->c);
}

