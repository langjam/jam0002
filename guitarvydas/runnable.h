
typedef byte* Datum;
struct Component;
struct Message;
struct Connection;

struct {
  void (*react) (Component* self);
  Message* inputQueue;
  Message* outputQueue;
  void (*initialize) (Component* self);
} Component;

struct {
  void *datum;
  void *Message;
} Message;

struct {
  Component* sender;
  Component* receiver;
} Connection;

