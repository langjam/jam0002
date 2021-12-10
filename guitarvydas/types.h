struct s_Component;
struct s_Sender;
struct s_Receiver;
struct s_Connection;
struct s_ListCell;

union u_Datum {
  char c;
  struct s_Component* component;
  struct s_Connection* connection;
};



struct s_ListCell {
  union u_Datum* data;
  struct s_ListCell* next;
};

struct s_Component {
  void (*react) (struct s_Component* self);
  struct s_ListCell* inputQueue;
  struct s_ListCell* outputQueue;
  void (*initialize) (struct s_Component* self);
};

struct s_Sender {
  struct s_Component* component;
  int pin;
};

struct s_Receiver {
  struct s_Component* component;
  int pin;
};

struct s_Connection {
  struct s_Sender sender;
  struct s_Receiver receiver;
};

typedef struct s_ListCell* List;
typedef struct s_ListCell* ListCell;
typedef union u_Datum Message;
typedef union u_Datum Datum;
typedef struct s_Component* Component;
typedef struct s_Connection* Connection;

typedef void (*InitializationFunction) (Component);
typedef void (*ReactionFunction) (Component, Message);
