struct s_Component;
struct u_Message;
struct s_Connection;
struct s_List;

union u_Datum {
  char c;
  struct s_Component* component;
  union u_Message* message;
  struct s_Connection* connection;
};
 
struct s_Component {
  void (*react) (struct s_Component* self);
  struct s_List* inputQueue;
  struct s_List* outputQueue;
  void (*initialize) (struct s_Component* self);
};

struct u_Message {
  union u_Datum d;
};

struct s_Connection {
  struct s_Component* sender;
  struct s_Component* receiver;
};

struct s_List {
  union u_Datum data;
  struct s_List* next;
};

typedef union u_Datum Datum;
typedef struct s_Component* Component;
typedef union u_Message* Message;
typedef struct s_Connection* Connection;
typedef struct s_List* List;

typedef void (*InitializationFunction) (Component);
typedef void (*ReactionFunction) (Component, Message);
