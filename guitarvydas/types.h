struct s_Component;
struct s_Sender;
struct s_Receiver;
struct s_Connection;
struct s_List;

union u_Message {
  char c;
  int i;
  struct s_Component* pcomponent;
  struct s_Connection* pconnection;
};

struct s_List {
  union {
    char c;
    int i;
    struct s_Component* pcomponent;
    struct s_Connection* pconnection;
    union u_Message message;
  } datum;
  struct s_List* next;
};

struct s_Component {
  void (*reactFunction) (struct s_Component* self, union u_Message);
  struct s_List* inputQueue;
  struct s_List* outputQueue;
  void (*initializeFunction) (struct s_Component* self);
};

struct s_Sender {
  struct s_Component* pcomponent;
  int pin;
};

struct s_Receiver {
  struct s_Component* pcomponent;
  int pin;
};

struct s_Connection {
  struct s_Sender sender;
  struct s_Receiver receiver;
};

typedef struct s_List List;
typedef struct s_Component Component;
typedef struct s_Connection Connection;
typedef union u_Message Message;

typedef void (*InitializationFunction) (Component*);
typedef void (*ReactionFunction) (Component*, Message);

