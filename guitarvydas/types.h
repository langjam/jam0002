struct s_Component;
struct s_Sender;
struct s_Receiver;
struct s_Connection;
struct s_ListCell;

union u_Message {
  char c;
  int i;
  struct s_Component* pcomponent;
  struct s_Connection* pconnection;
};

struct s_ListCell {
  union {
    char c;
    int i;
    struct s_Component* pcomponent;
    struct s_Connection* pconnection;
    union u_Message message;
  } datum;
  struct s_ListCell* next;
};

struct s_Component {
  void (*react) (struct s_Component* self);
  struct s_ListCell* inputQueue;
  struct s_ListCell* outputQueue;
  void (*initialize) (struct s_Component* self);
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

typedef struct s_ListCell List;
typedef struct s_ListCell ListCell;
typedef struct s_Component Component;
typedef struct s_Connection Connection;
typedef union u_Message Message;

typedef void (*InitializationFunction) (Component*);
typedef void (*ReactionFunction) (Component*, Message);

