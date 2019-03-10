#define Halt 0
#define LDC 1
#define LD 2
#define LDF 3
#define LDRF 4
#define ADD 5
#define AP 6
#define RTN 7
#define SEL 8
#define JOIN 9

typedef intptr_t value_t;

typedef struct _env_node_t
{
	value_t val;
	struct _env_node *next;
} env_node;

typedef env_node *env_t;

typedef struct _closure_t
{
	int pc;
	env_t env;
} closure_t;

typedef struct _dump_t
{
	int pc;
	env_t env;
}dump_t;