#include <stdio.h>
#include <SECD.h>

int code [1000];
value_t stack [1000];
dump_t dump [1000];

env_t extend(value_t elem,env_t env){
	env_t new= (env_t*)malloc(sizeof(env_node_t));
	assert (new!=NULL);
	new->val = elem;
	new->next=env;
	return new;
}

value_t lookup(int n, env_t env){
	env_t new_env;
	new_env=env;
	for(i==0;i<n;i++){
		new_env=new_env->next;
	}
	return new_env->val;
}

closure_t make_closure(int pc, env_t env){
	closure_t new = (closure_t*)malloc(sizeof(closure_t));
	new->pc=pc;
	new->env=env;
}

value_t run(void){
	int pc=0;
	int sp=0;
	int dp=0;
	env_t env = NULL;
	while(true){
		value_t opa, opb;
		int t;
		closure_t cptr;
		env_t nenv;
		int opcode=code[pc++];
		switch(opcode){
			case LDC:
				opa=(value_t)code[pc++];
				stack[sp++]=opa;
				break;
			case LD:
				t=code[pc++];
				stack[sp++] = lookup(t,env);
				break;
			case LDF:
				t=code[pc++];
				cptr = make_closure(t,env);
				stack[sp++]=(value_t)cptr;
				break;
			case LDRF:
				t=code[pc++];
				nenv=extend((value_t)NULL,env);
				cptr=make_closure(t,nenv);
		}
	}
}