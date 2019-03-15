#include<stdio.h>
#include<inttypes.h>
#include<stdlib.h>
#include <assert.h>

#include "SECD.h"

int code[] = {1,1,1,2,1,3,5,5,0};
value_t stack[];
dump_t dump [];

env_t extend(value_t elem,env_t env){
	env_t new= (env_t)malloc(sizeof(env_t));
	assert (new!=NULL);
	new->val = elem;
	new->next=env;
	return new;
}

value_t lookup(int n, env_t env){
	printf("Está no lookup, n: %d",n);
	env_t new_env;
	new_env=env;
	for(int i=0;i<n;i++){
		new_env=new_env->next;
	}
	return new_env->val;
}

closure_t* make_closure(int pc, env_t env){
	closure_t *new=(closure_t*)malloc(sizeof(closure_t));
	new->pc=pc;
	new->env=env;
	return new;
}

value_t run(void){
	int pc=0;
	int sp=0;
	int dp=0;
	env_t env = NULL;

	for(;;){
		value_t opa, opb;
		int t;
		closure_t *cptr;
		env_t nenv;
		int opcode=code[pc];
		switch(opcode){
			case LDC:
				printf("está no LDC\n");
				opa=(value_t)code[++pc];
				printf("opa: %" PRIxPTR "\n", opa);
				stack[sp]=opa;
				printf("sp: %d \n",sp);
				printf("stack[sp]: %" PRIxPTR "\n", stack[sp]);
				printf("pc: %d \n",pc);
				sp++;
				pc++;
				printf("sp: %d \n",sp);
				printf("pc: %d \n",pc);
				break;
			case LD:
				printf("está no LD\n");
				t=code[pc++];
				printf("t: %d \n",t);
				stack[sp] = lookup(t,env);
				sp++;
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
			case ADD:
				printf("está no ADD\n");
				opa=stack[--sp];
				opb=stack[--sp];
				printf("opa: %" PRIxPTR " opb: %" PRIxPTR "\n", opa, opb);
				stack[sp++]=opa+opb;
				int j;
				for(j = 0; stack[j] != '\0'; j++)
			    	printf("stack[j]: %" PRIxPTR "\n",stack[j]);
				pc++;
				break;
			case SEL:
				opa=stack[--sp];
				pc=pc+2;
				dp++;
				if(opa==0){
					pc=code[pc];
				}
				else{
					pc=code[pc++];
				}
				break;
			case JOIN:
				pc=dump[dp].pc;
				break;
			case RTN:
				pc=dump[dp].pc;
				env=dump[dp].env;	
				break;
			case AP:
				opa=stack[--sp];
				value_t vl=stack[--sp];
				cptr=(closure_t*)vl;
				dump[dp].pc=pc;
				dump[dp].env=env;
				dp++;
				env=extend(opa,cptr->env);
				pc=cptr->pc;
				break;
			case Halt:
				printf("Entrou no halt\n");
				//size_t n= sizeof(stack)/sizeof(stack[0]); 
			    return stack[0];
				break;
		}
	}
	
}

int main(){
	value_t val;
	val = run();
	printf("Resultado: %" PRIxPTR "\n", val);
}