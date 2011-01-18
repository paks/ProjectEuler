// Primegen.h

#pragma once

#include "uint32.h"
#include "uint64.h"
#define PRIMEGEN_WORDS 2048

using namespace System;


typedef struct {
	uint32 buf[16][PRIMEGEN_WORDS];
	uint64 p[512]; /* p[num-1] ... p[0], in that order */
	int num;
	int pos; /* next entry to use in buf; WORDS to restart */
	uint64 base;
	uint64 L;
} primegen;

void primegen_sieve(primegen *);
void primegen_fill(primegen *);

void primegen_init(primegen *);
uint64 primegen_next(primegen *);
uint64 primegen_peek(primegen *);
uint64 primegen_count(primegen *,uint64 to);
void primegen_skipto(primegen *,uint64 to);

namespace Primegen {

	public ref class Primes
	{
		private:
			primegen* pg;
		public:
			Primes();
			virtual ~Primes();
			unsigned long long next(void);
			uint64 peek(void);
			uint64 count(uint64 to);
			void skipto(uint64 to);
			void sieve(void);
			void fill(void);
			void reset(void);
			//Boolean isPrime(BigInteger n, int witnesses);
	};
}
