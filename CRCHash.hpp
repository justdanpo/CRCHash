//******************************************************************************
// CRCHash Routines (Version 1.0)
// Copyright (c) 2002-2007, Mira Software, Inc. All rights reserved.
// Author: Andrey Klimoff
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// o Redistributions of source code must retain the above copyright
//   notice, this list of conditions and the following disclaimer.
// o Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer in the
//   documentation and/or other materials provided with the distribution.
// o The names of the authors may not be used to endorse or promote
//   products derived from this software without specific prior written
//   permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//******************************************************************************

// Revision: October 10, 2007

// 2012.11.22 den_po: compile time generated tables

/*******************************************************************************
   Cyclic Redundancy Check (CRC)

   A CRC is a numerical value related to a block of data. This value provides
information that allows an application to determine if the block of data has
been modified. A CRC is similar to a Checksum, but is considered a stronger
error-checking mechanism.

   CRC is based on binary polynomial calculation.

   Standard CRC-8 generator polynomial:
       Name               : CRC-8 Standard
       Standards          : -
       References         : -
       Initializing value : FF
       Finalizing value   : FF
       Polynomial value   : 31 (Mirror value = 8C)
       Polynom            : x^8 + x^5 + x^4 + 1

   Standard CRC-16 generator polynomial:
       Name               : CRC-16 Standard
       Standards          : ITU X.25/T.30
       References         : LHA
       Initializing value : 0000
       Finalizing value   : 0000
       Polynomial value   : 8005 (Mirror value = A001)
       Polynom            : x^16 + x^15 + x^2 + 1

   CRC-16 CCITT generator polynomial:
       Name               : CRC-16 CCITT
       Standards          : CRC-CCITT
       References         : ITU X.25/T.30, ADCCP, SDLC/HDLC
       Initializing value : FFFF
       Finalizing value   : 0000
       Polynomial value   : 1021 (Mirror value = 8408)
       Polynom            : x^16 + x^12 + x^5 + 1

   CRC-16 XModem generator polynomial:
       Name               : CRC-16 XModem
       Standards          : CRC-XModem
       References         : -
       Initializing value : 0000
       Finalizing value   : 0000
       Polynomial value   : 8408 (Mirror value = 1021)
       Polynom            : x^16 + x^12 + x^5 + 1

   Standard CRC-32 generator polynomial:
       Name               : CRC-32 Standard
       Standards          : ISO 3309, ITU-T V.42, ANSI X3.66, FIPS PUB 71
       References         : ZIP, RAR, Ethernet, AUTODIN II, FDDI
       Initializing value : FFFFFFFF
       Finalizing value   : FFFFFFFF
       Polynomial value   : 04C11DB7 (Mirror value = EDB88320)
       Polynom            : x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 +
                            x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1

   Standard CRC-64 generator polynomial:
       Name               : CRC-64 Standard
       Standards          : ISO 3309
       References         : -
       Initializing value : FFFFFFFFFFFFFFFF
       Finalizing value   : FFFFFFFFFFFFFFFF
       Polynomial value   : 000000000000001B (Mirror value = D800000000000000)
       Polynom            : x^64 + x^4 + x^3 + x + 1

   Standard/Mirror polynomial relationship:

   Consider Standard CRC-16 generator polynomial

   x^16 + x^15 + x^2 + 1 = 1-[1000-0000-0000-0101] = 8005

   8005 -> 1-[1000-0000-0000-0101] -> MIRROR -> 1-[1010-0000-0000-0001] = A001

*******************************************************************************/

#ifndef CRCHashH
#define CRCHashH

//******************************************************************************
// Template class 'CRCHash' implements mirror-algorithm of CRC calculation.

template <typename T, const T POLYNOM, const T INITIAL, const T FINAL>
class CRCHash
	{
	private:
		template <unsigned int index> class TableEntry;

		T CRC;
		static const T Table[256];
	public:
		CRCHash(void) : CRC(INITIAL) {;};
		void Reset(void) { CRC = INITIAL; };
		void Update(const void *Buffer, unsigned Length);
		inline void Update(unsigned char Value);
		T Evaluate(void) const { return CRC ^ FINAL; };
		static T Evaluate(const void *Buffer, unsigned Length);
	};

//******************************************************************************

template <typename T, const T POLYNOM, const T INITIAL, const T FINAL>
	void CRCHash<T, POLYNOM, INITIAL, FINAL>::
		Update(unsigned char Value)
		{
			CRC = (CRC >> 8) ^ Table[Value ^ static_cast<unsigned char>(CRC)];
		}

//******************************************************************************

template <typename T, const T POLYNOM, const T INITIAL, const T FINAL>
	void CRCHash<T, POLYNOM, INITIAL, FINAL>::
		Update(const void *Buffer, unsigned Length)
			{
				while (Length--)
					Update(*static_cast<const unsigned char *>(Buffer)++);
			}

//******************************************************************************

template <typename T, const T POLYNOM, const T INITIAL, const T FINAL>
	T CRCHash<T, POLYNOM, INITIAL, FINAL>::
		Evaluate(const void *Buffer, unsigned Length)
			{
				CRCHash Instance;
				Instance.Update(Buffer, Length);
				return Instance.Evaluate();
			}

//******************************************************************************

template <typename T, const T POLYNOM, const T INITIAL, const T FINAL>
	template <unsigned int index>
		class CRCHash<T, POLYNOM, INITIAL, FINAL>::TableEntry
			{
			private:
				enum
				{
					R0 = index >> 1 ^ ( index & 1 ? POLYNOM : 0 ),
					R1 = R0 >> 1 ^ ( R0 & 1 ? POLYNOM : 0 ),
					R2 = R1 >> 1 ^ ( R1 & 1 ? POLYNOM : 0 ),
					R3 = R2 >> 1 ^ ( R2 & 1 ? POLYNOM : 0 ),
					R4 = R3 >> 1 ^ ( R3 & 1 ? POLYNOM : 0 ),
					R5 = R4 >> 1 ^ ( R4 & 1 ? POLYNOM : 0 ),
					R6 = R5 >> 1 ^ ( R5 & 1 ? POLYNOM : 0 ),
					R7 = R6 >> 1 ^ ( R6 & 1 ? POLYNOM : 0 ),
				};
			public:
				enum
				{
					value = R7
				};
			};

template <typename T, const T POLYNOM, const T INITIAL, const T FINAL>
	const T CRCHash<T, POLYNOM, INITIAL, FINAL>::Table[256] =
		{
			TableEntry<0>::value,	TableEntry<1>::value,
			TableEntry<2>::value,	TableEntry<3>::value,
			TableEntry<4>::value,	TableEntry<5>::value,
			TableEntry<6>::value,	TableEntry<7>::value,
			TableEntry<8>::value,	TableEntry<9>::value,
			TableEntry<10>::value,	TableEntry<11>::value,
			TableEntry<12>::value,	TableEntry<13>::value,
			TableEntry<14>::value,	TableEntry<15>::value,
			TableEntry<16>::value,	TableEntry<17>::value,
			TableEntry<18>::value,	TableEntry<19>::value,
			TableEntry<20>::value,	TableEntry<21>::value,
			TableEntry<22>::value,	TableEntry<23>::value,
			TableEntry<24>::value,	TableEntry<25>::value,
			TableEntry<26>::value,	TableEntry<27>::value,
			TableEntry<28>::value,	TableEntry<29>::value,
			TableEntry<30>::value,	TableEntry<31>::value,
			TableEntry<32>::value,	TableEntry<33>::value,
			TableEntry<34>::value,	TableEntry<35>::value,
			TableEntry<36>::value,	TableEntry<37>::value,
			TableEntry<38>::value,	TableEntry<39>::value,
			TableEntry<40>::value,	TableEntry<41>::value,
			TableEntry<42>::value,	TableEntry<43>::value,
			TableEntry<44>::value,	TableEntry<45>::value,
			TableEntry<46>::value,	TableEntry<47>::value,
			TableEntry<48>::value,	TableEntry<49>::value,
			TableEntry<50>::value,	TableEntry<51>::value,
			TableEntry<52>::value,	TableEntry<53>::value,
			TableEntry<54>::value,	TableEntry<55>::value,
			TableEntry<56>::value,	TableEntry<57>::value,
			TableEntry<58>::value,	TableEntry<59>::value,
			TableEntry<60>::value,	TableEntry<61>::value,
			TableEntry<62>::value,	TableEntry<63>::value,
			TableEntry<64>::value,	TableEntry<65>::value,
			TableEntry<66>::value,	TableEntry<67>::value,
			TableEntry<68>::value,	TableEntry<69>::value,
			TableEntry<70>::value,	TableEntry<71>::value,
			TableEntry<72>::value,	TableEntry<73>::value,
			TableEntry<74>::value,	TableEntry<75>::value,
			TableEntry<76>::value,	TableEntry<77>::value,
			TableEntry<78>::value,	TableEntry<79>::value,
			TableEntry<80>::value,	TableEntry<81>::value,
			TableEntry<82>::value,	TableEntry<83>::value,
			TableEntry<84>::value,	TableEntry<85>::value,
			TableEntry<86>::value,	TableEntry<87>::value,
			TableEntry<88>::value,	TableEntry<89>::value,
			TableEntry<90>::value,	TableEntry<91>::value,
			TableEntry<92>::value,	TableEntry<93>::value,
			TableEntry<94>::value,	TableEntry<95>::value,
			TableEntry<96>::value,	TableEntry<97>::value,
			TableEntry<98>::value,	TableEntry<99>::value,
			TableEntry<100>::value,	TableEntry<101>::value,
			TableEntry<102>::value,	TableEntry<103>::value,
			TableEntry<104>::value,	TableEntry<105>::value,
			TableEntry<106>::value,	TableEntry<107>::value,
			TableEntry<108>::value,	TableEntry<109>::value,
			TableEntry<110>::value,	TableEntry<111>::value,
			TableEntry<112>::value,	TableEntry<113>::value,
			TableEntry<114>::value,	TableEntry<115>::value,
			TableEntry<116>::value,	TableEntry<117>::value,
			TableEntry<118>::value,	TableEntry<119>::value,
			TableEntry<120>::value,	TableEntry<121>::value,
			TableEntry<122>::value,	TableEntry<123>::value,
			TableEntry<124>::value,	TableEntry<125>::value,
			TableEntry<126>::value,	TableEntry<127>::value,
			TableEntry<128>::value,	TableEntry<129>::value,
			TableEntry<130>::value,	TableEntry<131>::value,
			TableEntry<132>::value,	TableEntry<133>::value,
			TableEntry<134>::value,	TableEntry<135>::value,
			TableEntry<136>::value,	TableEntry<137>::value,
			TableEntry<138>::value,	TableEntry<139>::value,
			TableEntry<140>::value,	TableEntry<141>::value,
			TableEntry<142>::value,	TableEntry<143>::value,
			TableEntry<144>::value,	TableEntry<145>::value,
			TableEntry<146>::value,	TableEntry<147>::value,
			TableEntry<148>::value,	TableEntry<149>::value,
			TableEntry<150>::value,	TableEntry<151>::value,
			TableEntry<152>::value,	TableEntry<153>::value,
			TableEntry<154>::value,	TableEntry<155>::value,
			TableEntry<156>::value,	TableEntry<157>::value,
			TableEntry<158>::value,	TableEntry<159>::value,
			TableEntry<160>::value,	TableEntry<161>::value,
			TableEntry<162>::value,	TableEntry<163>::value,
			TableEntry<164>::value,	TableEntry<165>::value,
			TableEntry<166>::value,	TableEntry<167>::value,
			TableEntry<168>::value,	TableEntry<169>::value,
			TableEntry<170>::value,	TableEntry<171>::value,
			TableEntry<172>::value,	TableEntry<173>::value,
			TableEntry<174>::value,	TableEntry<175>::value,
			TableEntry<176>::value,	TableEntry<177>::value,
			TableEntry<178>::value,	TableEntry<179>::value,
			TableEntry<180>::value,	TableEntry<181>::value,
			TableEntry<182>::value,	TableEntry<183>::value,
			TableEntry<184>::value,	TableEntry<185>::value,
			TableEntry<186>::value,	TableEntry<187>::value,
			TableEntry<188>::value,	TableEntry<189>::value,
			TableEntry<190>::value,	TableEntry<191>::value,
			TableEntry<192>::value,	TableEntry<193>::value,
			TableEntry<194>::value,	TableEntry<195>::value,
			TableEntry<196>::value,	TableEntry<197>::value,
			TableEntry<198>::value,	TableEntry<199>::value,
			TableEntry<200>::value,	TableEntry<201>::value,
			TableEntry<202>::value,	TableEntry<203>::value,
			TableEntry<204>::value,	TableEntry<205>::value,
			TableEntry<206>::value,	TableEntry<207>::value,
			TableEntry<208>::value,	TableEntry<209>::value,
			TableEntry<210>::value,	TableEntry<211>::value,
			TableEntry<212>::value,	TableEntry<213>::value,
			TableEntry<214>::value,	TableEntry<215>::value,
			TableEntry<216>::value,	TableEntry<217>::value,
			TableEntry<218>::value,	TableEntry<219>::value,
			TableEntry<220>::value,	TableEntry<221>::value,
			TableEntry<222>::value,	TableEntry<223>::value,
			TableEntry<224>::value,	TableEntry<225>::value,
			TableEntry<226>::value,	TableEntry<227>::value,
			TableEntry<228>::value,	TableEntry<229>::value,
			TableEntry<230>::value,	TableEntry<231>::value,
			TableEntry<232>::value,	TableEntry<233>::value,
			TableEntry<234>::value,	TableEntry<235>::value,
			TableEntry<236>::value,	TableEntry<237>::value,
			TableEntry<238>::value,	TableEntry<239>::value,
			TableEntry<240>::value,	TableEntry<241>::value,
			TableEntry<242>::value,	TableEntry<243>::value,
			TableEntry<244>::value,	TableEntry<245>::value,
			TableEntry<246>::value,	TableEntry<247>::value,
			TableEntry<248>::value,	TableEntry<249>::value,
			TableEntry<250>::value,	TableEntry<251>::value,
			TableEntry<252>::value,	TableEntry<253>::value,
			TableEntry<254>::value,	TableEntry<255>::value,
		};

//******************************************************************************

typedef unsigned char  CRC08;
typedef unsigned short CRC16;
typedef unsigned long  CRC32;

class CRC08Hash : public CRCHash<CRC08, 0x8CU, 0xFFU, 0xFFU> {};
class CRC16Hash : public CRCHash<CRC16, 0xA001U, 0x0000U, 0x0000U> {};
class CRC32Hash : public CRCHash<CRC32, 0xEDB88320UL, 0xFFFFFFFFUL, 0xFFFFFFFFUL> {};

//******************************************************************************

// typedef unsigned __int64 CRC64;
// class CRC64Hash : public CRCHash<CRC64, 0xD800000000000000UI64, 0xFFFFFFFFFFFFFFFFUI64, 0xFFFFFFFFFFFFFFFFUI64> {};

//******************************************************************************

#endif
