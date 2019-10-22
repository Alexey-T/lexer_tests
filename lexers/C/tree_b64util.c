
/* Allgemeine Header: */
/* #include "bskdef.h" */ /* siehe b64util.h */
#if (defined _WIN32_WCE) || (defined USE_OSAL_ERR)
  #include "osal_err.h"         /* fuer OSAL_vErrorSet/OSAL_iErrorGet */
  #define SetErrNo(i)   OSAL_vErrorSet(i)
  #define GetErrNo()    OSAL_iErrorGet()
#else
  #define SetErrNo(i)   ((void)(errno=(i)))
  #define GetErrNo()    errno
#endif

/* Eigene Headerdatei: */
#include "b64util.h"
/* Weitere Modul-Header: */
/* #include "..." */

/***************************************************************************
Schalter und Definitionen (ggf. auch fuer eigene Anpassungsdatei)
***************************************************************************/

/* B64UTIL__DBG_Test: Test mit Test-Records */
/* ---------------------------------------- */
#ifndef B64UTIL__DBG_Test
  #define B64UTIL__DBG_Test     (Off&&!defined NDEBUG)
#endif


/* Zuletzt eigene, private Anpassungsdatei oder wrn_norm.h */
#include "wrn_norm.h"


/***************************************************************************
Tests
***************************************************************************/

#if B64UTIL_nInval!=-1
  #error "B64UTIL_nInval!=-1"
  /* (fuer Rueckgabe in hsRem[1] <0 und fuer TestCase ='\xFF') */
#endif


/***************************************************************************
Lokale Definitionen (Schalter, Macros, Typen, Konstanten und Variablen)
***************************************************************************/

/* Tabellen der Testfaelle */
/* ----------------------- */
#if B64UTIL__DBG_Test
  #define B64UTIL__nTestCases   30
  typedef struct B64UTIL__tstTestCase {
    const char          *hsRemIn;       /* Voriger Rest base64-String */
    const char          *hsB64;         /* base64-String */
    const char          *pcBin;         /* binaerer String (als char) */
    size_t              xoBytes;        /* Anzahl Bytes */
    const char          *hsRemOut;      /* Rest nach B64UTIL_pcB642BinA */
    int32               i32ErrPos;      /* Fehlerposition oder -1 */
  } B64UTIL__tstTestCase, *B64UTIL__tpstTestCase;

  const B64UTIL__tstTestCase B64UTIL__astTestCases[B64UTIL__nTestCases] = {
    {"", "UEsD",                        "\x50\x4B\x03",3,"\0\0",-1},
    {"", "UEsDUA==",                    "\x50\x4B\x03\x50",4,"\0\2",-1},
    {"", "UEsDUEs=",                    "\x50\x4B\x03\x50\x4B",5,"\0\1",-1},
    {"", "UEsDBBQAAgAIAG8h8B6MepP6wi4A","\x50\x4B\x03\x04\x14\x00\x02\x00\x08\x00\x6F\x21\xF0\x1E\x8C\x7A\x93\xFA\xC2\x2E\x00",
                                        21,"\0\0",-1},
    {"", "ABCDEFGHIJKLMNOPQRSTUVWXYZA=","\x00\x10\x83\x10\x51\x87\x20\x92\x8B\x30\xD3\x8F\x41\x14\x93\x51\x55\x97\x61\x90",
                                        20,"\0\1",-1},
    {"", "abcdefghijklmnopqrstuvwxyzA=","\x69\xB7\x1D\x79\xF8\x21\x8A\x39\x25\x9A\x7A\x29\xAA\xBB\x2D\xBA\xFC\x31\xCB\x30",
                                        20,"\0\1",-1},
    {"", "0123456789+/",                "\xD3\x5D\xB7\xE3\x9E\xBB\xF3\xDF\xBF",
                                        9,"\0\0",-1},
    {"", "UEsDU",                       "\x50\x4B\x03",3,"U",-1},
    {"", "UEsDUA",                      "\x50\x4B\x03",3,"UA",-1},
    {"", "UEsDUEs",                     "\x50\x4B\x03",3,"UEs",-1},
    {"", " U \n E \t s \r DU E s =",    "\x50\x4B\x03\x50\x4B",5,"\0\1",-1},
    {"", "UEsDU@Es=",                   "\x50\x4B\x03",3,"U",5},
    {"U","Es",                          "",0,"UEs",-1},
    {"UE","s",                          "",0,"UEs",-1},
    {"UEs","",                          "",0,"UEs",-1},
    {"U","=",                           "",0,"\0\xFF",-1}, /* '\xFF'=(uchar)B64UTIL_nInval */
    {"UE","=",                          "\x50",1,"\0\2",-1},
    {"UEs","=",                         "\x50\x4B",2,"\0\1",-1},
    {"U","EsD",                         "\x50\x4B\x03",3,"\0\0",-1},
    {"UE","sD",                         "\x50\x4B\x03",3,"\0\0",-1},
    {"UEs","D",                         "\x50\x4B\x03",3,"\0\0",-1},
    {"U","EsDU",                        "\x50\x4B\x03",3,"U",-1},
    {"UE","sDU",                        "\x50\x4B\x03",3,"U",-1},
    {"UEs","DU",                        "\x50\x4B\x03",3,"U",-1},
  };
#endif /* B64UTIL__DBG_Test */


/***************************************************************************
Lokale Funktionen
***************************************************************************/

#if B64UTIL__DBG_Test
  /* B64UTIL__vTestB642Bin: B64UTIL_pcB642BinA mit B64UTIL__astTestCases pruefen */
  /* --------------------------------------------------------------------------- */
  static void B64UTIL__vTestB642Bin(void) {
    size_t                      xoTestCase;
    const B64UTIL__tstTestCase  *pstTestCase;
    char                        acRem[4];
    byte                        abBinBuf[256];
    const char                  *pcErrPos;
    size_t                      xoBytes;
    bool                        boRemStr;
    int32                       i32ErrPos;

    pstTestCase = B64UTIL__astTestCases;
    for (xoTestCase=0;xoTestCase<B64UTIL__nTestCases;xoTestCase++) {
      if (pstTestCase->hsB64==NULL) break;
      i32ErrPos = -1;
      memcpy(acRem,pstTestCase->hsRemIn,sizeof(acRem));
      boRemStr = true;
      pcErrPos = B64UTIL_pcB642BinA(abBinBuf,&xoBytes, pstTestCase->hsB64,acRem);
      if (pcErrPos!=NULL) {
        if (*pcErrPos==B64UTIL_ncPad) boRemStr = false;
        else i32ErrPos = (int32)(pcErrPos-pstTestCase->hsB64);
      } /* pcErrPos!=NULL */
      if (i32ErrPos!=pstTestCase->i32ErrPos)
        printf("?B64UTIL__vTestB642Bin: i32ErrPos!=B64UTIL__astTestCases[%lu].i32ErrPos\n",
               (dword)xoTestCase);
      if (xoBytes!=pstTestCase->xoBytes)
        printf("?B64UTIL__vTestB642Bin: xoBytes!=B64UTIL__astTestCases[%lu].xoBytes\n",
               (dword)xoTestCase);
      if (memcmp(abBinBuf,pstTestCase->pcBin,xoBytes)!=0)
        printf("?B64UTIL__vTestB642Bin: abBinBuf!=B64UTIL__astTestCases[%lu].pcBin\n",
               (dword)xoTestCase);
      if (boRemStr) {
        if (strcmp(acRem,pstTestCase->hsRemOut)!=0)
          printf("?B64UTIL__vTestB642Bin: acRem!=B64UTIL__astTestCases[%lu].hsRemOut\n",
                 (dword)xoTestCase);
      }
      else {
        if (memcmp(acRem,pstTestCase->hsRemOut,2)!=0)
          printf("?B64UTIL__vTestB642Bin: acRem!=B64UTIL__astTestCases[%lu].hsRemOut\n",
                 (dword)xoTestCase);
      }
      pstTestCase++;
    } /* for */
  } /* B64UTIL__vTestB642Bin */


  /* B64UTIL__vTestB64Len: B64UTIL_vB64LenA mit B64UTIL__astTestCases pruefen */
  /* ------------------------------------------------------------------------ */
  static void B64UTIL__vTestB64Len(void) {
    size_t                      xoTestCase;
    const B64UTIL__tstTestCase  *pstTestCase;
    size_t                      xoB64Len;
    const char                  *pcErrPos;
    int32                       i32ErrPos;
    ldiv_t                      ldiv_res;       /* Anzahl Triples und Rest */


    pstTestCase = B64UTIL__astTestCases;
    for (xoTestCase=0;xoTestCase<B64UTIL__nTestCases;xoTestCase++) {
      if (pstTestCase->hsB64==NULL) break;
      /* Laenge von hsRemIn+hsB64 */
      xoB64Len = B64UTIL_xoB64LenA(pstTestCase->hsRemIn,NULL);
      xoB64Len += B64UTIL_xoB64LenA(pstTestCase->hsB64,&pcErrPos);
      /* ErrPos bestimmen */
      i32ErrPos = -1;
      if (pcErrPos!=NULL && *pcErrPos!=B64UTIL_ncPad)
        i32ErrPos = (int32)(pcErrPos-pstTestCase->hsB64);
      /* ErrPos pruefen */
      if (i32ErrPos!=pstTestCase->i32ErrPos)
        printf("?B64UTIL__vTestB642Len: i32ErrPos!=B64UTIL__astTestCases[%lu].i32ErrPos\n",
               (dword)xoTestCase);
      /* Anzahl Triples und Rest berechnen */
      ldiv_res = ldiv((long)xoB64Len,4);
      if (*pstTestCase->hsRemOut!='\0') { /* Rest? */
        if (ldiv_res.quot*3!=(long)pstTestCase->xoBytes)
          printf("?B64UTIL__vTestB64Len: quot*3!=B64UTIL__astTestCases[%lu].xoBytes\n",
                 (dword)xoTestCase);
        if (ldiv_res.rem!=(long)strlen(pstTestCase->hsRemOut))
          printf("?B64UTIL__vTestB64Len: rem!=strlen(B64UTIL__astTestCases[%lu].pstTestCase->hsRemOut)\n",
                 (dword)xoTestCase);
      } /* Rest */
      else { /* kein Rest */
        if (ldiv_res.rem>0) --ldiv_res.rem;
        if (ldiv_res.quot*3+ldiv_res.rem!=(long)pstTestCase->xoBytes)
          printf("?B64UTIL__vTestB64Len: quot*3+rem!=B64UTIL__astTestCases[%lu].xoBytes\n",
                 (dword)xoTestCase);
      } /* kein Rest */
      pstTestCase++;
    } /* for */
  } /* B64UTIL__vTestB64Len */


  /* B64UTIL__vTestBin2B64: B64UTIL_vBin2B64A mit B64UTIL__astTestCases pruefen */
  /* -------------------------------------------------------------------------- */
  static void B64UTIL__vTestBin2B64(void) {
    size_t                      xoTestCase;
    const B64UTIL__tstTestCase  *pstTestCase;
    char                        acB64Buf[256];

    pstTestCase = B64UTIL__astTestCases;
    for (xoTestCase=0;xoTestCase<B64UTIL__nTestCases;xoTestCase++) {
      if (pstTestCase->hsB64==NULL) break;
      if (*pstTestCase->hsRemIn=='\0' && pstTestCase->i32ErrPos==-1
       && strlen(pstTestCase->hsB64)%4==0) {
        B64UTIL_vBin2B64A(acB64Buf,(byte*)pstTestCase->pcBin,pstTestCase->xoBytes);
        if (memcmp(acB64Buf,pstTestCase->hsB64,(pstTestCase->xoBytes+2)/3*4+1)!=0) {
          printf("?B64UTIL__vTestBin2B64: acB84Buf!=B64UTIL__astTestCases[%lu].hsB64\n",
                 (dword)xoTestCase);
        } /* Fehler */
      } /* *pstTestCase->hsRemIn=='\0' && ... */
      pstTestCase++;
    } /* for */
  } /* B64UTIL__vTestBin2B64 */
#endif /* B64UTIL__DBG_Test */


/***************************************************************************
Globale Funktionen
***************************************************************************/


/* B64UTIL_pcB642BinA: base64-String nach Bin wandeln */
/* -------------------------------------------------- */
const char * stdcall B64UTIL_pcB642BinA(byte *pbBin, size_t *pxoBytes,
                                        const char *hsB64, char *hsRem) {
  size_t        xoI4;           /* Zaehler fuer jeweils 4 Char */
  tLongRec      unTriple;       /* Struktur fuer jeweils 3 Bytes */
  byte          *pbBinStart;    /* Anfangswert von pbBin */
  int8          i8B64;

  DUMMY_INIT_LOCAL_VARIABLE(unTriple.dw,dword);

  assert(pbBin!=NULL);
  assert(hsRem!=NULL);
  assert(hsB64!=NULL);

  pbBinStart = pbBin; /* Anfang zur Berechnung von *pxoBytes speichern */
  xoI4 = 4; /* Zaehler fuer jeweils 4 base64-Char */

  /* Rest vom letzen Aufruf uebernehmen */
  if (hsRem[0]!='\0') {
    xoI4 = 0;
    do unTriple.dw = (unTriple.dw<<6) + B64UTIL_ai8Char2B64[(uchar)hsRem[xoI4]];
    while (hsRem[++xoI4]!='\0');
    hsRem[0] = '\0';
    xoI4 = 4 - xoI4;
  } /* hsRem[0]!='\0' */

  /* neue Zeichen verarbeiten */
  for (;;) {
    i8B64 = B64UTIL_ai8Char2B64[(uchar)*hsB64++];
    if (i8B64>=0) { /* base64-Char? */
      unTriple.dw = (unTriple.dw<<6) + i8B64;
      if (--xoI4!=0) continue; /* noch keine 4 base64-Zeichen beisammen */
      /* 4 base64-Zeichen ergeben 3 Byte */
      *pbBin++ = unTriple.b.b2;
      *pbBin++ = unTriple.b.b1;
      *pbBin++ = unTriple.b.b0;
      xoI4 = 4; /* Zaehler fuer die naechsten 4 base64-Char */
      continue;
    } /* base64-Char */
    if (i8B64==B64UTIL_nSpace) continue; /* Leerzeichen */
    if (i8B64==B64UTIL_nEnd) {
      hsB64 = NULL; /* kein Fehler */
      break; /* Ende ohne Fehler */
    } /* End */
    if (i8B64==B64UTIL_nInval) {
      --hsB64; /* Zurueck auf falsches Zeichen */
      break; /* Ende mit Fehler */
    } /* Inval */
    /* hier nur noch Pad moeglich */
    --hsB64; /* Zurueck auf Fuellzeichen */
    switch (xoI4) {
      case 4:
        /* kein Rest */
        hsRem[1] = (char)0; /* eigentlich kein Fuellzeichen erlaubt */
        break; /* Ende */
      case 3:
        /* ein base64-Char am Ende gibt kein ganzes Byte */
        hsRem[1] = (char)B64UTIL_nInval; /* Fuellzeichen hier unzulaessig */
        break; /* Ende */
      case 2:
        /* zwei base64-Char am Ende geben ein Byte */
        *pbBin++ = (byte)(unTriple.w.w0 >> 4);
        hsRem[1] = (char)2; /* normalerweise zwei Fuellzeichen noetig */
        break; /* Ende */
      default: /* nur noch xoI4==1 moeglich */
        /* drei base64-Char am Ende geben zwei Byte */
        unTriple.dw >>= 2;
        *pbBin++ = unTriple.b.b1;
        *pbBin++ = unTriple.b.b0;
        hsRem[1] = (char)1; /* ein Fuellzeichen noetig */
    } /* switch */
    *pxoBytes = (size_t)(pbBin-pbBinStart);
    return(hsB64);
  } /* for */

  /* hier Ende ohne Fuellzeichen, ggf. Rest speichern */
  if (xoI4<4) {
    hsRem[4-xoI4] = '\0';
    for (;;) {
      hsRem[3-xoI4] = B64UTIL_acB642Char[unTriple.b.b0&0x3F];
      if (++xoI4==4) break;
      unTriple.dw >>= 6;
    } /* for */
  } /* xoI4!=4 */

  /* Anzahl und Fehlerposition zurueckgeben */
  *pxoBytes = (size_t)(pbBin-pbBinStart);
  return(hsB64);
} /* B64UTIL_pcB642BinA */


/* B64UTIL_xoB64LenA: Effektive Laenge eines base64-Strings ermitteln */
/* ------------------------------------------------------------------ */
size_t stdcall B64UTIL_xoB64LenA(const char *hsB64, const char **ppcEnd) {
  size_t        xoB64Len;
  int8          i8B64;

  assert(hsB64!=NULL);

  xoB64Len = 0;
  for (;;) {
    i8B64 = B64UTIL_ai8Char2B64[(uchar)*hsB64++];
    if (i8B64>=0) { /* base64-Char? */
      ++xoB64Len;
      continue;
    } /* base64-Char */
    if (i8B64==B64UTIL_nSpace) continue; /* Leerzeichen */
    if (i8B64==B64UTIL_nEnd) {
      hsB64 = NULL; /* kein Fehler */
      break; /* Ende ohne Fehler */
    } /* End */
    /* hier nur noch Inval oder Pad moeglich */
    --hsB64; /* Zurueck auf falsches oder Fuellzeichen */
    break; /* Ende mit Fehler */
  } /* for */

  /* ppcEnd setzen */
  if (ppcEnd!=NULL) *ppcEnd = hsB64;

  return(xoB64Len);
} /* B64UTIL_xoB64LenA */


/* B64UTIL_vBin2B64A: Bin nach base64 wandeln */
/* ------------------------------------------ */
void stdcall B64UTIL_vBin2B64A(char *hsB64, const byte *pbBin, size_t xoBytes) {
  ldiv_t        ldiv_res;       /* Anzahl Triples und Rest */
  tLongRec      unTriple;       /* Struktur fuer jeweils 3 Bytes */

  /* Anzahl Triples und Rest berechnen */
  ldiv_res = ldiv((long)xoBytes,3);

  /* Alle vollst. Tripel bearbeiten */
  unTriple.b.b3 = 0;
  while (ldiv_res.quot-->0) {
    unTriple.b.b2 = *pbBin++;
    unTriple.b.b1 = *pbBin++;
    unTriple.b.b0 = *pbBin++;
    hsB64[3] = B64UTIL_acB642Char[unTriple.b.b0&0x3F];
    unTriple.dw >>= 6;
    hsB64[2] = B64UTIL_acB642Char[unTriple.b.b0&0x3F];
    unTriple.dw >>= 6;
    hsB64[1] = B64UTIL_acB642Char[unTriple.b.b0&0x3F];
    unTriple.w.w0 >>= 6;
    hsB64[0] = B64UTIL_acB642Char[unTriple.b.b0];
    hsB64 += 4;
  } /* while */

  /* Rest mit Fuellzeichen bearbeiten */
  if (ldiv_res.rem!=0) {
    *hsB64++ = B64UTIL_acB642Char[pbBin[0]>>2];
    if (ldiv_res.rem==1) {
      *hsB64++ = B64UTIL_acB642Char[(pbBin[0]&0x03)<<4];
      *hsB64++ = B64UTIL_ncPad;
    } /* ldiv_res.rem==1 */
    else { /* ldiv_res.rem==2 */
      *hsB64++ = B64UTIL_acB642Char[((pbBin[0]&0x03)<<4)+(pbBin[1]>>4)];
      *hsB64++ = B64UTIL_acB642Char[(pbBin[1]&0x0F)<<2];
    } /* ldiv_res.rem==2 */
    *hsB64++ = B64UTIL_ncPad;
  } /* ldiv_res.rem!=0 */

  /* Delimiter einsetzen */
  *hsB64 = '\0';
} /* B64UTIL_vBin2B64A */


/***************************************************************************
Init/Deinit
***************************************************************************/

byte B64UTIL__bInitCnt; /* Zaehler fuer Init/Deinit */


/* B64UTIL_vInit: Modul initialisieren */
/* --------------------------------- */
void stdcall B64UTIL_vInit(void) {
  uchar         uc;
  int8          i8;

  TestModInit(B64UTIL);

  /* erst mal alle Eintraege auf Ungueltig */
  memset(B64UTIL_ai8Char2B64,B64UTIL_nInval,sizeof(B64UTIL_ai8Char2B64));

  /* alle Leerzeichen markieren */
  for (uc=(uchar)'\1';uc<=(uchar)' ';++uc) {
    if (isspace(uc)) B64UTIL_ai8Char2B64[uc] = B64UTIL_nSpace;
  } /* for */

  /* Wert fuer alle base64 Zeichen einsetzen */
  for (i8=0;i8<(int8)sizeof(B64UTIL_acB642Char);i8++)
    B64UTIL_ai8Char2B64[(uchar)B64UTIL_acB642Char[i8]] = i8;

  /* Ende und Fuellzeichen markieren */
  B64UTIL_ai8Char2B64[(uchar)'\0'] = B64UTIL_nEnd;
  B64UTIL_ai8Char2B64[(uchar)B64UTIL_ncPad] = B64UTIL_nPad;

  /* Test */
  #if B64UTIL__DBG_Test
    B64UTIL__vTestB642Bin();
    B64UTIL__vTestB64Len();
    B64UTIL__vTestBin2B64();
  #endif /* B64UTIL__DBG_Test */
} /* B64UTIL_vInit */


/* B64UTIL_vDeinit: Modul deinitialisieren */
/* ------------------------------------- */
void stdcall B64UTIL_vDeinit(void) {
  TestModDeinit(B64UTIL);

} /* B64UTIL_vDeinit */

    char* II(int a) {

    }

/* EOF b64util.c */
