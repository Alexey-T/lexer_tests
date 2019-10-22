int stdcall CFGD_iInsertI(CFGD_tpstEntry pstListEntry,
                          CFGD_tpstEntry pstEntry,
                          int iUPos, int iSPos) {
  CFGD_tpstEntry        pstPrevEntry;   /* Voriger Eintrag */
  CFGD_tpstEntry        pstNextEntry;   /* Naechster Eintrag */
  SDL_tpstHdr           pstNameNrList;  /* Liste aus pstListEntry */
  DAP_tpstHdr           pstList;        /* Liste aus pstListEntry */
  bool                  boEquPrev;      /* Name = Prev */
  bool                  boEquNext;      /* Name = Next */
  size_t                xoElementCount; /* Anz. aus pstNameNrList */


  DUMMY_INIT_LOCAL_VARIABLE(pstPrevEntry,void*);
  DUMMY_INIT_LOCAL_VARIABLE(pstNextEntry,void*);

  #ifndef NDEBUG
    if (pstListEntry->en8TypeClass!=nenTCNone) {
      SetErrNo(EINVAL);
      return(0);
    }
  #endif


  return(iUPos);
} /* CFGD_iInsertI