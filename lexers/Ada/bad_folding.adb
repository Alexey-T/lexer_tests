-- Prob with this file.
--main begin-end not folded.
--Intern. begin-end (with label DO_MDEV_FILE) not folded.

procedure INITIALIZE_DEVELOPER_PARAMETERS is
begin


  DO_MDEV_FILE:
  begin
  --  Read the MDEV file
    GET_MDEVS;
    PREFACE.PUT_LINE("MDEV_FILE found - Using those MDEVs and parameters");
  exception
  --  If there is any problem
  --  Put that the MDEV file is corrupted and the options are:
      --  to proceed with default parameters
      --  to set parameters with a CHANGE (SET) PARAMETERS and save
      --  to examine the MDEV file with a text editor and try to repair it
    when NAME_ERROR  =>
      WORDS_MDEV := DEFAULT_MDEV_ARRAY;
    when BAD_MDEV_FILE  =>
      PREFACE.PUT_LINE("MDEV_FILE exists, but empty or corupted - Default MDEVs used");
      PREFACE.PUT_LINE("You can set new parameters with CHANGE PARAMETERS and save.");
      WORDS_MDEV := DEFAULT_MDEV_ARRAY;
  end DO_MDEV_FILE;


--  if not IS_OPEN(DBG) and then WORDS_MDEV(HAVE_DEBUG_FILE)  then
--    CREATE(DBG, OUT_FILE, DEBUG_FULL_NAME);
--    PREFACE.PUT_LINE("WORD.DBG Created at Initialization");
--  end if;
  if not IS_OPEN(STATS) and then WORDS_MDEV(HAVE_STATISTICS_FILE)  then
    CREATE(STATS, OUT_FILE, STATS_FULL_NAME);
    PREFACE.PUT_LINE("WORD.STA Created at Initialization");
  end if;

end INITIALIZE_DEVELOPER_PARAMETERS;

