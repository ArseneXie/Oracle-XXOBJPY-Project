CREATE OR REPLACE PACKAGE XX_OBJ_PY_PKG IS

  /**
  *Gen DDL and Setup Script via Python
  *@author <a href="mailto:arsene@readycom.com.tw">Arsene</a>
  *@version 0.1a,2014/12/30 under develop
  */

  C_NOLOG   CONSTANT NUMBER := 0;
  C_ERROR   CONSTANT NUMBER := 1;
  C_WARNING CONSTANT NUMBER := 2;
  C_LOG     CONSTANT NUMBER := 3;
  Z_DEBUG_LEVEL NUMBER := 2; --default debug level

  Z_DBMSOUT_ON BOOLEAN := FALSE;
  Z_FLOGOUT_ON BOOLEAN := FALSE;
  Z_FOUTOUT_ON BOOLEAN := FALSE;
  Z_FILEOUT_ON BOOLEAN := FALSE;
  Z_UTL_FILE   UTL_FILE.FILE_TYPE;

  TYPE T_EXT_CODE IS TABLE OF VARCHAR2(10) INDEX BY VARCHAR2(20);
  Z_EXT_LIST T_EXT_CODE;

  C_SYNONYM_TYPE CONSTANT VARCHAR2(10) := 'SYNONYM';
  C_FNDLOAD_TYPE CONSTANT VARCHAR2(10) := 'FNDLOAD';

  Z_PACKAGE_EXT VARCHAR2(5) := '.pck';
  Z_TRIGGER_EXT VARCHAR2(5) := '.trg';
  Z_TABLE_EXT   VARCHAR2(5) := '.sql';
  Z_VIEW_EXT    VARCHAR2(5) := '.sql';
  Z_MVIEW_EXT   VARCHAR2(5) := '.sql';
  Z_INDEX_EXT   VARCHAR2(5) := '.sql';
  Z_SQL_EXT     VARCHAR2(5) := '.sql';
  Z_JAVASRC_EXT VARCHAR2(5) := '.jsp';

  Z_BLANK_LINE    VARCHAR2(5) := '' || CHR(10);
  Z_TAB           VARCHAR2(10) := '   ';
  Z_DROP_TABLE    BOOLEAN := FALSE;
  Z_DROP_SEQUENCE BOOLEAN := FALSE;
  Z_DROP_SYNONYM  BOOLEAN := FALSE;
  Z_DROP_INDEX    BOOLEAN := FALSE;
  Z_DROP_MVIEW    BOOLEAN := FALSE;
  Z_DEF_SCHEMA    VARCHAR2(10) := 'APPS';
  Z_WITH_SCHEMA   BOOLEAN := FALSE;
  Z_GEN_TAB_TRG   BOOLEAN := TRUE;
  Z_AUTHOR        VARCHAR2(50) := 'Arsene';
  Z_ZIP_SOURCE    BOOLEAN := FALSE;
  Z_MAIL_ATTCH    BOOLEAN := FALSE;
  Z_MAIL_SUBJECT  VARCHAR2(300);
  Z_EMAIL         VARCHAR2(100);
  Z_SMTP_SRV      VARCHAR2(100);
  Z_SENDER        VARCHAR2(100) := 'no-reply@xxobj.auto.ddl';
  Z_TIME_CODE     VARCHAR2(100) := TO_CHAR(SYSDATE, 'YYYYMMDDHH24MISS');
  Z_USER_ID       NUMBER := 0;
  Z_CUST_ID       NUMBER := 0;
  Z_FNDLD_FPWD    VARCHAR2(100) := 'apps';
  Z_FNDLD_TPWD    VARCHAR2(100) := 'apps';

  C_CONC_LCT    CONSTANT VARCHAR2(50) := '$FND_TOP/patch/115/import/afcpprog.lct';
  C_FORM_LCT    CONSTANT VARCHAR2(50) := '$FND_TOP/patch/115/import/afsload.lct';
  C_MENU_LCT    CONSTANT VARCHAR2(50) := '$FND_TOP/patch/115/import/afsload.lct';
  C_REQGRP_LCT  CONSTANT VARCHAR2(50) := '$FND_TOP/patch/115/import/afcpreqg.lct';
  C_LOOKUP_LCT  CONSTANT VARCHAR2(50) := '$FND_TOP/patch/115/import/aflvmlu.lct';
  C_PROFILE_LCT CONSTANT VARCHAR2(50) := '$FND_TOP/patch/115/import/afscprof.lct';
  C_XMLP_LCT    CONSTANT VARCHAR2(50) := '$XDO_TOP/patch/115/import/xdotmpl.lct';
  C_FRMCUS_LCT  CONSTANT VARCHAR2(50) := '$FND_TOP/patch/115/import/affrmcus.lct'; --form perionalization

  Z_DDL_PREFIX CONSTANT VARCHAR2(10) := '#';
  Z_DDL_TYPE   CONSTANT VARCHAR2(10) := 'ddl';
  Z_FNDLD_TYPE CONSTANT VARCHAR2(10) := 'fndload';
  Z_DOWNLOAD   CONSTANT VARCHAR2(10) := 'DOWNLOAD';
  Z_UPLOAD     CONSTANT VARCHAR2(10) := 'UPLOAD';

  Z_SPLIT_CHAR VARCHAR2(1) := ',';

  TYPE T_OBJINFO IS VARRAY(10) OF VARCHAR2(100);
  TYPE ATTACHMENTS_LIST IS TABLE OF VARCHAR2(4000);

  PROCEDURE SET_AUTHOR(P_AUTHOR IN VARCHAR2);

  FUNCTION STRING_TO_OBJINFO(P_STRING     IN VARCHAR2,
                             P_SPLIT_CHAR IN VARCHAR2) RETURN T_OBJINFO;

  FUNCTION REPLACECLOB(SRCCLOB     IN CLOB,
                       REPLACESTR  IN VARCHAR2,
                       REPLACEWITH IN VARCHAR2,
                       P_SUBTITLE  IN VARCHAR2,
                       P_ADD_DDL   IN VARCHAR2) RETURN CLOB;

  FUNCTION GET_XX_OBJ_ID RETURN NUMBER;

  PROCEDURE REC_CFG_PARAM(P_XX_ID      IN NUMBER,
                          P_PARAM_NAME IN VARCHAR2,
                          P_PARAM_VAL  IN VARCHAR2);

  PROCEDURE REC_SCHEMA_INFO(P_XX_ID          IN NUMBER,
                            P_SCHEMA_NAME    IN VARCHAR2,
                            P_SCHEMA_PASSWD  IN VARCHAR2,
                            P_SCHEMA_DATA_TS IN VARCHAR2,
                            P_SCHEMA_IDX_TS  IN VARCHAR2);

  PROCEDURE REC_CUST_OBJ(P_XX_ID          IN NUMBER,
                         P_CUST_TYPE      IN VARCHAR2,
                         P_PROGRAM_CODE   IN VARCHAR2,
                         P_OBJECT_TYPE    IN VARCHAR2,
                         P_OBJECT_NAME    IN VARCHAR2,
                         P_OBJECT_SCHEMA  IN VARCHAR2,
                         P_SYNONYM_SCHEMA IN VARCHAR2);

  PROCEDURE GEN_OBJ_DDL_LIST(P_XX_ID IN NUMBER);

  FUNCTION GET_FILE_NAME(P_OBJ_NAME   IN VARCHAR2,
                         P_EXT        IN VARCHAR2,
                         PROGRAM_CODE IN VARCHAR2) RETURN VARCHAR2;

  FUNCTION GET_SUBTITLE(P_SUBTITLE IN VARCHAR2) RETURN VARCHAR2;

  PROCEDURE SCHEMA_INFO_INIT;

  PROCEDURE CUST_OBJS_INIT;

  PROCEDURE GEN_DDL_SUBTITLE(P_FILE          IN UTL_FILE.FILE_TYPE,
                             P_SUBTITLE_NAME IN VARCHAR2);

  FUNCTION GET_OBJ_DDL(P_OBJ_TYPE IN VARCHAR2, P_OBJ_NAME IN VARCHAR2)
    RETURN CLOB;

  FUNCTION GET_TABLE_DDL(P_OBJ_NAME IN VARCHAR2) RETURN CLOB;

  FUNCTION GET_PACKAGE_DDL(P_OBJ_NAME IN VARCHAR2) RETURN CLOB;

  FUNCTION GET_IDX_DDL_STMT(P_OBJ_NAME   IN VARCHAR2,
                            P_OBJ_SCHEMA IN VARCHAR2) RETURN VARCHAR2;

  FUNCTION GET_VIEW_DDL(P_OBJ_NAME IN VARCHAR2) RETURN CLOB;

  PROCEDURE GEN_MVIEW_DDL(P_OBJ_NAME IN VARCHAR2);

  PROCEDURE GEN_TRG_DDL(P_OBJ_NAME IN VARCHAR2);

  PROCEDURE GEN_IDX_DDL(P_OBJ_NAME IN VARCHAR2);

  FUNCTION GET_SEQUENCE_DDL(P_OBJ_NAME IN VARCHAR2) RETURN CLOB;

  PROCEDURE GEN_JAVASRC_DDL(P_OBJ_NAME IN VARCHAR2);

  PROCEDURE GEN_CONC_IDT(P_OBJ_NAME IN VARCHAR2);

  PROCEDURE GEN_FORM_IDT(P_OBJ_NAME IN VARCHAR2);

  FUNCTION GET_SYNONYM_DDL RETURN CLOB;

  FUNCTION GET_FNDLOAD_SCPT(P_OBJ_NAME IN VARCHAR2) RETURN CLOB;

  FUNCTION GET_FIDX_COL(P_COL_NAME IN VARCHAR2,
                        P_OWNER    IN VARCHAR2,
                        P_TABLE    IN VARCHAR2) RETURN VARCHAR2;

  PROCEDURE RECORD_DDL_LIST(P_OWNER        IN VARCHAR2,
                            P_PROGRAM_CODE IN VARCHAR2,
                            P_FILE_NAME    IN VARCHAR2,
                            P_OBJ_TYPE     IN VARCHAR2,
                            P_OBJ_NAME     IN VARCHAR2);

  PROCEDURE APPS_INIT(P_USER_ID IN NUMBER);

  PROCEDURE CALL_FNDLOAD(P_LCT_SRC  IN VARCHAR2,
                         P_LDT_DEST IN VARCHAR2,
                         P_TYPE_ARG IN VARCHAR2,
                         P_APPL_ARG IN VARCHAR2,
                         P_OBJ_ARG  IN VARCHAR2);

  FUNCTION GET_PKG_CLOB RETURN CLOB;

  PROCEDURE PURGE_TEMP_TABLES;

/*
  Create  Sequence XX_CUST_OBJS_S;
  
  Create  Table XX_CUST_OBJ_PARAM_TMP
  (
  XX_CUST_ID NUMBER,
  PARAM_NAME VARCHAR2(50),
  PARAM_VAL VARCHAR2(50)
  );
  
  Create  Table XX_CUST_OBJS_TMP
  (
  XX_CUST_ID NUMBER,
  XX_CUST_TYPE VARCHAR2(10),
  GEN_DDL_SEQ NUMBER,
  PROGRAM_CODE VARCHAR2(50),
  OBJECT_TYPE VARCHAR2(30),
  OBJECT_NAME VARCHAR2(200),
  OBJECT_SCHEMA VARCHAR2(10),
  SYNONYM_SCHEMA VARCHAR2(10)
  );
  
  Create  Table XX_SCHEMA_INFO_TMP
  (
  XX_CUST_ID NUMBER,
  SCHEMA_NAME VARCHAR2(10),
  SCHEMA_PASSWD VARCHAR2(30),
  SCHEMA_DATA_TS VARCHAR2(30),
  SCHEMA_IDX_TS VARCHAR2(30)
  );
  
  Create  Table XX_DDL_SCRIPT_LIST_TMP
  (
  XX_CUST_ID NUMBER,
  OWNER VARCHAR2(10),
  PROGRAM_CODE VARCHAR2(50),
  SCRIPT_FILE_NAME VARCHAR2(300),
  OBJECT_TYPE VARCHAR2(100),
  OBJECT_NAME VARCHAR2(100)
  );
  
  Create  Table XX_FNDLOAD_REQUEST_TMP
  (
  XX_CUST_ID NUMBER,
  REQUEST_ID NUMBER
  );
  
  Quick Gen Object List
  SELECT '#,XXAPSF0067,' || OBJECT_TYPE || ',' || OBJECT_NAME || ',' || OWNER ||
         ',APPS'
    FROM ALL_OBJECTS
   WHERE OBJECT_NAME LIKE UPPER('XX%APS%KPN%')
     AND OBJECT_TYPE <> 'SYNONYM'
   ORDER BY DECODE(OBJECT_TYPE, 'TABLE', 1, 'SEQUENCE', 2, 3);
  */

END XX_OBJ_PY_PKG;
/
CREATE OR REPLACE PACKAGE BODY XX_OBJ_PY_PKG IS

  PROCEDURE LOG(P_MSG_TYPE IN NUMBER, P_MESSAGE IN VARCHAR2) IS
    V_CODE   VARCHAR2(10);
    V_OUTMSG VARCHAR2(4000);
  BEGIN
    CASE P_MSG_TYPE
      WHEN C_ERROR THEN
        V_CODE := 'ERROR: ';
      WHEN C_WARNING THEN
        V_CODE := 'WARNING: ';
      ELSE
        V_CODE := 'LOG: ';
    END CASE;
  
    V_OUTMSG := V_CODE || P_MESSAGE;
  
    IF P_MSG_TYPE <= Z_DEBUG_LEVEL THEN
      IF Z_DBMSOUT_ON THEN
        DBMS_OUTPUT.PUT_LINE(V_OUTMSG);
      END IF;
      IF Z_FLOGOUT_ON THEN
        FND_FILE.PUT_LINE(FND_FILE.LOG, V_OUTMSG);
      END IF;
      IF Z_FOUTOUT_ON THEN
        FND_FILE.PUT_LINE(FND_FILE.OUTPUT, V_OUTMSG);
      END IF;
      IF Z_FILEOUT_ON THEN
        UTL_FILE.PUT_LINE(Z_UTL_FILE, V_OUTMSG);
      END IF;
    END IF;
  END LOG;

  FUNCTION GET_PKG_SUBPROG(P_PROGRAM_NAME IN VARCHAR2, P_LINE IN NUMBER)
    RETURN VARCHAR2 IS
    V_RETURN VARCHAR2(100);
  BEGIN
    SELECT DECODE(POSITION,
                  0,
                  REPLACE(REPLACE(TEXT, ' IS', ''), ' AS', ''),
                  SUBSTR(TEXT, 1, POSITION - 1)) PROG_NAME
      INTO V_RETURN
      FROM (SELECT LINE, TEXT, INSTR(TEXT, '(') POSITION
              FROM (SELECT LINE,
                           LTRIM(LTRIM(UPPER(LTRIM(TEXT)), 'PROCEDURE')) TEXT
                      FROM ALL_SOURCE
                     WHERE NAME = P_PROGRAM_NAME
                       AND LINE <= P_LINE
                       AND TYPE = 'PACKAGE BODY'
                       AND LTRIM(TEXT) LIKE 'PROCEDURE%'
                    UNION
                    SELECT LINE,
                           LTRIM(LTRIM(UPPER(LTRIM(TEXT)), 'FUNCTION')) TEXT
                      FROM ALL_SOURCE
                     WHERE NAME = P_PROGRAM_NAME
                       AND LINE <= P_LINE
                       AND TYPE = 'PACKAGE BODY'
                       AND UPPER(LTRIM(TEXT)) LIKE 'FUNCTION%'
                     ORDER BY LINE DESC)
             WHERE ROWNUM = 1);
  
    RETURN P_PROGRAM_NAME || '.' || V_RETURN;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN P_PROGRAM_NAME;
  END GET_PKG_SUBPROG;

  PROCEDURE LOG_EXP_ERROR(P_PROG_UNIT      IN VARCHAR2,
                          P_EXP_LINE       IN NUMBER,
                          P_ADD_MSG        IN VARCHAR2 DEFAULT '',
                          P_SQLERRM_FLAG   IN VARCHAR2 DEFAULT 'Y',
                          P_SQLERRM_MSG    IN VARCHAR2 DEFAULT '',
                          P_BACKTRACE_FLAG IN VARCHAR2 DEFAULT 'Y') IS
  
    V_ERROR_MSG VARCHAR2(1000);
  BEGIN
    V_ERROR_MSG := '@' || GET_PKG_SUBPROG(P_PROG_UNIT, P_EXP_LINE) || ', ' ||
                   P_ADD_MSG;
    LOG(C_ERROR, V_ERROR_MSG);
    IF P_SQLERRM_FLAG = 'Y' THEN
      LOG(C_ERROR, 'SQLERRM:' || P_SQLERRM_MSG);
    END IF;
  
    IF P_BACKTRACE_FLAG = 'Y' THEN
      LOG(C_ERROR, DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
    END IF;
  END LOG_EXP_ERROR;

  /**
  * set Z_AUTHOR 
  * @param P_AUTHOR VARCHAR2 given author information
  * @see GET_SUBTITLE
  */
  PROCEDURE SET_AUTHOR(P_AUTHOR IN VARCHAR2) IS
  BEGIN
    Z_AUTHOR := P_AUTHOR;
  END SET_AUTHOR;

  /**
  * let A,B,C be OBJINFO ARRAY, Due to apex_util.string_to_table is only available over 11g
  * @param P_STRING VARCHAR2 strings with split charater
  * @param P_SPLIT_CHAR VARCHAR2 split character
  * @return T_OBJINFO object array as varchar2 array
  */
  FUNCTION STRING_TO_OBJINFO(P_STRING     IN VARCHAR2,
                             P_SPLIT_CHAR IN VARCHAR2) RETURN T_OBJINFO IS
    V_OBJ_REC   T_OBJINFO;
    V_READ      VARCHAR2(1000);
    V_REMAIN    VARCHAR2(4000);
    V_I         NUMBER;
    V_SPLIT_POS NUMBER;
    V_EXIT      BOOLEAN := FALSE;
  BEGIN
    V_REMAIN  := P_STRING;
    V_OBJ_REC := T_OBJINFO();
    V_I       := 1;
    WHILE NOT V_EXIT AND V_I < 1000 LOOP
      V_SPLIT_POS := INSTR(V_REMAIN, P_SPLIT_CHAR, 1, 1);
      IF V_SPLIT_POS > 0 THEN
        V_READ   := SUBSTR(V_REMAIN, 1, V_SPLIT_POS - 1);
        V_REMAIN := SUBSTR(V_REMAIN, V_SPLIT_POS + 1);
      ELSE
        V_READ := V_REMAIN;
        V_EXIT := TRUE;
      END IF;
      V_OBJ_REC.EXTEND;
      V_OBJ_REC(V_I) := V_READ;
      V_I := V_I + 1;
    END LOOP;
  
    RETURN V_OBJ_REC;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN T_OBJINFO();
  END STRING_TO_OBJINFO;

  /**
  * replace strings in clob object, the main purpose is to replace SCHEMA info
  * @param SRCCLOB CLOB clob object
  * @param REPLACESTR VARCHAR2 the old string want to be replaced
  * @param REPLACEWITH VARCHAR2 the new string to replace with
  * @param P_SUBTITLE VARCHAR2 to generate the subtitle string in front of the ddl file, this string will be added during replace
  * @param P_ADD_DDL VARCHAR2 additional ddl statement in end of this ddl file, this string will be added during replace
  * @return CLOB the clob object be replaced
  */
  FUNCTION REPLACECLOB(SRCCLOB     IN CLOB,
                       REPLACESTR  IN VARCHAR2,
                       REPLACEWITH IN VARCHAR2,
                       P_SUBTITLE  IN VARCHAR2,
                       P_ADD_DDL   IN VARCHAR2) RETURN CLOB IS
    L_BUFFER    VARCHAR2(32767);
    L_AMOUNT    NUMBER := 32000;
    L_POS       NUMBER := 1;
    L_CLOB_LEN  NUMBER;
    NEWCLOB     CLOB := EMPTY_CLOB;
    V_SKIP_FLAG BOOLEAN := FALSE;
  
  BEGIN
    DBMS_LOB.CREATETEMPORARY(NEWCLOB, TRUE);
    IF LENGTH(P_SUBTITLE) > 0 THEN
      L_BUFFER := GET_SUBTITLE(P_SUBTITLE);
      DBMS_LOB.WRITEAPPEND(NEWCLOB, LENGTH(L_BUFFER), L_BUFFER);
    END IF;
    IF REPLACESTR = REPLACEWITH THEN
      V_SKIP_FLAG := TRUE;
    END IF;
    L_CLOB_LEN := DBMS_LOB.GETLENGTH(SRCCLOB);
    WHILE L_POS < L_CLOB_LEN LOOP
    
      DBMS_LOB.READ(SRCCLOB, L_AMOUNT, L_POS, L_BUFFER);
      IF L_BUFFER IS NOT NULL THEN
        IF NOT V_SKIP_FLAG THEN
          -- replace the text
          L_BUFFER := REPLACE(L_BUFFER, REPLACESTR, REPLACEWITH);
        END IF;
        -- write it to the new clob
        DBMS_LOB.WRITEAPPEND(NEWCLOB, LENGTH(L_BUFFER), L_BUFFER);
      END IF;
      L_POS := L_POS + L_AMOUNT;
    
    END LOOP;
  
    IF LENGTH(P_ADD_DDL) > 0 THEN
      DBMS_LOB.WRITEAPPEND(NEWCLOB, LENGTH(P_ADD_DDL), P_ADD_DDL);
    END IF;
  
    RETURN NEWCLOB;
  
  EXCEPTION
    WHEN OTHERS THEN
      RAISE;
  END;

  FUNCTION GET_XX_OBJ_ID RETURN NUMBER IS
    V_RETURN NUMBER;
  BEGIN
    SELECT XX_CUST_OBJS_S.NEXTVAL INTO V_RETURN FROM DUAL;
    RETURN V_RETURN;
  END GET_XX_OBJ_ID;

  PROCEDURE REC_CFG_PARAM(P_XX_ID      IN NUMBER,
                          P_PARAM_NAME IN VARCHAR2,
                          P_PARAM_VAL  IN VARCHAR2) IS
  BEGIN
    INSERT INTO XX_CUST_OBJ_PARAM_TMP
      (XX_CUST_ID, PARAM_NAME, PARAM_VAL)
    VALUES
      (P_XX_ID, P_PARAM_NAME, P_PARAM_VAL);
  
  END REC_CFG_PARAM;

  PROCEDURE REC_SCHEMA_INFO(P_XX_ID          IN NUMBER,
                            P_SCHEMA_NAME    IN VARCHAR2,
                            P_SCHEMA_PASSWD  IN VARCHAR2,
                            P_SCHEMA_DATA_TS IN VARCHAR2,
                            P_SCHEMA_IDX_TS  IN VARCHAR2) IS
  BEGIN
    INSERT INTO XX_SCHEMA_INFO_TMP
      (XX_CUST_ID,
       SCHEMA_NAME,
       SCHEMA_PASSWD,
       SCHEMA_DATA_TS,
       SCHEMA_IDX_TS)
    VALUES
      (P_XX_ID,
       P_SCHEMA_NAME,
       P_SCHEMA_PASSWD,
       P_SCHEMA_DATA_TS,
       P_SCHEMA_IDX_TS);
  
  END REC_SCHEMA_INFO;

  PROCEDURE REC_CUST_OBJ(P_XX_ID          IN NUMBER,
                         P_CUST_TYPE      IN VARCHAR2,
                         P_PROGRAM_CODE   IN VARCHAR2,
                         P_OBJECT_TYPE    IN VARCHAR2,
                         P_OBJECT_NAME    IN VARCHAR2,
                         P_OBJECT_SCHEMA  IN VARCHAR2,
                         P_SYNONYM_SCHEMA IN VARCHAR2) IS
  BEGIN
    INSERT INTO XX_CUST_OBJS_TMP
      (XX_CUST_ID,
       XX_CUST_TYPE,
       PROGRAM_CODE,
       OBJECT_TYPE,
       OBJECT_NAME,
       OBJECT_SCHEMA,
       SYNONYM_SCHEMA)
    VALUES
      (P_XX_ID,
       DECODE(P_CUST_TYPE, Z_DDL_PREFIX, Z_DDL_TYPE, Z_FNDLD_TYPE),
       P_PROGRAM_CODE,
       P_OBJECT_TYPE,
       P_OBJECT_NAME,
       P_OBJECT_SCHEMA,
       P_SYNONYM_SCHEMA);
  
  END REC_CUST_OBJ;

  PROCEDURE INIT_EXT_CODE IS
  BEGIN
    Z_EXT_LIST('PACKAGE') := '.pck';
    Z_EXT_LIST('TABLE') := '.sql';
    Z_EXT_LIST('VIEW') := '.sql';
    Z_EXT_LIST('MVIEW') := '.sql';
    Z_EXT_LIST('INDEX') := '.sql';
    Z_EXT_LIST('SYNONYM') := '.sql';
    Z_EXT_LIST('SEQUENCE') := '.sql';
    Z_EXT_LIST('SQL') := '.sql';
    Z_EXT_LIST('JAVASRC') := '.jsp';
    Z_EXT_LIST('FNDLOAD') := '.ldt';
  END INIT_EXT_CODE;

  PROCEDURE INIT_PARAM_VARS IS
    CURSOR CUR_PARAM IS
      SELECT PARAM_NAME, PARAM_VAL
        FROM XX_CUST_OBJ_PARAM_TMP
       WHERE XX_CUST_ID = Z_CUST_ID;
  BEGIN
    FOR REC IN CUR_PARAM LOOP
      IF REC.PARAM_NAME = 'DROP_TABLE' THEN
        Z_DROP_TABLE := CASE
                          WHEN REC.PARAM_VAL = 'Y' THEN
                           TRUE
                          ELSE
                           FALSE
                        END;
      ELSIF REC.PARAM_NAME = 'DROP_SEQUENCE' THEN
        Z_DROP_SEQUENCE := CASE
                             WHEN REC.PARAM_VAL = 'Y' THEN
                              TRUE
                             ELSE
                              FALSE
                           END;
      ELSIF REC.PARAM_NAME = 'DROP_SYNONYM' THEN
        Z_DROP_SYNONYM := CASE
                            WHEN REC.PARAM_VAL = 'Y' THEN
                             TRUE
                            ELSE
                             FALSE
                          END;
      ELSIF REC.PARAM_NAME = 'DROP_INDEX' THEN
        Z_DROP_INDEX := CASE
                          WHEN REC.PARAM_VAL = 'Y' THEN
                           TRUE
                          ELSE
                           FALSE
                        END;
      ELSIF REC.PARAM_NAME = 'DROP_MVIEW' THEN
        Z_DROP_MVIEW := CASE
                          WHEN REC.PARAM_VAL = 'Y' THEN
                           TRUE
                          ELSE
                           FALSE
                        END;
      ELSIF REC.PARAM_NAME = 'FNDLOAD_FPWD' THEN
        Z_FNDLD_FPWD := REC.PARAM_VAL;
        Z_FNDLD_TPWD := Z_FNDLD_FPWD;
      ELSIF REC.PARAM_NAME = 'FNDLOAD_TPWD' THEN
        Z_FNDLD_TPWD := REC.PARAM_VAL;
      ELSIF REC.PARAM_NAME = 'WITH_SCHEMA' THEN
        Z_WITH_SCHEMA := CASE
                           WHEN REC.PARAM_VAL = 'Y' THEN
                            TRUE
                           ELSE
                            FALSE
                         END;
      ELSIF REC.PARAM_NAME = 'GEN_TAB_TRG' THEN
        Z_GEN_TAB_TRG := CASE
                           WHEN REC.PARAM_VAL = 'Y' THEN
                            TRUE
                           ELSE
                            FALSE
                         END;
      ELSIF REC.PARAM_NAME = 'USER_NAME' THEN
        BEGIN
          SELECT USER_ID
            INTO Z_USER_ID
            FROM FND_USER
           WHERE USER_NAME = UPPER(REC.PARAM_VAL);
        EXCEPTION
          WHEN OTHERS THEN
            Z_USER_ID := 0;
        END;
      ELSIF REC.PARAM_NAME = 'ZIP_SOURCE' THEN
        IF Z_MAIL_ATTCH THEN
          Z_ZIP_SOURCE := TRUE;
        ELSE
          Z_ZIP_SOURCE := CASE
                            WHEN REC.PARAM_VAL = 'Y' THEN
                             TRUE
                            ELSE
                             FALSE
                          END;
        END IF;
      END IF;
    END LOOP;
  END INIT_PARAM_VARS;

  PROCEDURE GEN_OBJ_DDL_LIST(P_XX_ID IN NUMBER) IS
    CURSOR CUR_OBJ_LIST IS
      SELECT OBJECT_TYPE, OBJECT_NAME, OBJECT_SCHEMA, PROGRAM_CODE
        FROM XX_CUST_OBJS_TMP
       WHERE XX_CUST_ID = Z_CUST_ID
         AND XX_CUST_TYPE = Z_DDL_TYPE
         AND OBJECT_TYPE <> C_SYNONYM_TYPE;
  
    CURSOR CUR_SYN_SCH IS
      SELECT DISTINCT PROGRAM_CODE, SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = C_SYNONYM_TYPE
         AND XX_CUST_TYPE = Z_DDL_TYPE
         AND XX_CUST_ID = Z_CUST_ID;
  
    CURSOR CUR_FNDLOAD IS
      SELECT DISTINCT PROGRAM_CODE
        FROM XX_CUST_OBJS_TMP
       WHERE XX_CUST_TYPE = Z_FNDLD_TYPE
         AND XX_CUST_ID = Z_CUST_ID;
  
    V_FILE_NAME VARCHAR2(100);
  BEGIN
    Z_CUST_ID := P_XX_ID;
    INIT_EXT_CODE;
    INIT_PARAM_VARS;
    CUST_OBJS_INIT;
    SCHEMA_INFO_INIT;
  
    FOR REC IN CUR_OBJ_LIST LOOP
      RECORD_DDL_LIST(REC.OBJECT_SCHEMA,
                      REC.PROGRAM_CODE,
                      GET_FILE_NAME(REC.OBJECT_NAME,
                                    Z_EXT_LIST(REC.OBJECT_TYPE),
                                    REC.PROGRAM_CODE),
                      REC.OBJECT_TYPE,
                      REC.OBJECT_NAME);
      --GEN_SYN_DDL;
    END LOOP;
  
    FOR RSS IN CUR_SYN_SCH LOOP
      V_FILE_NAME := GET_FILE_NAME(RSS.SYNONYM_SCHEMA || '_' ||
                                   C_SYNONYM_TYPE,
                                   Z_EXT_LIST(C_SYNONYM_TYPE),
                                   RSS.PROGRAM_CODE);
    
      RECORD_DDL_LIST(RSS.SYNONYM_SCHEMA,
                      RSS.PROGRAM_CODE,
                      V_FILE_NAME,
                      C_SYNONYM_TYPE,
                      RSS.SYNONYM_SCHEMA);
    END LOOP;
  
    --GEN FNDLoad Script
    FOR RFL IN CUR_FNDLOAD LOOP
      RECORD_DDL_LIST(C_FNDLOAD_TYPE,
                      RFL.PROGRAM_CODE,
                      RFL.PROGRAM_CODE || '_DOWNLOAD.txt',
                      C_FNDLOAD_TYPE,
                      Z_DOWNLOAD);
    
      RECORD_DDL_LIST(C_FNDLOAD_TYPE,
                      RFL.PROGRAM_CODE,
                      RFL.PROGRAM_CODE || '_UPLOAD.txt',
                      C_FNDLOAD_TYPE,
                      Z_UPLOAD);
    END LOOP;
  END GEN_OBJ_DDL_LIST;

  /**
  * Generate ddl file name, the rule of filename could be modified here centrally
  * @param P_OBJ_NAME VARCHAR2 object name 
  * @param P_EXT VARCHAR2 file extension
  * @param PROGRAM_CODE VARCHAR2 file extension
  * @return VARCHAR2 the file name
  */
  FUNCTION GET_FILE_NAME(P_OBJ_NAME   IN VARCHAR2,
                         P_EXT        IN VARCHAR2,
                         PROGRAM_CODE IN VARCHAR2) RETURN VARCHAR2 IS
    V_RETURN VARCHAR2(200);
  BEGIN
    V_RETURN := P_OBJ_NAME || P_EXT;
    RETURN V_RETURN;
  END GET_FILE_NAME;

  /**
  * Generate ddl subtitle string, the rule of subtitle string could be modified here centrally
  * @param P_SUBTITLE VARCHAR2 basic subtitle 
  * @return VARCHAR2 the subtitle string with specific format and include generate date and some common information
  */
  FUNCTION GET_SUBTITLE(P_SUBTITLE IN VARCHAR2) RETURN VARCHAR2 IS
    V_RETURN VARCHAR2(200);
  BEGIN
    V_RETURN := '/******  ' || P_SUBTITLE || ' ***gen@' ||
                TO_CHAR(SYSDATE, 'YYYY/MM/DD HH24:MI:SS') || ' by ' ||
                Z_AUTHOR || '*****/';
    RETURN V_RETURN;
  END GET_SUBTITLE;

  /**
  * finish schema information(passwd and tablespace) with spcific algorithm, and display final schema information as log message
  */
  PROCEDURE SCHEMA_INFO_INIT IS
    V_DATA_TS VARCHAR2(50);
    V_IDX_TS  VARCHAR2(50);
  
    CURSOR CUR_FILL_SCHEMA IS
      SELECT OBJECT_SCHEMA SCHEMA_NAME
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_SCHEMA IS NOT NULL
         AND XX_CUST_ID = Z_CUST_ID
      MINUS
      SELECT SCHEMA_NAME
        FROM XX_SCHEMA_INFO_TMP
       WHERE XX_CUST_ID = Z_CUST_ID;
  
    CURSOR CUR_DATA_TS_NULL IS
      SELECT SCHEMA_NAME, SCHEMA_DATA_TS
        FROM XX_SCHEMA_INFO_TMP
       WHERE SCHEMA_DATA_TS IS NULL
         AND XX_CUST_ID = Z_CUST_ID
         FOR UPDATE OF SCHEMA_DATA_TS;
  
    CURSOR CUR_IDX_TS_NULL IS
      SELECT SCHEMA_NAME, SCHEMA_IDX_TS
        FROM XX_SCHEMA_INFO_TMP
       WHERE SCHEMA_IDX_TS IS NULL
         AND XX_CUST_ID = Z_CUST_ID
         FOR UPDATE OF SCHEMA_IDX_TS;
  
    CURSOR CUR_SCHEMA_INFO IS
      SELECT SCHEMA_NAME, SCHEMA_PASSWD, SCHEMA_DATA_TS, SCHEMA_IDX_TS
        FROM XX_SCHEMA_INFO_TMP
       WHERE XX_CUST_ID = Z_CUST_ID;
  BEGIN
    --fill SCHEMA
    FOR RF IN CUR_FILL_SCHEMA LOOP
      INSERT INTO XX_SCHEMA_INFO_TMP
        (XX_CUST_ID,
         SCHEMA_NAME,
         SCHEMA_PASSWD,
         SCHEMA_DATA_TS,
         SCHEMA_IDX_TS)
      VALUES
        (Z_CUST_ID, RF.SCHEMA_NAME, NULL, NULL, NULL);
    END LOOP;
    --set passwd
    UPDATE XX_SCHEMA_INFO_TMP
       SET SCHEMA_PASSWD = CHR(38) || 'PASSWD_OF_' || SCHEMA_NAME
     WHERE SCHEMA_PASSWD IS NULL
       AND XX_CUST_ID = Z_CUST_ID;
  
    --set DATA TS 
    FOR RDT IN CUR_DATA_TS_NULL LOOP
      V_DATA_TS := NULL;
      BEGIN
        SELECT DEFAULT_TABLESPACE
          INTO V_DATA_TS
          FROM DBA_USERS
         WHERE USERNAME = RDT.SCHEMA_NAME;
      EXCEPTION
        WHEN OTHERS THEN
          SELECT DEFAULT_TABLESPACE
            INTO V_DATA_TS
            FROM DBA_USERS
           WHERE USERNAME = Z_DEF_SCHEMA;
      END;
      IF V_DATA_TS IS NOT NULL THEN
        UPDATE XX_SCHEMA_INFO_TMP
           SET SCHEMA_DATA_TS = V_DATA_TS
         WHERE CURRENT OF CUR_DATA_TS_NULL;
      ELSE
        LOG(C_WARNING,
            'Schema TableSpace Not Found, schema: ' || RDT.SCHEMA_NAME);
      END IF;
    END LOOP;
  
    --ser IDX TS
    FOR RIT IN CUR_IDX_TS_NULL LOOP
      V_IDX_TS := NULL;
      BEGIN
        SELECT TABLESPACE_NAME
          INTO V_IDX_TS
          FROM (SELECT TABLESPACE_NAME
                  FROM ALL_INDEXES
                 WHERE OWNER = RIT.SCHEMA_NAME
                 ORDER BY INSTR(TABLESPACE_NAME, 'IDX') DESC,
                          TABLESPACE_NAME)
         WHERE ROWNUM = 1;
      EXCEPTION
        WHEN OTHERS THEN
          BEGIN
            SELECT DEFAULT_TABLESPACE
              INTO V_IDX_TS
              FROM DBA_USERS
             WHERE USERNAME = RIT.SCHEMA_NAME;
          EXCEPTION
            WHEN OTHERS THEN
              SELECT DEFAULT_TABLESPACE
                INTO V_IDX_TS
                FROM DBA_USERS
               WHERE USERNAME = Z_DEF_SCHEMA;
          END;
      END;
      IF V_IDX_TS IS NOT NULL THEN
        UPDATE XX_SCHEMA_INFO_TMP
           SET SCHEMA_IDX_TS = V_IDX_TS
         WHERE CURRENT OF CUR_IDX_TS_NULL;
      ELSE
        LOG(C_WARNING,
            'Schema Index TableSpace Not Found, schema: ' ||
            RIT.SCHEMA_NAME);
      END IF;
    END LOOP;
  
    FOR RS IN CUR_SCHEMA_INFO LOOP
      LOG(C_LOG,
          '@@@: ' || RS.SCHEMA_NAME || ' - ' || RS.SCHEMA_PASSWD || ' - ' ||
          RS.SCHEMA_DATA_TS || ' - ' || RS.SCHEMA_IDX_TS);
    END LOOP;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_SQLERRM_MSG => SQLERRM);
  END SCHEMA_INFO_INIT;

  /**
  * final object information(schema, synonym schema) with spcific algorithm, and display final object information as log message
  */
  PROCEDURE CUST_OBJS_INIT IS
    CURSOR CUR_TAB_TRG IS
      SELECT T.TRIGGER_NAME, O.PROGRAM_CODE, O.SYNONYM_SCHEMA
        FROM ALL_TRIGGERS T, XX_CUST_OBJS_TMP O
       WHERE O.OBJECT_TYPE = 'TABLE'
         AND O.OBJECT_NAME = T.TABLE_NAME
         AND O.XX_CUST_ID = Z_CUST_ID
         AND NOT EXISTS (SELECT 1
                FROM XX_CUST_OBJS_TMP S
               WHERE S.OBJECT_TYPE = 'TRIGGER'
                 AND S.OBJECT_NAME = T.TRIGGER_NAME
                 AND S.XX_CUST_ID = Z_CUST_ID)
       ORDER BY O.OBJECT_NAME, T.TRIGGER_NAME;
  
    CURSOR CUR_OBJ_SCHEMA IS
      SELECT GEN_DDL_SEQ, OBJECT_TYPE, OBJECT_NAME, OBJECT_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_SCHEMA IS NULL
         AND XX_CUST_ID = Z_CUST_ID
         AND XX_CUST_TYPE = Z_DDL_TYPE
         FOR UPDATE OF OBJECT_SCHEMA;
  
    CURSOR CUR_SYN_SCHEMA IS
      SELECT GEN_DDL_SEQ,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE SYNONYM_SCHEMA IS NULL
         AND XX_CUST_ID = Z_CUST_ID
         AND XX_CUST_TYPE = Z_DDL_TYPE
         FOR UPDATE OF SYNONYM_SCHEMA;
  
    CURSOR CUR_GEN_SYN IS
      SELECT X.PROGRAM_CODE,
             X.OBJECT_NAME,
             X.OBJECT_SCHEMA,
             X.SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP X
       WHERE X.SYNONYM_SCHEMA <> X.OBJECT_SCHEMA
         AND X.XX_CUST_ID = Z_CUST_ID
         AND XX_CUST_TYPE = Z_DDL_TYPE
         AND NOT EXISTS (SELECT 1
                FROM XX_CUST_OBJS_TMP Y
               WHERE X.OBJECT_NAME = Y.OBJECT_NAME
                 AND Y.XX_CUST_ID = Z_CUST_ID
                 AND Y.OBJECT_SCHEMA = 'SYNONYM');
  
    CURSOR CUR_INVALID_SYN IS
      SELECT OBJECT_NAME
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'SYNONYM'
         AND INSTR(OBJECT_NAME, '@') = 0
         AND INSTR(OBJECT_NAME, '/') = 0
         AND SYNONYM_SCHEMA = OBJECT_SCHEMA
         AND XX_CUST_ID = Z_CUST_ID
         AND XX_CUST_TYPE = Z_DDL_TYPE
         FOR UPDATE OF OBJECT_NAME;
  
    CURSOR CUR_CUST_OBJS IS
      SELECT GEN_DDL_SEQ,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE XX_CUST_ID = Z_CUST_ID
         AND XX_CUST_TYPE = Z_DDL_TYPE;
  
    V_SCHEMA VARCHAR2(10);
  BEGIN
    --auto gen table triggers 
    IF Z_GEN_TAB_TRG THEN
      FOR RTT IN CUR_TAB_TRG LOOP
        INSERT INTO XX_CUST_OBJS_TMP
          (XX_CUST_ID,
           XX_CUST_TYPE,
           PROGRAM_CODE,
           OBJECT_TYPE,
           OBJECT_NAME,
           OBJECT_SCHEMA,
           SYNONYM_SCHEMA)
        VALUES
          (Z_CUST_ID,
           Z_DDL_TYPE,
           RTT.PROGRAM_CODE,
           'TRIGGER',
           RTT.TRIGGER_NAME,
           RTT.SYNONYM_SCHEMA,
           RTT.SYNONYM_SCHEMA);
      END LOOP;
    END IF;
  
    FOR ROS IN CUR_OBJ_SCHEMA LOOP
      BEGIN
        SELECT OWNER
          INTO V_SCHEMA
          FROM ALL_OBJECTS
         WHERE OBJECT_NAME = ROS.OBJECT_NAME
           AND OBJECT_TYPE = ROS.OBJECT_TYPE;
      EXCEPTION
        WHEN OTHERS THEN
          V_SCHEMA := Z_DEF_SCHEMA;
      END;
      UPDATE XX_CUST_OBJS_TMP
         SET OBJECT_SCHEMA = V_SCHEMA
       WHERE CURRENT OF CUR_OBJ_SCHEMA;
    END LOOP;
  
    FOR RSS IN CUR_SYN_SCHEMA LOOP
      BEGIN
        SELECT OWNER
          INTO V_SCHEMA
          FROM (SELECT OWNER
                  FROM ALL_OBJECTS
                 WHERE OBJECT_NAME = RSS.OBJECT_NAME
                   AND OBJECT_TYPE = 'SYNONYM'
                 ORDER BY DECODE(OWNER,
                                 Z_DEF_SCHEMA,
                                 1,
                                 'APPS',
                                 2,
                                 'PUBLIC',
                                 3,
                                 4))
         WHERE ROWNUM = 1;
      EXCEPTION
        WHEN OTHERS THEN
          V_SCHEMA := RSS.OBJECT_SCHEMA;
      END;
      UPDATE XX_CUST_OBJS_TMP
         SET SYNONYM_SCHEMA = V_SCHEMA
       WHERE CURRENT OF CUR_SYN_SCHEMA;
    END LOOP;
  
    FOR RGS IN CUR_GEN_SYN LOOP
      INSERT INTO XX_CUST_OBJS_TMP
        (XX_CUST_ID,
         XX_CUST_TYPE,
         PROGRAM_CODE,
         OBJECT_TYPE,
         OBJECT_NAME,
         OBJECT_SCHEMA,
         SYNONYM_SCHEMA)
      VALUES
        (Z_CUST_ID,
         Z_DDL_TYPE,
         RGS.PROGRAM_CODE,
         'SYNONYM',
         RGS.OBJECT_NAME,
         RGS.OBJECT_SCHEMA,
         RGS.SYNONYM_SCHEMA);
    END LOOP;
  
    FOR RIS IN CUR_INVALID_SYN LOOP
      DELETE FROM XX_CUST_OBJS_TMP WHERE CURRENT OF CUR_INVALID_SYN;
      LOG(C_WARNING,
          'SYNONYM ' || RIS.OBJECT_NAME ||
          ' invalid defined, please check he object list control file.');
    END LOOP;
  
    FOR RC IN CUR_CUST_OBJS LOOP
      LOG(C_LOG,
          '###: ' || RC.OBJECT_TYPE || ' - ' || RC.OBJECT_NAME || ' - ' ||
          RC.OBJECT_SCHEMA || ' - ' || RC.SYNONYM_SCHEMA);
    END LOOP;
  END CUST_OBJS_INIT;

  /**
  * generate DDL Subtitle in front of the ddl file, for given ddl file 
  * @param P_FILE UTL_FILE.FILE_TYPE ddl file object
  * @param P_SUBTITLE_NAME VARCHAR2 subtitle string 
  */
  PROCEDURE GEN_DDL_SUBTITLE(P_FILE          IN UTL_FILE.FILE_TYPE,
                             P_SUBTITLE_NAME IN VARCHAR2) IS
  BEGIN
    UTL_FILE.PUT_LINE(P_FILE, GET_SUBTITLE(P_SUBTITLE_NAME));
  END GEN_DDL_SUBTITLE;

  FUNCTION GET_OBJ_DDL(P_OBJ_TYPE IN VARCHAR2, P_OBJ_NAME IN VARCHAR2)
    RETURN CLOB IS
    V_RETURN CLOB;
  BEGIN
    IF P_OBJ_TYPE = 'TABLE' THEN
      V_RETURN := GET_TABLE_DDL(P_OBJ_NAME);
    ELSIF P_OBJ_TYPE = 'PACKAGE' THEN
      V_RETURN := GET_PACKAGE_DDL(P_OBJ_NAME);
    ELSIF P_OBJ_TYPE = 'VIEW' THEN
      V_RETURN := GET_VIEW_DDL(P_OBJ_NAME);
    ELSIF P_OBJ_TYPE = 'SEQUENCE' THEN
      V_RETURN := GET_SEQUENCE_DDL(P_OBJ_NAME);
    ELSIF P_OBJ_TYPE = 'SYNONYM' THEN
      V_RETURN := GET_SYNONYM_DDL;
    ELSIF P_OBJ_TYPE = 'FNDLOAD' THEN
      V_RETURN := GET_FNDLOAD_SCPT(P_OBJ_NAME);
    ELSE
      V_RETURN := 'Todo..';
    END IF;
    RETURN V_RETURN;
  END GET_OBJ_DDL;

  FUNCTION GET_TABLE_DDL(P_OBJ_NAME IN VARCHAR2) RETURN CLOB IS
    V_RETURN       CLOB;
    V_EXIST        NUMBER;
    V_COL_STMT     VARCHAR2(300);
    V_COMMENT_STMT VARCHAR2(300);
    V_GRANT_STMT   VARCHAR2(300);
    V_COL_COUNT    NUMBER;
    V_COUNTER      NUMBER;
    V_DATA_TS      VARCHAR2(20);
    V_TEMP_FLAG    VARCHAR2(1);
    V_GRANT_FLAG   BOOLEAN := FALSE;
    V_IDX_STMT     VARCHAR2(1000);
    V_PREFIX       VARCHAR2(10);
    V_ONCMT_ACT    VARCHAR2(100);
    V_PARTITIONED  VARCHAR2(10);
    V_TEMPSTR      VARCHAR2(2000);
    V_PART_COUNT   NUMBER;
    V_SUBPART_CNT  NUMBER;
    V_SUBPART      VARCHAR2(1);
    V_COUNTER2     NUMBER;
  
    CURSOR CUR_OBJ_INFO IS
      SELECT PROGRAM_CODE,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'TABLE'
         AND OBJECT_NAME = P_OBJ_NAME
         AND XX_CUST_ID = Z_CUST_ID;
  
    CURSOR CUR_TBL_COLS IS
      SELECT COLUMN_NAME,
             DATA_TYPE,
             DATA_LENGTH,
             NULLABLE,
             DATA_DEFAULT,
             DEFAULT_LENGTH
        FROM ALL_TAB_COLUMNS
       WHERE TABLE_NAME = P_OBJ_NAME
       ORDER BY COLUMN_ID;
  
    CURSOR CUR_TAB_CMM IS
      SELECT TABLE_NAME TAB_NAME, COMMENTS
        FROM ALL_TAB_COMMENTS
       WHERE TABLE_NAME = P_OBJ_NAME
         AND COMMENTS IS NOT NULL;
  
    CURSOR CUR_COL_CMM IS
      SELECT TABLE_NAME || '.' || COLUMN_NAME COL_NAME, COMMENTS
        FROM ALL_COL_COMMENTS
       WHERE TABLE_NAME = P_OBJ_NAME
         AND COMMENTS IS NOT NULL;
  
    CURSOR CUR_GRANTEE(I_OBJ_SCHEMA IN VARCHAR2) IS
      SELECT GRANTEE,
             --LISTAGG(PRIVILEGE,',') WITHIN GROUP(ORDER BY PRIVILEGE) AS PRIV_CON  -- above 11gR2
             RTRIM(XMLAGG(XMLELEMENT(E, PRIVILEGE || ',') ORDER BY PRIVILEGE)
                   .EXTRACT('//text()'),
                   ',') AS PRIV_CON
        FROM DBA_TAB_PRIVS
       WHERE TABLE_NAME = P_OBJ_NAME
         AND GRANTEE <> I_OBJ_SCHEMA
       GROUP BY GRANTEE;
  
    CURSOR CUR_TAB_IDX IS
      SELECT INDEX_NAME FROM ALL_INDEXES WHERE TABLE_NAME = P_OBJ_NAME;
  
    CURSOR CUR_TAB_PARTITION IS
      SELECT PARTITION_NAME, HIGH_VALUE, TABLESPACE_NAME
        FROM DBA_TAB_PARTITIONS
       WHERE TABLE_NAME = P_OBJ_NAME
       ORDER BY PARTITION_POSITION;
  
    CURSOR CUR_TAB_SUBPARTITION(I_PART_NAME IN VARCHAR2) IS
      SELECT SUBPARTITION_NAME, HIGH_VALUE, TABLESPACE_NAME
        FROM ALL_TAB_SUBPARTITIONS
       WHERE TABLE_NAME = P_OBJ_NAME
         AND PARTITION_NAME = I_PART_NAME
       ORDER BY SUBPARTITION_POSITION;
  BEGIN
    --Z_CUST_ID := 3;
    --check exist
    SELECT COUNT(1)
      INTO V_EXIST
      FROM ALL_OBJECTS
     WHERE OBJECT_NAME = P_OBJ_NAME
       AND OBJECT_TYPE = 'TABLE';
  
    IF V_EXIST > 0 THEN
      SELECT PARTITIONED
        INTO V_PARTITIONED
        FROM ALL_TABLES
       WHERE TABLE_NAME = P_OBJ_NAME;
    
      FOR RI IN CUR_OBJ_INFO LOOP
        V_RETURN := GET_SUBTITLE('Table: ' || P_OBJ_NAME);
        IF Z_WITH_SCHEMA THEN
          V_PREFIX := RI.OBJECT_SCHEMA || '.';
        ELSE
          V_PREFIX := '';
        END IF;
        IF Z_DROP_TABLE THEN
          V_RETURN := V_RETURN || CHR(10) || 'DROP TABLE ' || V_PREFIX ||
                      P_OBJ_NAME || ';' || CHR(10);
        END IF;
      
        SELECT COUNT(COLUMN_NAME), 0
          INTO V_COL_COUNT, V_COUNTER
          FROM ALL_TAB_COLUMNS
         WHERE TABLE_NAME = P_OBJ_NAME;
      
        SELECT TEMPORARY,
               DECODE(DURATION,
                      NULL,
                      '',
                      'SYS$SESSION',
                      'ON COMMIT PRESERVE ROWS',
                      'ON COMMIT DELETE ROWS')
          INTO V_TEMP_FLAG, V_ONCMT_ACT
          FROM ALL_TABLES
         WHERE TABLE_NAME = P_OBJ_NAME;
      
        IF V_TEMP_FLAG = 'Y' THEN
          V_RETURN := V_RETURN || CHR(10) ||
                      'CREATE GLOBAL TEMPORARY TABLE ' || V_PREFIX ||
                      P_OBJ_NAME || '(';
        ELSE
          V_RETURN := V_RETURN || CHR(10) || 'CREATE TABLE ' || V_PREFIX ||
                      P_OBJ_NAME || '(';
        END IF;
        FOR REC IN CUR_TBL_COLS LOOP
          V_COUNTER := V_COUNTER + 1;
          IF REC.DATA_TYPE = 'VARCHAR2' THEN
            V_COL_STMT := Z_TAB || REC.COLUMN_NAME || Z_TAB || Z_TAB ||
                          REC.DATA_TYPE || '(' || TO_CHAR(REC.DATA_LENGTH) || ')';
          ELSE
            V_COL_STMT := Z_TAB || REC.COLUMN_NAME || Z_TAB || Z_TAB ||
                          REC.DATA_TYPE;
          END IF;
          IF REC.NULLABLE = 'N' THEN
            V_COL_STMT := V_COL_STMT || ' NOT NULL';
          END IF;
          IF REC.DATA_DEFAULT IS NOT NULL THEN
            V_COL_STMT := V_COL_STMT || ' default ' ||
                          REPLACE(REC.DATA_DEFAULT, CHR(10), '');
          END IF;
          IF V_COUNTER < V_COL_COUNT THEN
            V_COL_STMT := V_COL_STMT || ',';
          END IF;
          V_RETURN := V_RETURN || CHR(10) || V_COL_STMT;
        END LOOP;
        V_RETURN := V_RETURN || CHR(10) || ')';
      
        --On Commit Action for temp table
        IF V_TEMP_FLAG = 'Y' THEN
          V_RETURN := V_RETURN || CHR(10) || V_ONCMT_ACT || ';';
        ELSE
        
          --Table Space
          SELECT SCHEMA_DATA_TS
            INTO V_DATA_TS
            FROM XX_SCHEMA_INFO_TMP
           WHERE SCHEMA_NAME = RI.OBJECT_SCHEMA
             AND XX_CUST_ID = Z_CUST_ID;
        
          --partition
          IF V_PARTITIONED = 'YES' THEN
            SELECT 'PARTITION BY ' || X.PARTITIONING_TYPE ||
                   (SELECT ' (' || Y.COLUMN_NAME || ')'
                      FROM DBA_PART_KEY_COLUMNS Y
                     WHERE Y.NAME = X.TABLE_NAME) ||
                   DECODE(X.SUBPARTITIONING_TYPE,
                          'NONE',
                          '',
                          CHR(10) || ' SUBPARTITION BY ' ||
                          X.SUBPARTITIONING_TYPE ||
                          (SELECT ' (' || Z.COLUMN_NAME || ')'
                             FROM ALL_SUBPART_KEY_COLUMNS Z
                            WHERE Z.NAME = X.TABLE_NAME)) || CHR(10) || '(',
                   X.PARTITION_COUNT,
                   DECODE(X.SUBPARTITIONING_TYPE, 'NONE', 'N', 'Y')
              INTO V_TEMPSTR, V_PART_COUNT, V_SUBPART
              FROM DBA_PART_TABLES X
             WHERE X.TABLE_NAME = P_OBJ_NAME;
          
            V_RETURN  := V_RETURN || CHR(10) || V_TEMPSTR;
            V_COUNTER := 0;
            FOR RX IN CUR_TAB_PARTITION LOOP
              V_RETURN  := V_RETURN || CHR(10) || 'PARTITION ' ||
                           RX.PARTITION_NAME || ' VALUES (';
              V_RETURN  := V_RETURN || CHR(10) || RX.HIGH_VALUE;
              V_RETURN  := V_RETURN || CHR(10) || ' )  TABLESPACE ' ||
                           RX.TABLESPACE_NAME;
              V_COUNTER := V_COUNTER + 1;
              IF V_SUBPART = 'Y' THEN
                SELECT COUNT(1)
                  INTO V_SUBPART_CNT
                  FROM ALL_TAB_SUBPARTITIONS
                 WHERE TABLE_NAME = P_OBJ_NAME
                   AND PARTITION_NAME = RX.PARTITION_NAME;
                V_RETURN   := V_RETURN || CHR(10) || CHR(10) || '(';
                V_COUNTER2 := 0;
                FOR RY IN CUR_TAB_SUBPARTITION(RX.PARTITION_NAME) LOOP
                  V_RETURN   := V_RETURN || CHR(10) || 'SUBPARTITION ' ||
                                RY.SUBPARTITION_NAME || ' VALUES (';
                  V_RETURN   := V_RETURN || CHR(10) || RY.HIGH_VALUE;
                  V_RETURN   := V_RETURN || CHR(10) || ' )  TABLESPACE ' ||
                                RY.TABLESPACE_NAME;
                  V_COUNTER2 := V_COUNTER2 + 1;
                  IF V_COUNTER2 < V_SUBPART_CNT THEN
                    V_RETURN := V_RETURN || CHR(10) || ',' || CHR(10);
                  ELSE
                    V_RETURN := V_RETURN || CHR(10) || '' || CHR(10);
                  END IF;
                END LOOP;
                V_RETURN := V_RETURN || CHR(10) || ')';
              END IF;
              IF V_COUNTER < V_PART_COUNT THEN
                V_RETURN := V_RETURN || CHR(10) || ',' || CHR(10);
              ELSE
                V_RETURN := V_RETURN || CHR(10) || '' || CHR(10);
              END IF;
            
            END LOOP;
            V_RETURN := V_RETURN || CHR(10) || ');';
          ELSE
            V_RETURN := V_RETURN || CHR(10) || 'TABLESPACE ' || V_DATA_TS || ';';
          END IF;
        END IF;
      
        V_RETURN := V_RETURN || CHR(10) || Z_BLANK_LINE;
        --Comment
        FOR RTM IN CUR_TAB_CMM LOOP
          V_COMMENT_STMT := 'COMMENT ON TABLE ' || V_PREFIX || RTM.TAB_NAME ||
                            ' IS ''' || RTM.COMMENTS || ''';';
          V_RETURN       := V_RETURN || CHR(10) || V_COMMENT_STMT;
        END LOOP;
        FOR RCM IN CUR_COL_CMM LOOP
          V_COMMENT_STMT := 'COMMENT ON COLUMN ' || V_PREFIX ||
                            RCM.COL_NAME || ' IS ''' || RCM.COMMENTS ||
                            ''';';
          V_RETURN       := V_RETURN || CHR(10) || V_COMMENT_STMT;
        END LOOP;
        V_RETURN := V_RETURN || CHR(10) || Z_BLANK_LINE;
        --Index 
      
        FOR RTI IN CUR_TAB_IDX LOOP
          V_IDX_STMT := GET_IDX_DDL_STMT(RTI.INDEX_NAME, RI.OBJECT_SCHEMA);
          V_RETURN   := V_RETURN || CHR(10) || V_IDX_STMT;
        END LOOP;
        V_RETURN := V_RETURN || CHR(10) || Z_BLANK_LINE;
      
        --Grant
        IF RI.OBJECT_SCHEMA <> RI.SYNONYM_SCHEMA THEN
          V_GRANT_FLAG := FALSE;
          FOR RG IN CUR_GRANTEE(RI.OBJECT_SCHEMA) LOOP
            V_GRANT_STMT := 'GRANT ' || RG.PRIV_CON || ' ON ' || V_PREFIX ||
                            P_OBJ_NAME || ' TO ' || RG.GRANTEE || ';';
            V_RETURN     := V_RETURN || CHR(10) || V_GRANT_STMT;
            IF RG.GRANTEE = RI.SYNONYM_SCHEMA THEN
              V_GRANT_FLAG := TRUE;
            END IF;
          END LOOP;
          IF NOT V_GRANT_FLAG THEN
            V_GRANT_STMT := 'GRANT ALL ON ' || V_PREFIX || P_OBJ_NAME ||
                            ' TO ' || RI.SYNONYM_SCHEMA || ';';
            V_RETURN     := V_RETURN || CHR(10) || V_GRANT_STMT;
          END IF;
        END IF;
        V_RETURN := V_RETURN || CHR(10) || Z_BLANK_LINE;
      END LOOP;
      RETURN V_RETURN;
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_ADD_MSG     => 'OBJ:' || P_OBJ_NAME,
                    P_SQLERRM_MSG => SQLERRM);
  END GET_TABLE_DDL;

  FUNCTION GET_PACKAGE_DDL(P_OBJ_NAME IN VARCHAR2) RETURN CLOB IS
    V_RETURN       CLOB;
    V_FILE_NAME    VARCHAR2(100);
    V_EXT          VARCHAR2(5) := Z_PACKAGE_EXT;
    V_EXIST        NUMBER;
    V_SOURCE_CLOB  CLOB;
    V_REPLACE_STR  VARCHAR2(100);
    V_REPLACE_WITH VARCHAR2(100);
    V_PREFIX       VARCHAR2(10);
    V_GRANT_FLAG   BOOLEAN;
    V_GRANT_STMT   VARCHAR2(1000);
    V_OBJ_OWNER    VARCHAR2(10);
  
    V_REPLACE_STRB  VARCHAR2(100);
    V_REPLACE_WITHB VARCHAR2(100);
  
    CURSOR CUR_OBJ_INFO IS
      SELECT PROGRAM_CODE,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'PACKAGE'
         AND OBJECT_NAME = P_OBJ_NAME
         AND XX_CUST_ID = Z_CUST_ID;
  
    CURSOR CUR_GRANTEE(I_OBJ_SCHEMA IN VARCHAR2) IS
      SELECT GRANTEE,
             --LISTAGG(PRIVILEGE,',') WITHIN GROUP(ORDER BY PRIVILEGE) AS PRIV_CON  -- above 11gR2
             RTRIM(XMLAGG(XMLELEMENT(E, PRIVILEGE || ',') ORDER BY PRIVILEGE)
                   .EXTRACT('//text()'),
                   ',') AS PRIV_CON
        FROM DBA_TAB_PRIVS
       WHERE TABLE_NAME = P_OBJ_NAME
         AND GRANTEE <> I_OBJ_SCHEMA
       GROUP BY GRANTEE;
  
  BEGIN
    --check exist
    SELECT COUNT(1)
      INTO V_EXIST
      FROM ALL_OBJECTS
     WHERE OBJECT_NAME = P_OBJ_NAME
       AND OBJECT_TYPE = 'PACKAGE';
  
    IF V_EXIST > 0 THEN
      FOR REC IN CUR_OBJ_INFO LOOP
        IF Z_WITH_SCHEMA THEN
          V_PREFIX := REC.OBJECT_SCHEMA || '.';
        ELSE
          V_PREFIX := '';
        END IF;
      
        --check schema for replace ddl
        SELECT '"' || OWNER || '"."' || OBJECT_NAME || '"', OWNER
          INTO V_REPLACE_STR, V_OBJ_OWNER
          FROM ALL_OBJECTS
         WHERE OBJECT_NAME = P_OBJ_NAME
           AND OBJECT_TYPE = 'PACKAGE';
      
        V_REPLACE_WITH := V_PREFIX || REC.OBJECT_NAME;
      
        --Grant
        IF REC.OBJECT_SCHEMA <> REC.SYNONYM_SCHEMA THEN
          V_GRANT_FLAG := FALSE;
          FOR RG IN CUR_GRANTEE(REC.OBJECT_SCHEMA) LOOP
            V_GRANT_STMT := 'GRANT ' || RG.PRIV_CON || ' ON ' || V_PREFIX ||
                            P_OBJ_NAME || ' TO ' || RG.GRANTEE || ';';
            IF RG.GRANTEE = REC.SYNONYM_SCHEMA THEN
              V_GRANT_FLAG := TRUE;
            END IF;
          END LOOP;
          IF NOT V_GRANT_FLAG THEN
            V_GRANT_STMT := 'GRANT EXECUTE ON ' || V_PREFIX || P_OBJ_NAME ||
                            ' TO ' || REC.SYNONYM_SCHEMA || ';';
          END IF;
        ELSE
          V_GRANT_STMT := '';
        END IF;
      
        V_REPLACE_STRB  := 'END ' || P_OBJ_NAME || ';';
        V_REPLACE_WITHB := 'END ' || P_OBJ_NAME || ';' || CHR(10) || '/';
      
        SELECT REPLACECLOB(REPLACECLOB(DBMS_METADATA.GET_DDL('PACKAGE',
                                                             P_OBJ_NAME,
                                                             V_OBJ_OWNER),
                                       V_REPLACE_STR,
                                       V_REPLACE_WITH,
                                       '', --'Package: ' || P_OBJ_NAME,
                                       V_GRANT_STMT),
                           V_REPLACE_STRB,
                           V_REPLACE_WITHB,
                           '',
                           '')
          INTO V_SOURCE_CLOB
          FROM DUAL;
      
        V_RETURN := V_SOURCE_CLOB;
      
      END LOOP;
    
      RETURN V_RETURN;
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_ADD_MSG     => 'OBJ:' || P_OBJ_NAME,
                    P_SQLERRM_MSG => SQLERRM);
  END GET_PACKAGE_DDL;

  /**
  * generate Index DDL statement  
  * @param P_OBJ_NAME VARCHAR2 Index object name
  * @param P_OBJ_SCHEMA VARCHAR2 Index Schema name
  * @return VARCHAR2 index ddl statement
  */
  FUNCTION GET_IDX_DDL_STMT(P_OBJ_NAME   IN VARCHAR2,
                            P_OBJ_SCHEMA IN VARCHAR2) RETURN VARCHAR2 IS
    V_IDX_STMT    VARCHAR2(2000);
    V_UNIQUE_TYPE VARCHAR2(10);
    V_INDEX_COLS  VARCHAR2(1000);
    V_TABLE_NAME  VARCHAR2(50);
    V_PREFIX      VARCHAR2(10);
    V_IDX_TS      VARCHAR2(30);
    V_PARTITIONED VARCHAR2(10);
  BEGIN
    SELECT RTRIM(XMLAGG(XMLELEMENT(E, COLUMN_NAME || ',') ORDER BY COLUMN_POSITION)
                 .EXTRACT('//text()'),
                 ',') AS IDX_CON
      INTO V_INDEX_COLS
      FROM (SELECT DECODE(INSTR(COLUMN_NAME, '$'),
                          0,
                          COLUMN_NAME,
                          GET_FIDX_COL(COLUMN_NAME, TABLE_OWNER, TABLE_NAME)) COLUMN_NAME,
                   COLUMN_POSITION
              FROM DBA_IND_COLUMNS
             WHERE INDEX_NAME = P_OBJ_NAME);
  
    SELECT DECODE(UNIQUENESS, 'UNIQUE', 'UNIQUE', ''),
           TABLE_NAME,
           PARTITIONED
      INTO V_UNIQUE_TYPE, V_TABLE_NAME, V_PARTITIONED
      FROM ALL_INDEXES
     WHERE INDEX_NAME = P_OBJ_NAME;
  
    IF Z_WITH_SCHEMA THEN
      V_PREFIX := P_OBJ_SCHEMA || '.';
    ELSE
      V_PREFIX := '';
    END IF;
  
    V_IDX_STMT := 'CREATE ' || V_UNIQUE_TYPE || ' INDEX ' || V_PREFIX ||
                  P_OBJ_NAME || ' ON ' || V_PREFIX || V_TABLE_NAME || '(' ||
                  V_INDEX_COLS || ') ' || CHR(10);
  
    IF V_PARTITIONED = 'YES' THEN
      V_IDX_STMT := V_IDX_STMT || ' LOCAL;';
    ELSE
      --tablespace
      SELECT SCHEMA_IDX_TS
        INTO V_IDX_TS
        FROM XX_SCHEMA_INFO_TMP
       WHERE SCHEMA_NAME = P_OBJ_SCHEMA
         AND XX_CUST_ID = Z_CUST_ID;
      V_IDX_STMT := V_IDX_STMT || 'TABLESPACE ' || V_IDX_TS || ';';
    END IF;
  
    RETURN V_IDX_STMT;
  END GET_IDX_DDL_STMT;

  FUNCTION GET_VIEW_DDL(P_OBJ_NAME IN VARCHAR2) RETURN CLOB IS
    V_RETURN     CLOB;
    V_EXIST      NUMBER;
    V_TEXT       LONG;
    V_PREFIX     VARCHAR2(10);
    V_GRANT_FLAG BOOLEAN;
    V_GRANT_STMT VARCHAR2(1000);
  
    CURSOR CUR_OBJ_INFO IS
      SELECT PROGRAM_CODE,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'VIEW'
         AND OBJECT_NAME = P_OBJ_NAME
         AND XX_CUST_ID = Z_CUST_ID;
  
    CURSOR CUR_GRANTEE(I_OBJ_SCHEMA IN VARCHAR2) IS
      SELECT GRANTEE,
             --LISTAGG(PRIVILEGE,',') WITHIN GROUP(ORDER BY PRIVILEGE) AS PRIV_CON  -- above 11gR2
             RTRIM(XMLAGG(XMLELEMENT(E, PRIVILEGE || ',') ORDER BY PRIVILEGE)
                   .EXTRACT('//text()'),
                   ',') AS PRIV_CON
        FROM DBA_TAB_PRIVS
       WHERE TABLE_NAME = P_OBJ_NAME
         AND GRANTEE <> I_OBJ_SCHEMA
       GROUP BY GRANTEE;
  BEGIN
    --check exist
    SELECT COUNT(1)
      INTO V_EXIST
      FROM ALL_VIEWS
     WHERE VIEW_NAME = P_OBJ_NAME;
    --gen ddl
    IF V_EXIST > 0 THEN
      FOR REC IN CUR_OBJ_INFO LOOP
        V_RETURN := GET_SUBTITLE('View: ' || P_OBJ_NAME);
        IF Z_WITH_SCHEMA THEN
          V_PREFIX := REC.OBJECT_SCHEMA || '.';
        ELSE
          V_PREFIX := '';
        END IF;
        V_RETURN := V_RETURN || CHR(10) || 'CREATE OR REPLACE VIEW ' ||
                    V_PREFIX || REC.OBJECT_NAME || CHR(10) || 'AS';
      
        SELECT TEXT
          INTO V_TEXT
          FROM ALL_VIEWS
         WHERE VIEW_NAME = P_OBJ_NAME;
        V_RETURN := V_RETURN || CHR(10) || TO_CLOB(V_TEXT) || CHR(10) || ';' ||
                    CHR(10);
      
        --Grant
        IF REC.OBJECT_SCHEMA <> REC.SYNONYM_SCHEMA THEN
          V_GRANT_FLAG := FALSE;
          FOR RG IN CUR_GRANTEE(REC.OBJECT_SCHEMA) LOOP
            V_GRANT_STMT := 'GRANT ' || RG.PRIV_CON || ' ON ' || V_PREFIX ||
                            P_OBJ_NAME || ' TO ' || RG.GRANTEE || ';';
            V_RETURN     := V_RETURN || CHR(10) || V_GRANT_STMT;
            IF RG.GRANTEE = REC.SYNONYM_SCHEMA THEN
              V_GRANT_FLAG := TRUE;
            END IF;
          END LOOP;
          IF NOT V_GRANT_FLAG THEN
            V_GRANT_STMT := 'GRANT SELECT ON ' || V_PREFIX || P_OBJ_NAME ||
                            ' TO ' || REC.SYNONYM_SCHEMA || ';';
            V_RETURN     := V_RETURN || CHR(10) || V_GRANT_STMT;
          END IF;
        END IF;
      END LOOP;
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
    RETURN V_RETURN;
  END GET_VIEW_DDL;

  /**
  * generate Materialized View DDL script  
  * @param P_OBJ_NAME VARCHAR2 View object name
  */
  PROCEDURE GEN_MVIEW_DDL(P_OBJ_NAME IN VARCHAR2) IS
    V_FILE         UTL_FILE.FILE_TYPE;
    V_FILE_NAME    VARCHAR2(100);
    V_EXT          VARCHAR2(5) := Z_MVIEW_EXT;
    V_EXIST        NUMBER;
    V_QUERY        LONG;
    V_BUILD_STMT   VARCHAR2(100);
    V_REFRESH_STMT VARCHAR2(100);
    V_REWRITE_STMT VARCHAR2(100);
    V_PREFIX       VARCHAR2(10);
    V_GRANT_FLAG   BOOLEAN;
    V_GRANT_STMT   VARCHAR2(1000);
    V_IDX_STMT     VARCHAR2(1000);
  
    CURSOR CUR_OBJ_INFO IS
      SELECT PROGRAM_CODE,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'MVIEW'
         AND OBJECT_NAME = P_OBJ_NAME
         AND XX_CUST_ID = Z_CUST_ID;
  
    CURSOR CUR_GRANTEE(I_OBJ_SCHEMA IN VARCHAR2) IS
      SELECT GRANTEE,
             --LISTAGG(PRIVILEGE,',') WITHIN GROUP(ORDER BY PRIVILEGE) AS PRIV_CON  -- above 11gR2
             RTRIM(XMLAGG(XMLELEMENT(E, PRIVILEGE || ',') ORDER BY PRIVILEGE)
                   .EXTRACT('//text()'),
                   ',') AS PRIV_CON
        FROM DBA_TAB_PRIVS
       WHERE TABLE_NAME = P_OBJ_NAME
         AND GRANTEE <> I_OBJ_SCHEMA
       GROUP BY GRANTEE;
  
    CURSOR CUR_MV_IDX IS
      SELECT INDEX_NAME
        FROM ALL_INDEXES
       WHERE TABLE_NAME = P_OBJ_NAME
         AND INSTR(INDEX_NAME, 'SNAP$') = 0;
  BEGIN
    --check exist
    SELECT COUNT(1)
      INTO V_EXIST
      FROM ALL_MVIEWS
     WHERE MVIEW_NAME = P_OBJ_NAME;
    --gen ddl
    IF V_EXIST > 0 THEN
      FOR REC IN CUR_OBJ_INFO LOOP
        V_FILE_NAME := GET_FILE_NAME(P_OBJ_NAME, V_EXT, REC.PROGRAM_CODE);
        V_FILE      := UTL_FILE.FOPEN('--to-delete--',
                                      V_FILE_NAME,
                                      'W',
                                      32767);
        GEN_DDL_SUBTITLE(V_FILE, 'View: ' || P_OBJ_NAME);
        IF Z_WITH_SCHEMA THEN
          V_PREFIX := REC.OBJECT_SCHEMA || '.';
        ELSE
          V_PREFIX := '';
        END IF;
      
        --Drop 
        IF Z_DROP_MVIEW THEN
          UTL_FILE.PUT_LINE(V_FILE,
                            'DROP MATERIALIZED VIEW  ' || V_PREFIX ||
                            P_OBJ_NAME || ';');
          UTL_FILE.PUT_LINE(V_FILE, Z_BLANK_LINE);
        END IF;
      
        SELECT QUERY,
               'BUILD ' || BUILD_MODE BUILD_STMT,
               'REFRESH ' || REFRESH_METHOD || ' ON ' || REFRESH_MODE REFRESH_STMT,
               DECODE(REWRITE_ENABLED, 'Y', 'ENABLE ', 'DISABLE ') ||
               'QUERY REWRITE' REWRITE_STMT
          INTO V_QUERY, V_BUILD_STMT, V_REFRESH_STMT, V_REWRITE_STMT
          FROM ALL_MVIEWS
         WHERE MVIEW_NAME = P_OBJ_NAME;
      
        UTL_FILE.PUT_LINE(V_FILE,
                          'CREATE MATERIALIZED VIEW  ' || V_PREFIX ||
                          REC.OBJECT_NAME);
        UTL_FILE.PUT_LINE(V_FILE, V_BUILD_STMT);
        UTL_FILE.PUT_LINE(V_FILE, V_REFRESH_STMT);
        UTL_FILE.PUT_LINE(V_FILE, V_REWRITE_STMT);
        UTL_FILE.PUT_LINE(V_FILE, 'AS');
      
        UTL_FILE.PUT(V_FILE, V_QUERY);
        UTL_FILE.PUT_LINE(V_FILE, ';');
        UTL_FILE.PUT_LINE(V_FILE, Z_BLANK_LINE);
      
        --Index   
        FOR RMI IN CUR_MV_IDX LOOP
          V_IDX_STMT := GET_IDX_DDL_STMT(RMI.INDEX_NAME, REC.OBJECT_SCHEMA);
          UTL_FILE.PUT_LINE(V_FILE, V_IDX_STMT);
        END LOOP;
        UTL_FILE.PUT_LINE(V_FILE, Z_BLANK_LINE);
      
        --Grant
        IF REC.OBJECT_SCHEMA <> REC.SYNONYM_SCHEMA THEN
          V_GRANT_FLAG := FALSE;
          FOR RG IN CUR_GRANTEE(REC.OBJECT_SCHEMA) LOOP
            V_GRANT_STMT := 'GRANT ' || RG.PRIV_CON || ' ON ' || V_PREFIX ||
                            P_OBJ_NAME || ' TO ' || RG.GRANTEE || ';';
            UTL_FILE.PUT_LINE(V_FILE, V_GRANT_STMT);
            IF RG.GRANTEE = REC.SYNONYM_SCHEMA THEN
              V_GRANT_FLAG := TRUE;
            END IF;
          END LOOP;
          IF NOT V_GRANT_FLAG THEN
            V_GRANT_STMT := 'GRANT ALL ON ' || V_PREFIX || P_OBJ_NAME ||
                            ' TO ' || REC.SYNONYM_SCHEMA || ';';
            UTL_FILE.PUT_LINE(V_FILE, V_GRANT_STMT);
          END IF;
        END IF;
      END LOOP;
      UTL_FILE.PUT_LINE(V_FILE, Z_BLANK_LINE);
      UTL_FILE.FCLOSE(V_FILE);
    
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_ADD_MSG     => 'OBJ:' || P_OBJ_NAME,
                    P_SQLERRM_MSG => SQLERRM);
  END GEN_MVIEW_DDL;

  /**
  * generate Trigger DDL script
  * @param P_OBJ_NAME VARCHAR2 View object name
  */
  PROCEDURE GEN_TRG_DDL(P_OBJ_NAME IN VARCHAR2) IS
    V_FILE          UTL_FILE.FILE_TYPE;
    V_FILE_NAME     VARCHAR2(100);
    V_EXT           VARCHAR2(5) := Z_TRIGGER_EXT;
    V_EXIST         NUMBER;
    V_REPLACE_STR   VARCHAR2(100);
    V_REPLACE_WITH  VARCHAR2(100);
    V_REPLACE_STR2  VARCHAR2(100);
    V_REPLACE_WITH2 VARCHAR2(100);
    V_PREFIX        VARCHAR2(10);
    V_DESCRIPTION   VARCHAR2(1000);
    V_WHEN_CLAUSE   VARCHAR2(1000);
    V_TRIGGER_BODY  LONG;
    V_STATUS        VARCHAR2(20);
    V_ALTER_STMT    VARCHAR2(500);
  
    CURSOR CUR_OBJ_INFO IS
      SELECT PROGRAM_CODE,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'TRIGGER'
         AND OBJECT_NAME = P_OBJ_NAME
         AND XX_CUST_ID = Z_CUST_ID;
  
  BEGIN
    --check exist
    SELECT COUNT(1)
      INTO V_EXIST
      FROM ALL_OBJECTS
     WHERE OBJECT_NAME = P_OBJ_NAME
       AND OBJECT_TYPE = 'TRIGGER';
  
    IF V_EXIST > 0 THEN
      FOR REC IN CUR_OBJ_INFO LOOP
        V_FILE_NAME := GET_FILE_NAME(P_OBJ_NAME, V_EXT, REC.PROGRAM_CODE);
        V_FILE      := UTL_FILE.FOPEN('--to--delete--',
                                      V_FILE_NAME,
                                      'W',
                                      32767);
        GEN_DDL_SUBTITLE(V_FILE, 'Trigger: ' || P_OBJ_NAME);
        IF Z_WITH_SCHEMA THEN
          V_PREFIX := REC.OBJECT_SCHEMA || '.';
        ELSE
          V_PREFIX := '';
        END IF;
      
        V_FILE_NAME := GET_FILE_NAME(P_OBJ_NAME, V_EXT, REC.PROGRAM_CODE);
      
        --check schema for replace ddl
        SELECT OWNER || '.' || TRIGGER_NAME,
               V_PREFIX || REC.OBJECT_NAME,
               TABLE_OWNER || '.' || TABLE_NAME,
               TABLE_NAME,
               DESCRIPTION,
               WHEN_CLAUSE,
               TRIGGER_BODY,
               STATUS
          INTO V_REPLACE_STR,
               V_REPLACE_WITH,
               V_REPLACE_STR2,
               V_REPLACE_WITH2,
               V_DESCRIPTION,
               V_WHEN_CLAUSE,
               V_TRIGGER_BODY,
               V_STATUS
          FROM ALL_TRIGGERS
         WHERE TRIGGER_NAME = P_OBJ_NAME;
      
        V_DESCRIPTION := REPLACE(V_DESCRIPTION,
                                 V_REPLACE_STR,
                                 V_REPLACE_WITH);
        V_DESCRIPTION := REPLACE(V_DESCRIPTION,
                                 V_REPLACE_STR2,
                                 V_REPLACE_WITH2);
      
        UTL_FILE.PUT_LINE(V_FILE,
                          'CREATE OR REPLACE TRIGGER ' || V_DESCRIPTION);
        IF LENGTH(V_WHEN_CLAUSE) > 0 THEN
          UTL_FILE.PUT_LINE(V_FILE, 'WHEN (' || V_WHEN_CLAUSE || ') ');
        END IF;
        UTL_FILE.PUT_LINE(V_FILE, V_TRIGGER_BODY);
        UTL_FILE.PUT_LINE(V_FILE, Z_BLANK_LINE);
      
        --alter enable/disable
        V_ALTER_STMT := 'ALTER TRIGGER ' || V_REPLACE_WITH || ' ' ||
                        V_STATUS || ';';
        UTL_FILE.PUT_LINE(V_FILE, V_ALTER_STMT);
      END LOOP;
      UTL_FILE.PUT_LINE(V_FILE, Z_BLANK_LINE);
      UTL_FILE.FCLOSE(V_FILE);
    
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_ADD_MSG     => 'OBJ:' || P_OBJ_NAME,
                    P_SQLERRM_MSG => SQLERRM);
  END GEN_TRG_DDL;

  /**
  * generate Index DDL script  
  * @param P_OBJ_NAME VARCHAR2 Index object name
  */
  PROCEDURE GEN_IDX_DDL(P_OBJ_NAME IN VARCHAR2) IS
    V_FILE      UTL_FILE.FILE_TYPE;
    V_FILE_NAME VARCHAR2(100);
    V_EXT       VARCHAR2(5) := Z_SQL_EXT;
    V_EXIST     NUMBER;
    V_PREFIX    VARCHAR2(10);
    V_IDX_STMT  VARCHAR2(1000);
  
    CURSOR CUR_OBJ_INFO IS
      SELECT PROGRAM_CODE,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'INDEX'
         AND OBJECT_NAME = P_OBJ_NAME
         AND XX_CUST_ID = Z_CUST_ID;
  BEGIN
    --check exist
    SELECT COUNT(1)
      INTO V_EXIST
      FROM ALL_INDEXES
     WHERE INDEX_NAME = P_OBJ_NAME;
    --gen ddl
    IF V_EXIST > 0 THEN
      FOR REC IN CUR_OBJ_INFO LOOP
        V_FILE_NAME := GET_FILE_NAME(P_OBJ_NAME, V_EXT, REC.PROGRAM_CODE);
        V_FILE      := UTL_FILE.FOPEN('--to--delete--',
                                      V_FILE_NAME,
                                      'W',
                                      32767);
        GEN_DDL_SUBTITLE(V_FILE, 'Index: ' || P_OBJ_NAME);
        IF Z_WITH_SCHEMA THEN
          V_PREFIX := REC.OBJECT_SCHEMA || '.';
        ELSE
          V_PREFIX := '';
        END IF;
      
        --Drop 
        IF Z_DROP_INDEX THEN
          UTL_FILE.PUT_LINE(V_FILE,
                            'DROP INDEX ' || V_PREFIX || P_OBJ_NAME || ';');
          UTL_FILE.PUT_LINE(V_FILE, Z_BLANK_LINE);
        END IF;
      
        V_IDX_STMT := GET_IDX_DDL_STMT(REC.OBJECT_NAME, REC.OBJECT_SCHEMA);
        UTL_FILE.PUT_LINE(V_FILE, V_IDX_STMT);
      END LOOP;
      UTL_FILE.PUT_LINE(V_FILE, Z_BLANK_LINE);
      UTL_FILE.FCLOSE(V_FILE);
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_ADD_MSG     => 'OBJ:' || P_OBJ_NAME,
                    P_SQLERRM_MSG => SQLERRM);
  END GEN_IDX_DDL;

  FUNCTION GET_SEQUENCE_DDL(P_OBJ_NAME IN VARCHAR2) RETURN CLOB IS
    V_RETURN     CLOB;
    V_EXIST      NUMBER;
    V_CACHE_STMT VARCHAR2(100);
    V_ORDER_STMT VARCHAR2(100);
    V_CYCLE_STMT VARCHAR2(100);
    V_PREFIX     VARCHAR2(10);
    V_GRANT_FLAG BOOLEAN;
    V_GRANT_STMT VARCHAR2(1000);
  
    CURSOR CUR_OBJ_INFO IS
      SELECT PROGRAM_CODE,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'SEQUENCE'
         AND OBJECT_NAME = P_OBJ_NAME
         AND XX_CUST_ID = Z_CUST_ID;
  
    CURSOR CUR_GRANTEE(I_OBJ_SCHEMA IN VARCHAR2) IS
      SELECT GRANTEE,
             --LISTAGG(PRIVILEGE,',') WITHIN GROUP(ORDER BY PRIVILEGE) AS PRIV_CON  -- above 11gR2
             RTRIM(XMLAGG(XMLELEMENT(E, PRIVILEGE || ',') ORDER BY PRIVILEGE)
                   .EXTRACT('//text()'),
                   ',') AS PRIV_CON
        FROM DBA_TAB_PRIVS
       WHERE TABLE_NAME = P_OBJ_NAME
         AND GRANTEE <> I_OBJ_SCHEMA
       GROUP BY GRANTEE;
  BEGIN
    --check exist
    SELECT COUNT(1)
      INTO V_EXIST
      FROM ALL_SEQUENCES
     WHERE SEQUENCE_NAME = P_OBJ_NAME;
    --gen ddl
    IF V_EXIST > 0 THEN
      FOR REC IN CUR_OBJ_INFO LOOP
        V_RETURN := GET_SUBTITLE('Sequence: ' || P_OBJ_NAME);
        IF Z_WITH_SCHEMA THEN
          V_PREFIX := REC.OBJECT_SCHEMA || '.';
        ELSE
          V_PREFIX := '';
        END IF;
      
        --Drop 
        IF Z_DROP_SEQUENCE THEN
          V_RETURN := V_RETURN || CHR(10) || 'DROP SEQUENCE ' || V_PREFIX ||
                      P_OBJ_NAME || ';' || CHR(10);
        END IF;
      
        SELECT DECODE(CACHE_SIZE, 0, '', 'CACHE ' || TO_CHAR(CACHE_SIZE)) CACHE_STMT,
               DECODE(ORDER_FLAG, 'Y', 'ORDER', 'NOORDER') ORDER_STMT,
               DECODE(CYCLE_FLAG, 'Y', 'CYCLE', 'NOCYCLE') CYCLE_STMT
          INTO V_CACHE_STMT, V_ORDER_STMT, V_CYCLE_STMT
          FROM ALL_SEQUENCES
         WHERE SEQUENCE_NAME = P_OBJ_NAME;
      
        V_RETURN := V_RETURN || CHR(10) || 'CREATE SEQUENCE ' || V_PREFIX ||
                    REC.OBJECT_NAME || ' ' || V_CACHE_STMT || ' ' ||
                    V_ORDER_STMT || ' ' || V_CYCLE_STMT || ';' || CHR(10);
      
        --Grant
        IF REC.OBJECT_SCHEMA <> REC.SYNONYM_SCHEMA THEN
          V_GRANT_FLAG := FALSE;
          FOR RG IN CUR_GRANTEE(REC.OBJECT_SCHEMA) LOOP
            V_GRANT_STMT := 'GRANT ' || RG.PRIV_CON || ' ON ' || V_PREFIX ||
                            P_OBJ_NAME || ' TO ' || RG.GRANTEE || ';';
            V_RETURN     := V_RETURN || CHR(10) || V_GRANT_STMT;
            IF RG.GRANTEE = REC.SYNONYM_SCHEMA THEN
              V_GRANT_FLAG := TRUE;
            END IF;
          END LOOP;
          IF NOT V_GRANT_FLAG THEN
            V_GRANT_STMT := 'GRANT SELECT,ALTER ON ' || V_PREFIX ||
                            P_OBJ_NAME || ' TO ' || REC.SYNONYM_SCHEMA || ';';
            V_RETURN     := V_RETURN || CHR(10) || V_GRANT_STMT;
          END IF;
        END IF;
      END LOOP;
      V_RETURN := V_RETURN || CHR(10) || Z_BLANK_LINE;
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
    RETURN V_RETURN;
  END GET_SEQUENCE_DDL;

  /**
  * generate Java Source DDL script  
  * @param P_OBJ_NAME VARCHAR2 Java Source object name
  */
  PROCEDURE GEN_JAVASRC_DDL(P_OBJ_NAME IN VARCHAR2) IS
    V_FILE      UTL_FILE.FILE_TYPE;
    V_FILE_NAME VARCHAR2(100);
    V_EXT       VARCHAR2(5) := Z_JAVASRC_EXT;
    V_EXIST     NUMBER;
    V_BLOB      BLOB;
    CURSOR CUR_OBJ_INFO IS
      SELECT PROGRAM_CODE,
             OBJECT_TYPE,
             OBJECT_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'JAVA'
         AND OBJECT_NAME = P_OBJ_NAME
         AND XX_CUST_ID = Z_CUST_ID;
  
  BEGIN
    --check exist
    SELECT COUNT(1)
      INTO V_EXIST
      FROM ALL_OBJECTS
     WHERE OBJECT_NAME = UPPER(P_OBJ_NAME)
       AND OBJECT_TYPE = 'JAVA SOURCE';
    --gen ddl
    IF V_EXIST > 0 THEN
      FOR REC IN CUR_OBJ_INFO LOOP
        V_FILE_NAME := GET_FILE_NAME(P_OBJ_NAME, V_EXT, REC.PROGRAM_CODE);
        V_FILE      := UTL_FILE.FOPEN('--to delete--',
                                      V_FILE_NAME,
                                      'W',
                                      32767);
        DBMS_LOB.CREATETEMPORARY(V_BLOB, TRUE, DBMS_LOB.SESSION);
        DBMS_JAVA.EXPORT_SOURCE(REC.OBJECT_NAME, REC.OBJECT_SCHEMA, V_BLOB);
        --XX_OBJ_MGMT_PKG.BLOB_TO_FILE(V_FILE, V_BLOB);
        DBMS_LOB.FREETEMPORARY(V_BLOB);
      END LOOP;
      UTL_FILE.FCLOSE(V_FILE);
    
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_ADD_MSG     => 'OBJ:' || P_OBJ_NAME,
                    P_SQLERRM_MSG => SQLERRM);
  END GEN_JAVASRC_DDL;

  PROCEDURE GEN_CONC_IDT(P_OBJ_NAME IN VARCHAR2) IS
    V_EXIST NUMBER;
  BEGIN
    --check exist
  
    --gen ddl
    IF V_EXIST > 0 THEN
      NULL;
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_ADD_MSG     => 'OBJ:' || P_OBJ_NAME,
                    P_SQLERRM_MSG => SQLERRM);
  END GEN_CONC_IDT;

  PROCEDURE GEN_FORM_IDT(P_OBJ_NAME IN VARCHAR2) IS
    V_EXIST NUMBER;
  BEGIN
    --check exist
  
    --gen ddl
    IF V_EXIST > 0 THEN
      NULL;
    ELSE
      LOG(C_WARNING, 'Object Not Exists, obj: ' || P_OBJ_NAME);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      LOG_EXP_ERROR(P_PROG_UNIT   => $$PLSQL_UNIT,
                    P_EXP_LINE    => $$PLSQL_LINE,
                    P_ADD_MSG     => 'OBJ:' || P_OBJ_NAME,
                    P_SQLERRM_MSG => SQLERRM);
  END GEN_FORM_IDT;

  FUNCTION GET_SYNONYM_DDL RETURN CLOB IS
    V_RETURN CLOB;
    V_PREFIX VARCHAR2(10);
  
    CURSOR CUR_SYN_SCH IS
      SELECT DISTINCT PROGRAM_CODE, SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE OBJECT_TYPE = 'SYNONYM'
         AND XX_CUST_ID = Z_CUST_ID;
  
    CURSOR CUR_SYN_ALL(I_PROGRAM IN VARCHAR2, I_SCHEMA IN VARCHAR2) IS
      SELECT PROGRAM_CODE,
             OBJECT_NAME,
             OBJECT_NAME SYNONYM_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE INSTR(OBJECT_NAME, '@') = 0
         AND INSTR(OBJECT_NAME, '/') = 0
         AND OBJECT_TYPE = 'SYNONYM'
         AND SYNONYM_SCHEMA = I_SCHEMA
         AND PROGRAM_CODE = I_PROGRAM
         AND XX_CUST_ID = Z_CUST_ID
      UNION
      SELECT PROGRAM_CODE,
             OBJECT_NAME,
             SUBSTR(OBJECT_NAME, 1, INSTR(OBJECT_NAME, '@') - 1) SYNONYM_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE INSTR(OBJECT_NAME, '@') > 0
         AND INSTR(OBJECT_NAME, '/') = 0
         AND SYNONYM_SCHEMA = I_SCHEMA
         AND PROGRAM_CODE = I_PROGRAM
         AND XX_CUST_ID = Z_CUST_ID
      UNION
      SELECT PROGRAM_CODE,
             SUBSTR(OBJECT_NAME, INSTR(OBJECT_NAME, '/') + 1) OBJECT_NAME,
             SUBSTR(OBJECT_NAME, 1, INSTR(OBJECT_NAME, '/') - 1) SYNONYM_NAME,
             OBJECT_SCHEMA,
             SYNONYM_SCHEMA
        FROM XX_CUST_OBJS_TMP
       WHERE INSTR(OBJECT_NAME, '/') > 0
         AND SYNONYM_SCHEMA = I_SCHEMA
         AND PROGRAM_CODE = I_PROGRAM
         AND XX_CUST_ID = Z_CUST_ID;
  BEGIN
    FOR RSS IN CUR_SYN_SCH LOOP
      V_RETURN := GET_SUBTITLE('Synonym: ' || RSS.SYNONYM_SCHEMA);
      IF Z_WITH_SCHEMA THEN
        V_PREFIX := RSS.SYNONYM_SCHEMA || '.';
      ELSE
        V_PREFIX := '';
      END IF;
      --Drop 
      IF Z_DROP_SYNONYM THEN
        FOR REC IN CUR_SYN_ALL(RSS.PROGRAM_CODE, RSS.SYNONYM_SCHEMA) LOOP
          V_RETURN := V_RETURN || CHR(10) || 'DROP SYNONYM ' || V_PREFIX ||
                      REC.SYNONYM_NAME || ';';
        END LOOP;
        V_RETURN := V_RETURN || CHR(10) || Z_BLANK_LINE;
      END IF;
      --Gen
      FOR REC IN CUR_SYN_ALL(RSS.PROGRAM_CODE, RSS.SYNONYM_SCHEMA) LOOP
        V_RETURN := V_RETURN || CHR(10) || 'CREATE SYNONYM ' || V_PREFIX ||
                    REC.SYNONYM_NAME || ' FOR ' || REC.OBJECT_SCHEMA || '.' ||
                    REC.OBJECT_NAME || ';';
      END LOOP;
      V_RETURN := V_RETURN || CHR(10) || Z_BLANK_LINE;
    END LOOP;
    RETURN V_RETURN;
  END GET_SYNONYM_DDL;

  FUNCTION GET_FNDLOAD_SCPT(P_OBJ_NAME IN VARCHAR2) RETURN CLOB IS
    CURSOR CUR_FNDLOAD IS
      SELECT OBJECT_TYPE, OBJECT_NAME, OBJECT_SCHEMA APPL_NAME
        FROM XX_CUST_OBJS_TMP
       WHERE XX_CUST_ID = Z_CUST_ID
         AND XX_CUST_TYPE = Z_FNDLD_TYPE
       ORDER BY OBJECT_TYPE;
    V_RETURN     CLOB;
    V_PRESTR     VARCHAR2(300);
    V_SCRIPT_STR VARCHAR2(1000);
    V_FILE_NAME  VARCHAR2(100);
    V_PRE_TYPE   VARCHAR2(100) := '---';
  BEGIN
    V_RETURN := GET_SUBTITLE('Fndload Script: ' || P_OBJ_NAME);
    IF P_OBJ_NAME = Z_DOWNLOAD THEN
      V_PRESTR := 'FNDLOAD apps/' || Z_FNDLD_FPWD || ' 0 Y DOWNLOAD ';
    ELSE
      V_PRESTR := 'FNDLOAD apps/' || Z_FNDLD_TPWD || ' 0 Y UPLOAD ';
    END IF;
  
    FOR REC IN CUR_FNDLOAD LOOP
      V_FILE_NAME := REC.OBJECT_TYPE || '_' || REC.OBJECT_NAME ||
                     Z_EXT_LIST(C_FNDLOAD_TYPE);
      IF REC.OBJECT_TYPE = 'MENU' THEN
        IF P_OBJ_NAME = Z_DOWNLOAD THEN
          V_SCRIPT_STR := V_PRESTR || C_MENU_LCT || ' ' || V_FILE_NAME ||
                          ' MENU MENU_NAME="' || REC.OBJECT_NAME || '"';
        ELSE
          V_SCRIPT_STR := V_PRESTR || C_MENU_LCT || ' ' || V_FILE_NAME;
        END IF;
      ELSIF REC.OBJECT_TYPE = 'REQUESTGROUP' THEN
        IF P_OBJ_NAME = Z_DOWNLOAD THEN
          V_SCRIPT_STR := V_PRESTR || C_REQGRP_LCT || ' ' || V_FILE_NAME ||
                          ' REQUEST_GROUP REQUEST_GROUP_NAME="' ||
                          REC.OBJECT_NAME || '" APPLICATION_SHORT_NAME="' ||
                          REC.APPL_NAME || '"';
        ELSE
          V_SCRIPT_STR := V_PRESTR || C_REQGRP_LCT || ' ' || V_FILE_NAME;
        END IF;
      ELSIF REC.OBJECT_TYPE = 'CONCURRENT' THEN
        IF P_OBJ_NAME = Z_DOWNLOAD THEN
          V_SCRIPT_STR := V_PRESTR || C_CONC_LCT || ' ' || V_FILE_NAME ||
                          ' PROGRAM APPLICATION_SHORT_NAME="' ||
                          REC.APPL_NAME || '" CONCURRENT_PROGRAM_NAME="' ||
                          REC.OBJECT_NAME || '"';
        ELSE
          V_SCRIPT_STR := V_PRESTR || C_CONC_LCT || ' ' || V_FILE_NAME;
        END IF;
      ELSIF REC.OBJECT_TYPE = 'FUNCTION' THEN
        IF P_OBJ_NAME = Z_DOWNLOAD THEN
          V_SCRIPT_STR := V_PRESTR || C_FORM_LCT || ' ' || V_FILE_NAME ||
                          ' FUNCTION FUNCTION_NAME="' || REC.OBJECT_NAME || '"';
        ELSE
          V_SCRIPT_STR := V_PRESTR || C_FORM_LCT || ' ' || V_FILE_NAME;
        END IF;
      ELSIF REC.OBJECT_TYPE = 'LOOKUP' THEN
        IF P_OBJ_NAME = Z_DOWNLOAD THEN
          V_SCRIPT_STR := V_PRESTR || C_LOOKUP_LCT || ' ' || V_FILE_NAME ||
                          ' FND_LOOKUP_TYPE APPLICATION_SHORT_NAME="' ||
                          REC.APPL_NAME || '" LOOKUP_TYPE="' ||
                          REC.OBJECT_NAME || '"';
        ELSE
          V_SCRIPT_STR := V_PRESTR || C_LOOKUP_LCT || ' ' || V_FILE_NAME;
        END IF;
      ELSIF REC.OBJECT_TYPE = 'PROFILE' THEN
        IF P_OBJ_NAME = Z_DOWNLOAD THEN
          V_SCRIPT_STR := V_PRESTR || C_PROFILE_LCT || ' ' || V_FILE_NAME ||
                          ' PROFILE PROFILE_NAME="' || REC.OBJECT_NAME ||
                          '" APPLICATION_SHORT_NAME="' || REC.APPL_NAME || '"';
        ELSE
          V_SCRIPT_STR := V_PRESTR || C_PROFILE_LCT || ' ' || V_FILE_NAME;
        END IF;
      ELSIF REC.OBJECT_TYPE = 'PERSONAL' THEN
        IF P_OBJ_NAME = Z_DOWNLOAD THEN
          V_SCRIPT_STR := V_PRESTR || C_FRMCUS_LCT || ' ' || V_FILE_NAME ||
                          ' FND_FORM_CUSTOM_RULES FUNCTION_NAME="' ||
                          REC.OBJECT_NAME || '"';
        ELSE
          V_SCRIPT_STR := V_PRESTR || C_FRMCUS_LCT || ' ' || V_FILE_NAME;
        END IF;
      ELSIF REC.OBJECT_TYPE = 'XMLP' THEN
        IF P_OBJ_NAME = Z_DOWNLOAD THEN
          V_SCRIPT_STR := V_PRESTR || C_XMLP_LCT || ' ' || V_FILE_NAME ||
                          ' XDO_DS_DEFINITIONS APPLICATION_SHORT_NAME="' ||
                          REC.APPL_NAME || '" DATA_SOURCE_CODE="' ||
                          REC.OBJECT_NAME || '"';
        ELSE
          V_SCRIPT_STR := V_PRESTR || C_XMLP_LCT || ' ' || V_FILE_NAME;
        END IF;
      END IF;
      IF V_PRE_TYPE <> REC.OBJECT_TYPE THEN
        V_RETURN   := V_RETURN || CHR(10) || CHR(10) || V_SCRIPT_STR;
        V_PRE_TYPE := REC.OBJECT_TYPE;
      ELSE
        V_RETURN := V_RETURN || CHR(10) || V_SCRIPT_STR;
      END IF;
    END LOOP;
    RETURN V_RETURN;
  END GET_FNDLOAD_SCPT;

  /**
  * get the expression of functional index
  * @param P_COL_NAME VARCHAR2 given virtual index column
  * @param P_OWNER VARCHAR2 the owner of index table
  * @param P_TABLE VARCHAR2 index table
  * @return VARCHAR2 the expression of functional index
  */
  FUNCTION GET_FIDX_COL(P_COL_NAME IN VARCHAR2,
                        P_OWNER    IN VARCHAR2,
                        P_TABLE    IN VARCHAR2) RETURN VARCHAR2 IS
    V_RETURN VARCHAR2(300);
  BEGIN
    SELECT C.DEFAULT$
      INTO V_RETURN
      FROM SYS.COL$ C, SYS.OBJ$ O, SYS.USER$ U
     WHERE C.OBJ# = O.OBJ#
       AND O.NAME = P_TABLE
       AND O.OWNER# = U.USER#
       AND U.NAME = P_OWNER
       AND C.NAME = P_COL_NAME;
  
    SELECT REPLACE(V_RETURN, '"', '') INTO V_RETURN FROM DUAL;
  
    RETURN V_RETURN;
  END GET_FIDX_COL;

  /**
  * record the ddl scripts list with owner,program code and file name info
  * @param P_OWNER VARCHAR2 object owner
  * @param P_PROGRAM_CODE VARCHAR2 program code info
  * @param P_FILE_NAME VARCHAR2 View ddl script file name
  */
  PROCEDURE RECORD_DDL_LIST(P_OWNER        IN VARCHAR2,
                            P_PROGRAM_CODE IN VARCHAR2,
                            P_FILE_NAME    IN VARCHAR2,
                            P_OBJ_TYPE     IN VARCHAR2,
                            P_OBJ_NAME     IN VARCHAR2) IS
  BEGIN
    INSERT INTO XX_DDL_SCRIPT_LIST_TMP
      (XX_CUST_ID,
       OWNER,
       PROGRAM_CODE,
       SCRIPT_FILE_NAME,
       OBJECT_TYPE,
       OBJECT_NAME)
    VALUES
      (Z_CUST_ID,
       P_OWNER, --OWNER
       P_PROGRAM_CODE, --PROGRAM_CODE
       '\' || P_FILE_NAME, --SCRIPT_FILE_NAME
       P_OBJ_TYPE, --OBJECT_TYPE
       P_OBJ_NAME --OBJECT_NAME
       );
  END RECORD_DDL_LIST;

  PROCEDURE APPS_INIT(P_USER_ID IN NUMBER) IS
  BEGIN
    FND_GLOBAL.APPS_INITIALIZE(USER_ID      => P_USER_ID,
                               RESP_ID      => 20420, --System Adminstator
                               RESP_APPL_ID => 1);
  END APPS_INIT;

  PROCEDURE CALL_FNDLOAD(P_LCT_SRC  IN VARCHAR2,
                         P_LDT_DEST IN VARCHAR2,
                         P_TYPE_ARG IN VARCHAR2,
                         P_APPL_ARG IN VARCHAR2,
                         P_OBJ_ARG  IN VARCHAR2) IS
    V_REQ_ID NUMBER;
  BEGIN
    V_REQ_ID := FND_REQUEST.SUBMIT_REQUEST(APPLICATION => 'FND',
                                           PROGRAM     => 'FNDLOAD',
                                           DESCRIPTION => NULL,
                                           START_TIME  => NULL,
                                           SUB_REQUEST => FALSE,
                                           ARGUMENT1   => 'DOWNLOAD',
                                           ARGUMENT2   => REPLACE(REPLACE(P_LCT_SRC,
                                                                          '$FND_TOP/',
                                                                          '@fnd:'),
                                                                  '$XDO_TOP/',
                                                                  '@xdo:'),
                                           ARGUMENT3   => P_LDT_DEST,
                                           ARGUMENT4   => P_TYPE_ARG,
                                           ARGUMENT5   => P_APPL_ARG,
                                           ARGUMENT6   => P_OBJ_ARG);
  
    COMMIT;
  END CALL_FNDLOAD;

  FUNCTION GET_PKG_CLOB RETURN CLOB IS
    V_RETURN CLOB;
  BEGIN
    V_RETURN := DBMS_METADATA.GET_DDL('PACKAGE', 'XXARF0504_PKG', 'APPS');
    RETURN V_RETURN;
  END GET_PKG_CLOB;

  PROCEDURE PURGE_TEMP_TABLES IS
  BEGIN
    DELETE FROM XX_CUST_OBJ_PARAM_TMP WHERE 1 = 1;
    DELETE FROM XX_CUST_OBJS_TMP WHERE 1 = 1;
    DELETE FROM XX_SCHEMA_INFO_TMP WHERE 1 = 1;
    DELETE FROM XX_DDL_SCRIPT_LIST_TMP WHERE 1 = 1;
    DELETE FROM XX_FNDLOAD_REQUEST_TMP WHERE 1 = 1;
  END PURGE_TEMP_TABLES;

END XX_OBJ_PY_PKG;
/
